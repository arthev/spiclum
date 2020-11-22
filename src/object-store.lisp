(in-package :spiclum)

;;;; 0. Basic definitions

;; The "canonical" basic lookup is on the unique IDs, so those
;; are also the lookups we can iterate to persist the whole store.

(defclass prevalence-system ()
  ((uuid-seed
    :initform 0
    :accessor uuid-seed)
   (uuid-seed-lock
    :initform (bt:make-lock "uuid-seed-lock")
    :reader uuid-seed-lock)
   (class-definition-store
    :initform (make-hash-table)
    :reader class-definition-store)
   (hash-store
    :initform (make-hash-table)
    :reader hash-store)
   (lock-store
    :initform (make-hash-table)
    :reader lock-store)
   (lock-store-lock
    :initform (bt:make-lock "lock-store-lock")
    :reader lock-store-lock)
   (storage-path
    :initform (error "Prevalence-system must be supplied a STORAGE-PATH")
    :initarg :storage-path
    :accessor storage-path)
   (storage-timestamp
    :initform nil
    :initarg :storage-timestamp
    :accessor storage-timestamp)
   (world-file
    :accessor world-file)
   (log-file
    :accessor log-file))
  (:documentation
   "HASH-STORE is a hash by persistent class with hash by slot as value."))

(defparameter *prevalencing-p* t
  "Toggle for whether to update the object store")

(defvar *prevalence-system*
  (make-instance 'prevalence-system
                 :storage-path (make-pathname :directory "home/arthur/spiclum-test"
                                              :name "spiclum-test")))

(defvar *world-filename* "spclmwrld"
  "file ending for world files")

(defvar *log-filename* "spclmlg"
  "file ending for log files")


(defun generate-uuid-for-object-store ()
  (bt:with-recursive-lock-held ((uuid-seed-lock *prevalence-system*))
    (incf (uuid-seed *prevalence-system*))))

(defmethod initialize-instance :after ((instance prevalence-system)
                                       &key storage-path storage-timestamp &allow-other-keys)
  (ensure-directories-exist storage-path)
  (flet ((pathname-for-timestamp-and-type (timestamp type)
           (cl-fad:merge-pathnames-as-file
            storage-path
            (make-pathname :name (format nil "~A-~A"
                                         (pathname-name storage-path)
                                         timestamp)
                           :type type))))
    (if storage-timestamp
        (let ((path (pathname-for-timestamp-and-type storage-timestamp *world-filename*)))
          (unless (cl-fad:file-exists-p path)
            (error "Can't find a file ~S as required to use STORAGE-TIMESTAMP ~s"
                   path storage-timestamp)))
        (let* ((worlds (remove-if (complement (lfix #'string= *world-filename*))
                                  (cl-fad:list-directory (cl-fad:pathname-directory-pathname storage-path))
                                  :key #'pathname-type))
               (most-recent-world (car (sort worlds #'string-greaterp :key #'pathname-name)))
               (storage-name (pathname-name most-recent-world)))
          (setf (storage-timestamp instance)
                (subseq storage-name (1+ (length (pathname-name storage-path)))))))
    (setf (world-file instance)
          (pathname-for-timestamp-and-type (storage-timestamp instance) *world-filename*))
    (setf (log-file instance)
          (pathname-for-timestamp-and-type (storage-timestamp instance) *log-filename*))))

;;;; 1. Class definition access

(defun register-last-class-definition (metaclass class name &rest args)
  (setf (gethash name (class-definition-store *prevalence-system*))
        (list* metaclass class name args)))

(defun last-class-definition (supplied-class supplied-name)
  "Returns the previous class definition supplied to ensure-class-using-metaclass.

Returns SUPPLIED-CLASS in place of the CLASS argument registered during the previous
call, since CLASS might have been NIL during the previous call - and we'd get wrong
behaviour if we supplied NIL once the class has been defined. If CLASS isn't NIL,
then SUPPLIED-CLASS and CLASS should be EQ."
  (destructuring-bind (metaclass class name &rest args)
      (gethash supplied-name (class-definition-store *prevalence-system*))
    (declare (ignore class))
    (list* metaclass supplied-class name args)))

;;;; 2. Instance Lookup

(defun prevalence-lookup-class-slot (class slotd value)
  "Looks up relevant entries for CLASS, SLOTD, VALUE.

The keys in the PREVALENCE-SYSTEM HASH-STORE are the names of the CLASS and SLOTD.
This is a low-level utility for use by other parts of the prevalence-system."
  (handler-bind ((type-error
                   (lambda (condition)
                     (when (and (eq nil (type-error-datum condition))
                                (eq 'hash-table (type-error-expected-type condition)))
                       (return-from prevalence-lookup-class-slot nil)))))
    (gethash value
             (gethash (c2mop:slot-definition-name slotd)
                      (gethash (class-name class)
                               (hash-store *prevalence-system*))))))

(defun (setf prevalence-lookup-class-slot) (new-value class slotd value)
  "Sets the appropriate nested hash-lookup value, creates new tables as necessary."
  (let* ((class-table (orf (gethash (class-name class)
                                    (hash-store *prevalence-system*))
                           (make-hash-table)))
         (slot-table (orf (gethash (c2mop:slot-definition-name slotd) class-table)
                          (make-hash-table :test (equality slotd)))))
    ;; Can't we access e.g. indexes with nil as value, then?
    ;; TEMPER - if there's no new value and we want to cease having an item available on the index,
    ;; maybe that's for slot-makunbound?
    (if (null new-value)
        (remhash value slot-table)
        (setf (gethash value slot-table) new-value))))

(defun prevalence-slot-available-p (class slotd value)
  "Checks whether the appropriate key is available."
  (ccase (key slotd)
    ((:index nil) t)
    (:class-unique
     (not (prevalence-lookup-class-slot class slotd value)))
    (:precedence-unique
     (not (prevalence-lookup-class-slot
           (find-slot-defining-class class slotd) slotd value)))))

(defun prevalence-instance-slots-available-p
    (instance &key (slots (c2mop:class-slots (class-of instance))))
  "Checks whether SLOTS are all available as keys for INSTANCE."
  (let ((class (class-of instance))
        problem-slots problem-values)
    (dolist (slotd slots)
      (let* ((slot-name (c2mop:slot-definition-name slotd))
             (slot-value (ignore-errors (slot-value instance slot-name))))
        (when (slot-boundp instance slot-name)
          (unless (prevalence-slot-available-p class slotd slot-value)
            (push slotd problem-slots)
            (push slot-value problem-values)))))
    (values (not problem-slots) problem-slots problem-values)))

;;;; 3. Instance Insertion

(defun prevalence-insert-class-slot (class slotd value object)
  "Inserts OBJECT into the HASH-STORE using appropriate strategy for SLOTD."
  (unless *prevalencing-p* (return-from prevalence-insert-class-slot :do-nothing))
  (flet ((unique-insert (using-class)
           (if (prevalence-lookup-class-slot using-class slotd value)
               (error 'non-unique-unique-keys
                      :breach-class using-class :breach-slots slotd :breach-values value)
               (setf (prevalence-lookup-class-slot using-class slotd value) object))))
    (ccase (key slotd)
      (:class-unique (unique-insert class))
      (:precedence-unique (unique-insert (find-slot-defining-class class slotd)))
      (:index
       (let ((slot-defining-class (find-slot-defining-class class slotd)))
         (pushnew object (prevalence-lookup-class-slot slot-defining-class slotd value))))
      ((nil) :do-nothing))))

(defun prevalence-insert-instance
    (instance &key (slots (c2mop:class-slots (class-of instance))))
  (let ((class (class-of instance)))
    (multiple-value-bind (available-p problem-slots problem-values)
        (prevalence-instance-slots-available-p instance :slots slots)
      (unless available-p
        (error 'non-unique-unique-keys :breach-class class
                                       :breach-slots problem-slots
                                       :breach-values problem-values)))
    (dolist (slotd slots)
      (when (slot-boundp instance (c2mop:slot-definition-name slotd))
        (prevalence-insert-class-slot
         class
         slotd
         (slot-value instance (c2mop:slot-definition-name slotd))
         instance)))))

(defun prevalence-insert-instances (instances)
  (let (finished current)
    (unwind-protect
         (dolist (instance instances)
           (setf current instance)
           (prevalence-insert-instance instance)
           (push instance finished))
      (unless (= (length instances) (length finished))
        (prevalence-remove-instances finished)
        (handler-case
            (prevalence-remove-instance current)
          (removing-nonexistant-entry ())))))) ; Gobble

;;;; 4. Instance Removal

(defun prevalence-remove-class-slot (class slotd value object)
  (unless *prevalencing-p* (return-from prevalence-remove-class-slot :do-nothing))
  (flet ((unique-removal (using-class)
           (if (eq (prevalence-lookup-class-slot using-class slotd value)
                   object)
               (setf (prevalence-lookup-class-slot using-class slotd value) nil)
               (error 'removing-nonexistant-entry
                      :breach-class using-class :breach-slots slotd
                      :breach-values value       :breach-object object))))
    (ccase (key slotd)
      ((nil) nil)
      (:class-unique
       (unique-removal class))
      (:precedence-unique
       (unique-removal (find-slot-defining-class class slotd)))
      (:index
       (let ((slot-defining-class (find-slot-defining-class class slotd)))
         (if (find object (prevalence-lookup-class-slot slot-defining-class slotd value))
             (setf (prevalence-lookup-class-slot slot-defining-class slotd value)
                   (remove object (prevalence-lookup-class-slot slot-defining-class slotd value)))
             (error 'removing-nonexistant-entry
                    :breach-class slot-defining-class :breach-slots slotd
                    :breach-values value               :breach-object object)))))))

(defun prevalence-remove-instance
    (instance &key
                (slots (c2mop:class-slots (class-of instance)))
                (values (slotds->values-map instance)))
  (let ((class (class-of instance)))
    (dolist (slotd slots)
      (when (key slotd)
        (multiple-value-bind (value present-p)
            (gethash slotd values)
          (when present-p
            (prevalence-remove-class-slot
             class slotd value instance)))))))

(defun prevalence-remove-instances (instances)
  (let (finished current)
    (unwind-protect
         (dolist (instance instances)
           (setf current instance)
           (prevalence-remove-instance instance)
           (push instance finished))
      (unless (= (length instances) (length finished))
        (prevalence-insert-instances finished)
        (handler-case
            (prevalence-insert-instance current)
          (non-unique-unique-keys ())))))) ; Gobble

;;;; 5. Locks

(defun prevalence-slot-locks (class slotds)
  "Returns a list of locks associated with CLASS and SLOTDS."
  (flet ((lock-for-slot-defining-class (slotd)
           (let ((slot-defining-class (find-slot-defining-class class slotd)))
             (assert slot-defining-class)
             (prevalence-lookup-lock (class-name slot-defining-class)
                                     (c2mop:slot-definition-name slotd)))))
    (remove nil
            (mapcar (lambda (slotd)
                      (ccase (key slotd)
                        (:class-unique (prevalence-lookup-lock
                                        (class-name class)
                                        (c2mop:slot-definition-name slotd)))
                        (:index (lock-for-slot-defining-class slotd))
                        (:precedence-unique (lock-for-slot-defining-class slotd))
                        ((nil) nil)))
                    slotds))))

(defun all-prevalence-slot-locks-for (obj)
  (let ((class (if (c2mop:classp obj) obj (class-of obj))))
    (unless (c2mop:class-finalized-p class)
      (c2mop:finalize-inheritance class))
    (prevalence-slot-locks class (c2mop:class-slots class))))

(defun prevalence-lookup-lock (class-name slot-name)
  "Looks up associated lock in *PREVALENCE-SYSTEM*. Creates as necessary."
  (let* ((class-table
           (orf (gethash class-name (lock-store *prevalence-system*))
                (bt:with-lock-held ((lock-store-lock *prevalence-system*))
                  (lif (table (gethash class-name (lock-store *prevalence-system*)))
                       table
                       (make-hash-table)))))
         (lock
           (orf (gethash slot-name class-table)
                (bt:with-lock-held ((lock-store-lock *prevalence-system*))
                  (lif (lock (gethash slot-name class-table))
                       lock
                       (bt:make-lock (format nil "~A lock: ~A ~A"
                                             *prevalence-system* class-name slot-name)))))))
    lock))
