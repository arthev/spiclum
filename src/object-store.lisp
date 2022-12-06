(in-package :spiclum)

;;;; -1. pathname helpers

(defun timestamped-storage-pathname (storage-path timestamp type)
  (uiop/pathname:merge-pathnames*
   (make-pathname :name (format nil "~A-~A"
                                (pathname-name storage-path)
                                timestamp)
                  :type type)
   storage-path))

(defun most-recent-timestamped-file (directory-pathname type)
  "Returns the most recent file, as per ansi timestamp."
  (car (sort (list-directory-for-type directory-pathname type)
             #'string-greaterp :key #'pathname-name)))

(defun compute-timestamp (storage-path storage-timestamp)
  (if storage-timestamp
      (progn (assert (uiop/filesystem:file-exists-p
                      (timestamped-storage-pathname
                       storage-path storage-timestamp *world-filename*)))
             storage-timestamp)
      (lif (most-recent-world
            (most-recent-timestamped-file
             (uiop/pathname:pathname-directory-pathname storage-path)
             *world-filename*))
           (subseq (pathname-name most-recent-world)
                   (1+ (length (pathname-name storage-path))))
           (ANSI-time))))

(defun update-prevalence-system-for-timestamp (instance timestamp)
  (with-accessors ((storage-path storage-path)) instance
    (let ((file-timestamp-string (if (windows-p)
                                     (remove #\: timestamp)
                                     timestamp)))
      (setf (storage-timestamp instance)
            timestamp
            (world-file instance)
            (timestamped-storage-pathname
             storage-path
             file-timestamp-string
             *world-filename*)
            (log-file instance)
            (timestamped-storage-pathname
             storage-path
             file-timestamp-string
             *log-filename*)))))

(defun initialize-prevalence-system-files (instance)
  ;; Log initialization time as comment for debug convenience
  (with-open-file (out (log-file instance)
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format out "~%;; PREVALENCE-SYSTEM targetting this log initialized at ~A~%"
            (ANSI-time)))
  ;; Create an empty world file if none exists
  (with-open-file (out (world-file instance)
                       :direction :output
                       :if-exists nil
                       :if-does-not-exist :create)))

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

(defmethod initialize-instance :after ((instance prevalence-system)
                                       &key storage-path storage-timestamp &allow-other-keys)
  (ensure-directories-exist storage-path)
  (let ((timestamp (compute-timestamp storage-path storage-timestamp)))
    (update-prevalence-system-for-timestamp instance timestamp))
  (initialize-prevalence-system-files instance))

(defvar *world-filename* "world"
  "file ending for world files")

(defvar *log-filename* "log"
  "file ending for log files")

(defparameter *prevalencing-p* t
  "Toggle for whether to update the object store")

(defvar *prevalence-system* nil)

(defun generate-uuid-for-object-store ()
  (bt:with-recursive-lock-held ((uuid-seed-lock *prevalence-system*))
    (incf (uuid-seed *prevalence-system*))))

(defun reset-uuid-seed-for-object-store ()
  (bt:with-recursive-lock-held ((uuid-seed-lock *prevalence-system*))
    (let ((slot-table (prevalence-lookup-store 'prevalence-object 'uuid)))
      (setf (uuid-seed *prevalence-system*)
            (loop for uuid being the hash-keys of slot-table
                  maximize uuid)))))

;;;; 1. Class definition access

(defun register-last-class-definition (name args)
  (setf (gethash name (class-definition-store *prevalence-system*))
        (list* name args)))

(defun last-class-definition (name)
  "Return previous class definition used in ensure-class-using-metaclass."
  (gethash name (class-definition-store *prevalence-system*)))

;;;; 2. Instance Lookup

(defun prevalence-lookup-store (class-name slot-name)
  "Lookup/create table mapping values for SLOT-NAME to prevalence-objects."
  (let* ((slotd (slot-by-name (find-class class-name) slot-name))
         (class-table (orf (gethash class-name
                                    (hash-store *prevalence-system*))
                           (make-hash-table))))
    (orf (gethash slot-name class-table)
         (make-hash-table :test (equality slotd)))))

(defun prevalence-lookup-class-slot (class slotd value)
  (gethash value
           (prevalence-lookup-store (class-name class)
                                    (c2mop:slot-definition-name slotd))))

(defun (setf prevalence-lookup-class-slot) (new-value class slotd value)
  "Sets the appropriate nested hash-lookup value."
  (let ((slot-table
          (prevalence-lookup-store (class-name class)
                                   (c2mop:slot-definition-name slotd))))
    (setf (gethash value slot-table) new-value)))

(defun clear-prevalence-lookup-class-slot (class slotd value)
  (let ((slot-table
          (prevalence-lookup-store (class-name class)
                                   (c2mop:slot-definition-name slotd))))
    (remhash value slot-table)))

(defun prevalence-slot-available-p (class slotd value)
  "Checks whether the appropriate key is available."
  (ccase (key slotd)
    ((:index nil) t)
    (:class-unique
     (not (prevalence-lookup-class-slot class slotd value)))
    (:unique
     (not (prevalence-lookup-class-slot
           (find-slot-defining-class class slotd) slotd value)))))

(defun prevalence-instance-slots-available-p
    (instance &key (slots (c2mop:class-slots (class-of instance))))
  "Checks whether SLOTS are all available as keys for INSTANCE."
  (let ((class (class-of instance))
        problem-slots problem-values)
    (do-bound-slots (slotd instance :slots slots :value slot-value)
      (unless (prevalence-slot-available-p class slotd slot-value)
        (push slotd problem-slots)
        (push slot-value problem-values)))
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
      (:unique (unique-insert (find-slot-defining-class class slotd)))
      (:index
       (let ((slot-defining-class (find-slot-defining-class class slotd)))
         (pushnew object (prevalence-lookup-class-slot slot-defining-class slotd value))))
      ((nil) :do-nothing))))

(defun prevalence-insert-instance
    (instance &key (slots (c2mop:class-slots (class-of instance))))
  (let ((class (class-of instance)))
    (multiple-value-bind (available-p problem-slots problem-values)
        (prevalence-instance-slots-available-p instance :slots slots)
      (if available-p
          (do-bound-slots (slotd instance :slots slots :value slot-value)
            (prevalence-insert-class-slot class slotd slot-value instance))
          (error 'non-unique-unique-keys :breach-class class
                                         :breach-slots problem-slots
                                         :breach-values problem-values)))))

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
               (clear-prevalence-lookup-class-slot using-class slotd value)
               (error 'removing-nonexistant-entry
                      :breach-class using-class :breach-slots slotd
                      :breach-values value       :breach-object object))))
    (ccase (key slotd)
      ((nil) :do-nothing)
      (:class-unique
       (unique-removal class))
      (:unique
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
                (values (slot->value-map instance)))
  (let ((class (class-of instance)))
    (dolist (slotd slots)
      (when (key slotd)
        (multiple-value-bind (value present-p)
            (gethash (c2mop:slot-definition-name slotd) values)
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
    (loop for slotd in slotds
          when (key slotd)
            collect (ccase (key slotd)
                      (:class-unique (prevalence-lookup-lock
                                      (class-name class)
                                      (c2mop:slot-definition-name slotd)))
                      (:index (lock-for-slot-defining-class slotd))
                      (:unique (lock-for-slot-defining-class slotd))))))

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
