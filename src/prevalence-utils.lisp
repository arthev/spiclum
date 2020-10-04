(in-package :spiclum)

(defclass prevalence-class (standard-class)
  ()
  (:documentation "Meta-class for persistent objects using prevalence."))

(defclass keyable-slot (c2mop:standard-slot-definition)
  ((key
    :initarg :key
    :accessor key
    :initform nil
    :documentation
    "Subslot to specify whether a slot is a key. See KEYABLE-SLOT-KEY.")
   (equality
    :initarg :equality
    :accessor equality
    :initform #'eql
    :documentation
    "Subslot to specify how to compare valus of the slot for equality.
    Used by the prevalence system's hash-tables.")))

(defparameter *persisting-p* t
  "Toggle for whether to persist (aka serialize and write) data or not.")

(defparameter *prevalencing-p* t
  "Toggle for whether to update the prevalence object store")

;;;; -2. Malplaced Helpers

(defun lock-name (lock)
  (sb-thread:mutex-name lock))

(defmacro with-recursive-locks (locks &body body)
  "Non-portable: bordeaux-threads doesn't support lock-name lookup."
  `(call-with-recursive-locks
    (sort ,locks #'string< :key #'lock-name)
    (lambda () ,@body)))

(defun call-with-recursive-locks (locks body)
  "Probably inefficient compared with acquire-recursive-lock.
   But bordeaux-threads don't support that for sbcl."
  (labels ((internal (locks)
             (if (endp locks)
                 (funcall body)
                 (bt:with-recursive-lock-held ((car locks))
                   (internal (cdr locks))))))
    (internal locks)))

;;;; -1. Helpers

(defun metaclass-of (obj)
  "Returns the metaclass of OBJ as primary value.
   Returns a secondary value indicating whether obj
   was a class-object (T) or not (nil).
   Probably assumes no metaclasses have metaclasses."
  (if (c2mop:classp obj)
      (values (class-of obj) t)
      (values (class-of (class-of obj)) nil)))

(defun slotds->values-map (instance)
  "Returns a hash-table mapping slotds to values. Unbound slots aren't keyed."
  (let ((hash-table (make-hash-table)))
    (dolist (slotd (c2mop:class-slots (class-of instance)))
      (let ((slot-name (c2mop:slot-definition-name slotd)))
        (when (slot-boundp instance slot-name)
          (setf (gethash slotd hash-table)
                (slot-value instance slot-name)))))
    hash-table))

(defmacro with-ignored-prevalence (&rest body)
  `(let ((*persisting-p* nil)
         (*prevalencing-p* nil))
     ,@body))

;;;; 0. KEYABLE-SLOT section

(defun %member-of-legal-keyable-slot-key-values (x)
  (member x '(nil :precedence-unique :class-unique :index)))

(deftype keyable-slot-key ()
  '(satisfies %member-of-legal-keyable-slot-key-values))

(defclass keyable-direct-slot (keyable-slot
                               c2mop:standard-direct-slot-definition)
  ())

(defclass keyable-effective-slot (keyable-slot
                                  c2mop:standard-effective-slot-definition)
  ())

(defmethod initialize-instance :before ((slot keyable-slot) &key key &allow-other-keys)
  "The MOP standard precludes portable programs from writing methods on
   SHARED-INITIALIZE, thus the duplication with REINITIALIZE-INSTANCE."
  (check-type key keyable-slot-key))

(defmethod reinitialize-instance :before ((slot keyable-slot) &key key &allow-other-keys)
  "The MOP standard precludes portable programs from writing methods on
   SHARED-INITIALIZE, thus the duplication with INITIALIZE-INSTANCE."
  (check-type key keyable-slot-key))

(defmethod c2mop:direct-slot-definition-class ((class prevalence-class)
                                               &rest initargs)
  "Efficiency here could possibly be increased by checking whether
   a normal slot would suffice, by analyzing INITARGS. Complicates
   both this method and COMPUTE-EFFECTIVE-SLOT-DEFINITION, however."
  (declare (ignore initargs))
  (find-class 'keyable-direct-slot))

(defmethod c2mop:effective-slot-definition-class ((class prevalence-class)
                                                  &rest initargs)
  "Efficiency here could possibly be increased by checking whether
   a normal slot would suffice, by analyzing INITARGS. Complicates
   both this method and COMPUTE-EFFECTIVE-SLOT-DEFINITION, however."
  (declare (ignore initargs))
  (find-class 'keyable-effective-slot))

(defmethod c2mop:compute-effective-slot-definition ((class prevalence-class)
                                                    slot-name
                                                    direct-slot-definitions)
  "As for STANDARD-CLASS, except updates the KEY slot
   of the EFFECTIVE-SLOT-DEFINITION."
  (let* ((normal-slot (call-next-method))
         (equality-arg (equality (car direct-slot-definitions)))
         (equality-fn (cond ((functionp equality-arg)
                             equality-arg)
                            ;; Presumably '#'eqtype
                            ((listp equality-arg)
                             (symbol-function (cadr equality-arg)))
                            (t (error "Can't recognise equality-arg: ~S"
                                      equality-arg)))))
    (setf (key normal-slot) (key (car direct-slot-definitions))
          (equality normal-slot) equality-fn)
    normal-slot))

(defun class-x-key-slots (instance-or-class key-type)
  "Returns a list of slot objects with KEY-TYPE keys.
   CONSIDER: Should classes be finalized if not finalized?"
  (let ((class (if (c2mop:classp instance-or-class)
                   instance-or-class
                   (class-of instance-or-class))))
    (remove-if-not (lambda (slot)
                     (and (typep slot 'keyable-slot)
                          (eq (key slot) key-type)))
                   (c2mop:class-slots class))))

(defun class-unique-key-slots (instance-or-class)
  "Returns a list of slot objects with :class-unique keys."
  (class-x-key-slots instance-or-class :class-unique))

(defun index-key-slots (instance-or-class)
  "Returns a list of slot objects with :index keys."
  (class-x-key-slots instance-or-class :index))

(defun precedence-unique-key-slots (instance-or-class)
  "Returns a list of slot objects with :precedence-unique keys"
  (class-x-key-slots instance-or-class :precedence-unique))

(defun nil-key-slots (instance-or-class)
  "Returns a list of slot objects with nil keys"
  (class-x-key-slots instance-or-class nil))

(define-condition prevalence-breach (error)
  ((breach-values
    :initarg :breach-values
    :accessor breach-values)
   (breach-slots
    :initarg :breach-slots
    :accessor breach-slots)
   (breach-class
    :initarg :breach-class
    :accessor breach-class))
  (:report
   (lambda (condition stream)
     (format stream "Breach for class ~S, slot(s) ~S, value(s) ~S"
             (breach-class condition)
             (breach-slots condition)
             (breach-values condition))))
  (:documentation
   "Ancestor error for different breaches of prevalence constraints."))

(define-condition non-unique-unique-keys (prevalence-breach)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Uniqueness broken: class ~S, slot(s) ~S, value(s) ~S"
             (breach-class condition)
             (breach-slots condition)
             (breach-values condition))))
  (:documentation
   "For breaches of the uniqueness constraints on keyable-slots."))

(define-condition removing-nonexistant-entry (prevalence-breach)
  ((breach-object
   :initarg :breach-object
   :accessor breach-object))
  (:report
   (lambda (condition stream)
     (format stream "Tried to remove object ~S, for class ~S, slot(s) ~S, value(s) ~S,~% ~
                     but couldn't find an entry"
             (breach-object condition) (breach-class condition)
             (breach-slots   condition) (breach-values condition))))
  (:documentation
   "For when trying to remove objects from mismatched entries."))


;;;; 1. PREVALENCE-CLASS section

(defmethod c2mop:validate-superclass ((class prevalence-class)
                                      (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)

;; NOTE: If we want to allow mixins, we might want the reverse
;; VALIDATE-SUPERCLASS method too. Probably on a 'caveat emptor' basis.
;; Might be rather odd behaviour, particularly if we have different metaclasses
;; on the mixins. For simplicity, designing as if disallowed.

(defun acceptable-persistent-slot-value-type-p (new-value)
  (%acceptable-persistent-slot-value-type-p (metaclass-of new-value) new-value))

(defgeneric %acceptable-persistent-slot-value-type-p (metaclass new-value)
  (:documentation "Predicate to determine whether NEW-VALUE can be stored
   as a persistent slot value. Exported for further user specification.
   Of course, if something can be stored, it must be (de)serializable as well."))

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'standard-class))) new-value)
  (error "Can't use ~S as slot-value for persistent object,~%~
          since it's metaclass ~S"
         new-value metaclass))

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'structure-class))) new-value)
  (error "Can't use ~S as slot-value for persistent object,~%~
          since it's metaclass ~S"
         new-value metaclass))

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'built-in-class))) new-value)
  t)

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'prevalence-class))) new-value)
  t)

(defun guarded-slot-value (instance slot-name)
  (if (slot-boundp instance slot-name)
      (values (slot-value instance slot-name) t)
      (values nil nil)))

(defmethod (setf c2mop:slot-value-using-class) (new-value
                                                (class prevalence-class)
                                                instance
                                                slotd)
  "SETFs the NEW-VALUE of INSTANCE for SLOTD, as a transaction.

Must atomatically update indexes and persist as appropriate."
  (assert (acceptable-persistent-slot-value-type-p new-value))
  (multiple-value-bind (old-value slot-boundp)
      (guarded-slot-value instance (c2mop:slot-definition-name slotd))
    (let (;; CLHS specifies that SETF may return multiple values
          (results (multiple-value-list (call-next-method)))
          (same-index-p (and slot-boundp
                             (funcall (equality slotd) new-value old-value)))
          prevalenced-p removed-p success-p)
      (with-recursive-locks (prevalence-slot-locks class (list slotd))
        (unwind-protect
             (progn
               (unless same-index-p
                 (prevalence-insert-class-slot class slotd new-value instance)
                 (setf prevalenced-p t)
                 (when slot-boundp
                   (prevalence-remove-class-slot class slotd old-value instance)
                   (setf removed-p t)))
               :persist ;TEMPER
               (setf success-p t)
               (values-list results))
          (unless success-p
            (when prevalenced-p
              (prevalence-remove-class-slot class slotd new-value instance))
            (if slot-boundp
                (call-next-method old-value class instance slotd)
                (slot-makunbound instance (c2mop:slot-definition-name slotd)))
            (when removed-p
              (prevalence-insert-class-slot class slotd old-value instance))))))))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  "Makes the instance and inserts into the prevalence system as a transaction.

If the transaction fails, remove it and return an error.
Thus, zero references to the object."
  (declare (ignorable class initargs))
  (let ((instance (with-ignored-prevalence (call-next-method)))
        success-p)
    (with-recursive-locks (all-prevalence-slot-locks-for instance)
      (unwind-protect
           (multiple-value-bind (available-p problem-slots problem-values)
               (prevalence-instance-slots-available-p instance)
             (if available-p
                 (progn (prevalence-insert-instance instance)
                        :persist ; TEMPER
                        (setf success-p t)
                        instance)
                 (error 'non-unique-unique-keys :breach-class class
                                                :breach-slots problem-slots
                                                :breach-values problem-values)))
        (unless success-p
          (prevalence-remove-instance instance))))))

;;;; 2. Prevalence-object section

(defclass prevalence-object ()
  ()
  (:metaclass prevalence-class)
  (:documentation "Class for prevalent objects to inherit, to specialize on
                   reinitialize-instance etc."))

(defmacro defpclass (class-name superclasses slot-specifiers &rest class-options)
  "Programmer macro for defining prevalence classes."
  (when (find :metaclass class-options :key #'car)
    (error "DEFPCLASS uses implicit metaclass PREVALENCE-CLASS,~%~
            but an explicit metaclass was provided for ~S" class-name))
  `(defclass ,class-name (,@superclasses prevalence-object)
     ,slot-specifiers
     ,@class-options
     (:metaclass prevalence-class)))

(defun update-instance-for-slotds->values-map (instance map)
  "Update all of INSTANCE's slot values to match MAP."
  (dolist (slotd (c2mop:class-slots (class-of instance)))
    (multiple-value-bind (value present-p)
        (gethash slotd map)
      (if present-p
          (setf (slot-value instance (c2mop:slot-definition-name slotd))
                value)
          (slot-makunbound instance (c2mop:slot-definition-name slotd))))))

(defun compute-slot-diff-against-slotds->values-map (instance map)
  "Returns list of slotds that differ between INSTANCE and MAP, by slot equality fns."
  (remove-if
   (lambda (slotd)
     (let ((slot-name (c2mop:slot-definition-name slotd)))
       (multiple-value-bind (value present-p)
           (gethash slotd map)
         (when (and present-p
                    (slot-boundp instance slot-name))
           (funcall (equality slotd)
                    value
                    (slot-value instance slot-name))))))
   (c2mop:class-slots (class-of instance))))

(defmethod reinitialize-instance :around ((instance prevalence-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((old-values (slotds->values-map instance))
         ;; doing this here to save on computation inside the locks
         (instance (let (success-p)
                     (unwind-protect
                          (progn (with-ignored-prevalence (call-next-method))
                                 (setf success-p t)
                                 instance)
                       (unless success-p
                         (update-instance-for-slotds->values-map instance old-values)))))
         (updated-slots (compute-slot-diff-against-slotds->values-map instance old-values))
         available-p prevalence-insert-p success-p)
    (with-recursive-locks (prevalence-slot-locks (class-of instance) updated-slots)
      (unwind-protect
           (multiple-value-bind (temp-available-p problem-slots problem-values)
               (prevalence-instance-slots-available-p instance :slots updated-slots)
             (setf available-p temp-available-p)
             (unless available-p
               (error 'non-unique-unique-keys :breach-class (class-of instance)
                                              :breach-slots problem-slots
                                              :breach-values problem-values))
             (prevalence-insert-instance instance :slots updated-slots)
             (setf prevalence-insert-p t)
             (prevalence-remove-instance instance
                                         :slots updated-slots
                                         :values old-values)
             :persist ;TEMPER
             (setf success-p t)
             instance)
        (unless success-p
          (when available-p
            ;; since we use the same order as they were inserted in,
            ;; if we encounter an error, it should be at the first non-inserted slot,
            ;; which is presumably where we abandoned out anyhow.
            (ignore-errors (prevalence-remove-instance instance :slots updated-slots)))
          (update-instance-for-slotds->values-map instance old-values)
          (when prevalence-insert-p ; as above
            (ignore-errors (prevalence-insert-instance instance :slots updated-slots))))))))

(defmethod change-class :around ((instance prevalence-object) (new-class prevalence-class)
                                 &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (let ((old-values (slotds->values-map instance))
        (old-class (class-of instance))
        prevalence-remove-p call-next-method-p prevalence-insert-p success-p)
    (with-recursive-locks (union (all-prevalence-slot-locks-for old-class)
                                 (all-prevalence-slot-locks-for new-class))
      (unwind-protect
           (progn
             (prevalence-remove-instance instance)
             (setf prevalence-remove-p t)
             (with-ignored-prevalence
                 (call-next-method))
             (setf call-next-method-p t)
             (prevalence-insert-instance instance)
             (setf prevalence-insert-p t)
             :persist ;; TEMPER
             (setf success-p t)
             instance)
        (unless success-p
          (when prevalence-insert-p
            (prevalence-remove-instance instance))
          (when call-next-method-p
            (with-ignored-prevalence
                (call-next-method instance old-class)
              ;; Simpler than constructing initargs for call-next-method,
              ;; particularly since initializations can run arbitrary code
              (update-instance-for-slotds->values-map instance old-values)))
          (when prevalence-remove-p
            (prevalence-insert-instance instance)))))))

;;;; 3. PREVALENCE-SYSTEM section

;; The "canonical" lookup is on the unique IDs, so those are
;; also the lookups we can iterate to persist the whole store.

(defclass prevalence-system ()
  ((hash-store
    :initform (make-hash-table)
    :reader hash-store)
   (lock-store
    :initform (make-hash-table)
    :reader lock-store)
   (lock-store-lock
    :initform (bt:make-lock "lock-store-lock")
    :reader lock-store-lock))
  (:documentation
   "HASH-STORE is a hash by persistent class with hash by slot as value."))

(defvar *prevalence-system* (make-instance 'prevalence-system))

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

(defun prevalence-slot-available-p (class slotd value)
  (ccase (key slotd)
    ((:index nil) t)
    (:class-unique
     (not (prevalence-lookup-class-slot class slotd value)))
    (:precedence-unique
     (not (prevalence-lookup-class-slot
           (find-slot-defining-class class slotd) slotd value)))))

(defun prevalence-instance-slots-available-p
    (instance &key (slots (c2mop:class-slots (class-of instance))))
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

(defun (setf prevalence-lookup-class-slot) (new-value class slotd value)
  "Sets the appropriate nested hash-lookup value,
   creating new tables as necessary.
   NOTE: Assumes any relevant locks are held already."
  (let* ((class-table (gethash (class-name class)
                               (hash-store *prevalence-system*)))
         (slot-table (ignore-errors (gethash (c2mop:slot-definition-name slotd) class-table))))
    (unless class-table
      (setf (gethash (class-name class) (hash-store *prevalence-system*))
            (setf class-table (make-hash-table))))
    (unless slot-table
      (setf (gethash (c2mop:slot-definition-name slotd) class-table)
            (setf slot-table (make-hash-table :test (equality slotd)))))
    ;; Can't we access e.g. indexes with nil as value, then?
    ;; TEMPER - if there's no new value and we want to cease having an item available on the index,
    ;; maybe that's for slot-makunbound?
    (if (null new-value)
        (remhash value slot-table)
        (setf (gethash value slot-table) new-value))))

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
    (dolist (slotd slots)
      (when (slot-boundp instance (c2mop:slot-definition-name slotd))
        (prevalence-insert-class-slot
         class
         slotd
         (slot-value instance (c2mop:slot-definition-name slotd))
         instance)))))

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

(defun prevalence-remove-instance (instance &key slots values)
  (let ((class (class-of instance))
        (values (or values (slotds->values-map instance))))
    (dolist (slotd (or slots (c2mop:class-slots class)))
      (when (key slotd)
        (multiple-value-bind (value present-p)
            (gethash slotd values)
          (when present-p
            (prevalence-remove-class-slot
             (class-of instance) slotd value instance)))))))

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
    (prevalence-slot-locks class (c2mop:class-slots class))))

(defun find-slot-defining-class (class slotd)
  "Finds the most specific slot-defining-class by
   searching through CLASS's precedence list until
   the first hit for a direct-slot-definition."
  (assert slotd)
  (find-if (lambda (candidate-class)
             (find-if (rfix #'direct-effective-slot-equivalence slotd)
                      (c2mop:class-direct-slots candidate-class)))
           (c2mop:class-precedence-list class)))

(defun direct-effective-slot-equivalence (direct-slot effective-slot)
  (eq (c2mop:slot-definition-name direct-slot)
      (c2mop:slot-definition-name effective-slot)))

(defun prevalence-lookup-lock (class-name slot-name)
  "Looks up the look associted with CLASS-NAME and SLOT-NAME in
   *PREVALENCE-SYSTEM*'s lock-store. Creates entries and returns
   a newly-associated lock, if necessary."
  (let* ((class-table (gethash class-name (lock-store *prevalence-system*)))
         (lock (ignore-errors (gethash slot-name class-table))))
    (cond ((not class-table)
           (bt:with-lock-held ((lock-store-lock *prevalence-system*))
             (unless (gethash class-name (lock-store *prevalence-system*))
               (setf (gethash class-name (lock-store *prevalence-system*))
                     (make-hash-table))))
           (prevalence-lookup-lock class-name slot-name))
          ((not lock)
           (bt:with-lock-held ((lock-store-lock *prevalence-system*))
             (unless (gethash slot-name class-table)
               (setf (gethash slot-name class-table)
                     (bt:make-lock (format nil "~A lock: ~A ~A"
                                           *prevalence-system* class-name slot-name)))))
           (prevalence-lookup-lock class-name slot-name))
          (t lock))))
