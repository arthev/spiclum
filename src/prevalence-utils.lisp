(in-package :spiclum)

;; LEGEND: CONSIDER, SEE, TODO

(defclass prevalence-class (standard-class)
  ()
  (:documentation "Meta-class for persistent objects using prevalence."))

;; NOTE: Since canonical IDs for each persistent object will be necessary
;;   to ensure a coherent universe, we have another instance where
;;   an :allocation :subclass option for a slot would be useful.
;;   (Of course, we can mimick the effect by including the slot in a wrapper
;;   macro...)

;; We can use some 'forbidden changes' signifier or something to react by refusing
;; to re-setf any sch primary canonical ID slots, perhaps. Either they must be static,
;; or we ust be able to register dependents in case of changes.

;; But how to handle inheritance amongst the persistent objects?
;; Just provide a query that specifies whether to look for the
;; explicit class, or the class and its subclasses, duh.

;; Uh-oh. How to handle :class allocated slots?
;; Lookup becomes irrelevant. Serializes trivially, if redundantly.
;; Maybe class-allocations with non-nil slot key are an error.

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

;; a get/ensure-hash-table sounds like a good abstractio

;;;; -1. Helpers

(defun metaclass-of (obj)
  "Returns the metaclass of OBJ as primarily value.
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
  (let* ((effective-slot-definition (call-next-method))
         (direct-slot-definition
           (find (c2mop:slot-definition-name effective-slot-definition)
                 direct-slot-definitions
                 :key #'c2mop:slot-definition-name)))
    (setf (key effective-slot-definition)
          (key direct-slot-definition)
          (equality effective-slot-definition)
          (equality direct-slot-definition))
    effective-slot-definition))

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

;; Seems like the main intercessory points are setfing values, instantiating,
;; and reinstatiating (e.g. due to class change/redefinition).
;; Also seems like we'll need an additional point,
;; to remove instances from the prevalence system.

(defmacro with-ignored-prevalence (&rest body)
  `(let ((*persisting-p* nil)
         (*prevalencing-p* nil))
     ,@body))

(defmethod c2mop:validate-superclass ((class prevalence-class)
                                      (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)

;; TODO: If we decide to allow mixins, then we probably
;; want to validate the reverse of the above too. (On a 'caveat emptor' basis.)
;; :precedence-unique probably behaves weird with mixins.
;; Even worse, mix-ins don't *have* to share metaclass, even!
;; One possibility is to forbid non-shared metaclass inheritance.
;; Can be checked during definining a class, obviously.
;; The other option is to allow it, but then we have to filter for
;; a lot of stuff...
;; Can probably disallow for the time being (or design as if disallowed),
;; because it'll give immediate errors if tried unless those filters are
;; in place.

;; To avoid problems with circularities, one option is to lazy up slots with
;; persistent objects as values, and then force them during lookup.
;; E.g. when serializing, serialize through lazy wrapper.
;; Can then either:
;; a) force during slot lookup (constant overhead)
;; b) force all during initialization (slower initialization)

;; Atomic setf for if multiple setfs are part of a transaction,
;; so if we are persisting-p or prevalencing-p can be trusted.

(defmethod canonicalize-direct-slot-property-value (property value)
  (declare (ignore property))
  value)

(defmethod canonicalize-direct-slot-property-value ((property (eql :equality)) value)
  (if (functionp value)
      value
      (destructuring-bind (_ fn-name)
          value
        (declare (ignore _))
        (symbol-function fn-name))))

(defun canonicalize-direct-slot (direct-slot-plist)
  (loop for (property value) on direct-slot-plist by #'cddr
        collect property
        collect (canonicalize-direct-slot-property-value property value)))

(defmethod c2mop:ensure-class-using-class ((class prevalence-class)
                                           name &rest args
                                           &key direct-slots
                                           &allow-other-keys)
  (let ((updated-slots (mapcar #'canonicalize-direct-slot direct-slots)))
    (apply #'call-next-method class name
           (replace-property :direct-slots updated-slots args))))

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

(defun guarded-slot-value-using-class (class instance slotd)
  ;; TODO: Might this be just guarded-slot-value?
  (if (slot-boundp instance (c2mop:slot-definition-name slotd))
      (values (c2mop:slot-value-using-class class instance slotd)
              t)
      (values nil
              nil)))

(defmethod (setf c2mop:slot-value-using-class) (new-value
                                                (class prevalence-class)
                                                instance
                                                slotd)
  "Responsible for validation, calling the standard-class setf,
   and for making and commiting a transaction."
  (assert (acceptable-persistent-slot-value-type-p new-value))
  (multiple-value-bind (old-value slot-boundp)
      (guarded-slot-value-using-class class instance slotd)
    (let (;; CLHS specifies that setf'ing can return multiple values
          (results (multiple-value-list (call-next-method)))
          (prevalenced-p nil)
          (persisted-p   nil)
          (completed-p   nil))
      (unwind-protect
           ;; Ooh - but what if a slot is equality #'eq, but that object has
           ;; been updated and someone's basically trying to 'write' the change?
           (unless (and slot-boundp
                        (funcall (equality slotd) new-value old-value))
             (prevalence-insert-class-slot class slotd new-value instance)
             (setf prevalenced-p t)
             '|persist-(lock)|
             (setf persisted-p t)
             (when slot-boundp
               (prevalence-remove-class-slot class slotd old-value instance))
             (setf completed-p t)
             (values-list results))
        (unless completed-p
          (when prevalenced-p
            (prevalence-remove-class-slot class slotd new-value instance)
            (if slot-boundp
                (call-next-method old-value class instance slotd)
                (slot-makunbound instance (c2mop:slot-definition-name slotd))))
          (when persisted-p
            '|persist-reverse?|))))))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  (declare (ignorable class initargs))
  (let ((instance (with-ignored-prevalence (call-next-method)))
        problem-slots problem-values)
    (with-recursive-locks (all-prevalence-slot-locks-for instance)
      (multiple-value-bind (available-p inner-problem-slots inner-problem-values)
          (prevalence-instance-slots-available-p instance)
        (if available-p
            (prevalence-insert-instance instance)
            (setf problem-slots inner-problem-slots
                  problem-values inner-problem-values))))
    (if problem-slots
        (error 'non-unique-unique-keys :breach-class class
                                       :breach-slots problem-slots
                                       :breach-values problem-values)
        :persist) ;TEMPER '|persist-(lock): 'make-instance class initargs|))
    instance))

;;;; 2. Prevalence-object section

(defclass prevalence-object ()
  ()
  (:metaclass prevalence-class)
  (:documentation "Class for prevalent objects to inherit, to specialize on
                   reinitialize-instance etc."))

(defmacro defpclass (class-name superclasses slot-specifiers &rest class-options)
  (when (find :metaclass class-options :key #'car)
    (error "DEFPCLASS uses implicit metaclass PREVALENCE-CLASS,~%~
            but an explicit metaclass was provided for ~S" class-name))
  `(defclass ,class-name (,@superclasses prevalence-object)
     ,slot-specifiers
     ,@class-options
     (:metaclass prevalence-class)))

(defmethod reinitialize-instance :around ((instance prevalence-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((old-values (slotds->values-map instance))
         (instance (with-ignored-prevalence (call-next-method)))
         (updated-slots
           (remove-if
            (lambda (slotd)
              (let ((slot-name (c2mop:slot-definition-name slotd)))
                (multiple-value-bind (value present-p)
                    (gethash slotd old-values)
                  (when (and present-p
                             (slot-boundp instance slot-name))
                    (funcall (equality slotd)
                             value
                             (slot-value instance slot-name))))))
            (c2mop:class-slots (class-of instance))))
         problem-slots problem-values)
    (with-recursive-locks (prevalence-slot-locks (class-of instance) updated-slots)
      (multiple-value-bind (available-p inner-problem-slots inner-problem-values)
          (prevalence-instance-slots-available-p instance :slots updated-slots)
        (if available-p
            (progn
              (prevalence-insert-instance instance :slots updated-slots)
              (prevalence-remove-instance instance
                                          :slots updated-slots
                                          :values old-values))
            (setf problem-slots inner-problem-slots
                  problem-values inner-problem-values))))
    (if problem-slots
        (error 'non-unique-unique-keys :breach-class (class-of instance)
                                       :breach-slots problem-slots
                                       :breach-values problem-values)
        :persist))) ;TEMPER

(defmethod change-class :around ((instance prevalence-object) new-class-name
                                 &rest initargs &key &allow-other-keys)
  (let ((old-values (slotds->values-map instance))
        (old-class (class-of instance))
        (new-class (find-class new-class-name)))
    (with-recursive-locks (append (all-prevalence-slot-locks-for old-class)
                                  (all-prevalence-slot-locks-for new-class))
      (handler-case
          (progn
            ;remove from all indexes
            (with-ignored-prevalence
                (call-next-method))
            ;add to all indexes
            '|serializer-call|
            instance)
        (error (e)
          (with-ignored-prevalence
              (call-next-method instance (class-name old-class)
                                "old-init-args")) ; todo
          ;add to all indexes
          (values instance e))))))



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
  "Takes a prevalence CLASS and a SLOTD object, as well as a VALUE,
   and looks up the relevant entries. The keys in the PREVALENCE-SYSTEM
   HASH-STORE are the names of the class and slot, however, to ease
   redefinitions."
  (handler-bind ((type-error
                   (lambda (condition)
                     (when (and (eq nil (type-error-datum condition))
                                (eq 'hash-table (type-error-expected-type condition)))
                       (return-from prevalence-lookup-class-slot nil)))))
    (gethash value
             (gethash (c2mop:slot-definition-name slotd)
                      (gethash (class-name class)
                               (hash-store *prevalence-system*))))))

(defun prevalence-lookup-available-p (class slotd value)
  (ccase (key slotd)
    ((:index nil) t)
    (:class-unique
     (not (prevalence-lookup-class-slot class slotd value)))
    (:precedence-unique
     (not (prevalence-lookup-class-slot
           (find-slot-defining-class class slotd) slotd value)))))

(defun prevalence-instance-slots-available-p (instance &key slots)
  (let ((class (class-of instance))
        problem-slots problem-values)
    (dolist (slotd (or slots (c2mop:class-slots class)))
      (let* ((slot-name (c2mop:slot-definition-name slotd))
             (slot-value (ignore-errors (slot-value instance slot-name))))
        (when (slot-boundp instance slot-name)
          (unless (prevalence-lookup-available-p class slotd slot-value)
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
    (if (null new-value)
        (remhash value slot-table)
        (setf (gethash value slot-table) new-value))))

(defun prevalence-insert-class-slot (class slotd value object)
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

(defun prevalence-insert-instance (instance &key slots)
  (let ((class (class-of instance)))
    (dolist (slotd (or slots (c2mop:class-slots class)))
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

;;;; Serializing has to respect unbound slots
