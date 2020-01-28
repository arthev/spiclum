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

;; Uh-oh. How to handle uniqueness for subclasses of a class?
;; Say we have an unique email, but we have more than one type of
;; user class (all inheriting from a root class).
;; Do we need :class-unique versus :precedence-unique?
;; What if there's mix-ins. :precedence-unique now behaves slightly weird.

;; Even worse, mix-ins don't *have* to share metaclass, even!
;; One possibility is to forbid non-shared metaclass inheritance.
;; Can be checked during definining a class, obviously.
;; The other option is to allow it, but then we have to filter for
;; a lot of stuff...
;; Can probably disallow for the time being (or design as if disallowed),
;; because it'll give immediate errors if tried unless those filters are
;; in place.

;; Uh-oh. How to handle :class allocated slots?

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

;; TODO: Need to add logic for these two :) *persisting-p* and *prevalencing-p*

(defparameter *persisting-p* t
  "Toggle for whether to persist (aka serialize and write) data or not.")

(defparameter *prevalencing-p* t
  "Toggle for whether to update the prevalence object store")

;;;; -2. Malplaced Helpers

(defmacro with-recursive-locks (locks &body body)
  "Non-portable: bordeaux-threads doesn't support lock-name lookup."
  `(call-with-recursive-locks
    (sort ,locks #'string< :key #'sb-thread:mutex-name)
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
          (key direct-slot-definition))
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

(define-condition prevalence-breach (error)
  ((breach-value
    :initarg :breach-value
    :accessor breach-value)
   (breach-slot
    :initarg :breach-slot
    :accessor breach-slot)
   (breach-class
    :initarg :breach-class
    :accessor breach-class))
  (:report
   (lambda (condition stream)
     (format stream "Breach for class ~S, slot ~S, value ~S"
             (breach-class condition)
             (breach-slot condition)
             (breach-value condition))))
  (:documentation
   "Ancestor error for different breaches of prevalence constraints."))

(define-condition non-unique-unique-key (prevalence-breach)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Uniqueness broken: class ~S, slot ~S, value ~S"
             (breach-class condition)
             (breach-slot condition)
             (breach-value condition))))
  (:documentation
   "For breaches of the uniqueness constraints on keyable-slots."))

(define-condition removing-nonexistant-entry (prevalence-breach)
  ((breach-object
   :initarg :breach-object
   :accessor breach-object))
  (:report
   (lambda (condition stream)
     (format stream "Tried to remove object ~S, for class ~S, slot ~S, value ~S,~% ~
                     but couldn't find an entry"
             (breach-object condition) (breach-class condition)
             (breach-slot   condition) (breach-value condition))))
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

;; To avoid problems with circularities, one option is to lazy up slots with
;; persistent objects as values, and then force them during lookup.
;; E.g. when serializing, serialize through lazy wrapper.
;; Can then either:
;; a) force during slot lookup (constant overhead)
;; b) force all during initialization (slower initialization)

;; Atomic setf for if multiple setfs are part of a transaction,
;; so if we are persisting-p or prevalencing-p can be trusted.


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
           (progn
             (prevalence-insert-class-slot class slotd new-value instance)
             (setf prevalenced-p t)
             '|persist-(lock)|
             (setf persisted-p t)
             (prevalence-remove-class-slot class slotd old-value instance)
             (setf completed-p t)
             (values-list results))
        (unless completed-p
          (when prevalenced-p
            (prevalence-remove-class-slots class slotd new-value instance)
            (if slot-boundp
                (call-next-method old-value class instance slotd)
                (slot-makunbound instance (c2mop:slot-definition-name slotd))))
          (when persisted-p
            '|persist-reverse?|))))))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  (declare (ignorable class initargs))
  (let ((instance (with-ignored-prevalence (call-next-method))))
    '|prevalence-for-all-slots-(lock)|
    '|persist-(lock): 'make-instance class initargs|
    instance))



;;;; 2. PREVALENCE-SYSTEM section

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

(defun (setf prevalence-lookup-class-slot) (new-value class slotd value)
  "Sets the appropriate nested hash-lookup value,
   creating new tables as necessary."
  (let* ((class-table (gethash (class-name class)
                               (hash-store *prevalence-system*)))
         (slot-table (ignore-errors (gethash (c2mop:slot-definition-name slotd) class-table))))
    (unless class-table
      (setf (gethash (class-name class) (hash-store *prevalence-system*))
            (setf class-table (make-hash-table))))
    (unless slot-table
      (setf (gethash (c2mop:slot-definition-name slotd) class-table)
            (setf slot-table (make-hash-table :test (equality slotd)))))
    (setf (gethash value slot-table) new-value)))

(defun prevalence-insert-class-slot (class slotd value object)
  (unless *prevalencing-p* (return-from prevalence-insert-class-slot nil))
  (ccase (key slotd)
    (:class-unique
     (with-recursive-locks (prevalence-slot-locks class slotd)
       (if (prevalence-lookup-class-slot class slotd value)
           (error 'non-unique-unique-key
                  :breach-class class :breach-slot slotd :breach-value value)
           (setf (prevalence-lookup-class-slot class slotd value) object))))
    ;; TODO: for :prec-uniq and :index, we can find the slot-defining-class first before locking
    (:precedence-unique
     (with-recursive-locks (prevalence-slot-locks class slotd)
       (let ((slot-defining-class (find-slot-defining-class class slotd)))
         (if (prevalence-lookup-class-slot slot-defining-class slotd value)
             (error 'non-unique-unique-key
                    :breach-class slot-defining-class :breach-slot slotd :breach-value value)
             (setf (prevalence-lookup-class-slot slot-defining-class slotd value) object)))))
    (:index
     (with-recursive-locks (prevalence-slot-locks class slotd)
       (pushnew object
                (prevalence-lookup-class-slot (find-slot-defining-class class slotd)
                                              slotd
                                              value))))
    ((nil) :do-nothing)))

(defun prevalence-remove-class-slot (class slotd value object)
  (unless *prevalencing-p* (return-from prevalence-remove-class-slot nil))
  (flet ((unique-removal (using-class)
           (if (eq (prevalence-lookup-class-slot using-class slotd value)
                   object)
               (setf (prevalence-lookup-class-slot using-class slotd value) nil)
               (error 'removing-nonexistant-entry
                      :breach-class using-class :breach-slot slotd
                      :breach-value value       :breach-object object))))
    (ccase (key slotd)
      (:class-unique
       (unique-removal class))
      (:precedence-unique
       (unique-removal (find-slot-defining-class class slotd)))
      (:index
       (let ((slot-defining-class (find-slot-defining-class class slotd)))
         (with-recursive-locks (prevalence-slot-locks slot-defining-class slotd)
           (if (find object (prevalence-lookup-class-slot slot-defining-class slotd value))
               (setf (prevalence-lookup-class-slot slot-defining-class slotd value)
                     (remove object (prevalence-lookup-class-slot slot-defining-class slotd value)))
               (error 'removing-nonexistant-entry
                      :breach-class slot-defining-class :breach-slot slotd
                      :breach-value value               :breach-object object))))))))

(defun prevalence-slot-locks (class &rest slotds)
  "Returns a sorted list of locks associated with the CLASS and SLOTDS."
  (flet ((lock-for-slot-defining-class (slotd)
           (let ((slot-defining-class (find-slot-defining-class class slotd)))
             (assert slot-defining-class)
             (prevalence-lookup-lock (class-name slot-defining-class)
                                     (c2mop:slot-definition-name slotd)))))
    (mapcar (lambda (slotd)
              (ccase (key slotd)
                (:class-unique (prevalence-lookup-lock (class-name class)
                                                       (c2mop:slot-definition-name slotd)))
                (:index (lock-for-slot-defining-class slotd))
                (:precedence-unique (lock-for-slot-defining-class slotd))))
            slotds)))

(defun find-slot-defining-class (class slotd)
  ;; TODO: Evaluate if we have to finalize classes.
  ;; TODO: Check the MOP book to see if this is proper behaviour.
  ;; ALSO if we need to mark nodes to avoid infinite recursion.
  (assert slotd)
  (cond ((null class)
         nil)
        ((find-if (rfix #'direct-effective-slot-equivalence slotd)
                  (c2mop:class-direct-slots class))
         class)
        (t
         (find-slot-defining-class
          (find-if (lambda (candidate-class)
                     (find-if (rfix #'effective-effective-slot-equivalence slotd)
                              (c2mop:class-slots candidate-class)))
                   (cdr (c2mop:class-precedence-list class)))
          slotd))))

(defun direct-effective-slot-equivalence (direct-slot effective-slot)
  (eq (c2mop:slot-definition-name direct-slot)
      (c2mop:slot-definition-name effective-slot)))

(defun effective-effective-slot-equivalence (slot1 slot2)
  (eq (c2mop:slot-definition-name slot1)
      (c2mop:slot-definition-name slot2)))

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
