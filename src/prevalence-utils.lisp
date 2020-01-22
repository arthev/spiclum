(in-package :spiclum)

;; Note: avoid locks on disk accesses. Perhaps enqueue all logging onto a
;;   single-thread?

;; LEGEND: CONSIDER, SEE, TODO

(defclass prevalence-class (standard-class)
  ()
  (:documentation "Meta-class for persistent objects using prevalence."))
;; NOTE: Since canonical IDs for each persistent object will be necessary
;;   to ensure a coherent universe, we have another instance where
;;   an :allocation :subclass option for a slot would be useful.
;;   (Of course, we can mimick the effect by including the slot in a wrapper
;;   macro...)
;;   But how to handle inheritance amongst the persistent objects?

(defclass keyable-slot (c2mop:standard-slot-definition)
  ((key :initarg :key
        :accessor key
        :initform nil
        :documentation "Subslot to specify whether a slot is a key.
                        See KEYABLE-SLOT-KEY.")))

(defparameter *persisting-p* t
  "Toggle for whether to persist (aka serialize and write) data or not.")

(defparameter *prevalencing-p* t
  "Toggle for whether to update the prevalence object store")

;;;; -1. Helpers

(defun class-metaobject-p (obj)
  (let ((class-groups '(standard-class built-in-class structure-class)))
    (some (lfix #'typep obj) class-groups)))

(defun metaclass-of (obj)
  "Returns the metaclass of OBJ as primarily value.
   Returns a secondary value indicating whether obj
   was a class-object (T) or not (nil).
   Probably assumes no metaclasses have metaclasses."
  (if (class-metaobject-p obj)
      (values (class-of obj) t)
      (values (class-of (class-of obj)) nil)))

;;;; 0. KEYABLE-SLOT section

(defun %member-of-legal-keyable-slot-key-values (x)
  (member x '(nil :unique :index)))

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
  (let ((class (if (typep instance-or-class 'standard-class)
                   instance-or-class
                   (class-of instance-or-class))))
    (remove-if-not (lambda (slot)
                     (and (typep slot 'keyable-slot)
                          (eq (key slot) key-type)))
                   (c2mop:class-slots class))))

(defun class-unique-key-slots (instance-or-class)
  "Returns a list of slot objects with :unique keys."
  (class-x-key-slots instance-or-class :unique))

(defun class-index-key-slots (instance-or-class)
  "Returns a list of slot objects with :index keys."
  (class-x-key-slots instance-or-class :index))

(define-condition non-unique-unique-key (error)
  ((breach-value
    :initarg :breach-value
    :accessor breach-value)
   (breach-slot
    :initarg :breach-slot
    :accessor breach-slot)
   (breach-class
    :initarg :breach-slot
    :accessor breach-class))
  (:report
   (lambda (condition stream)
     (format stream "Uniqueness broken: class ~S, slot ~S, value ~S"
             (breach-class condition)
             (breach-slot condition)
             (breach-value condition))))
  (:documentation
   "A condition for use when detecting (potential) breaches
    of the uniqueness constraint of unique keyable-slots."))

;;;; 1. PREVALENCE-CLASS section

;; Seems like the main intercessory points are setfing values, instantiating,
;; and reinstatiating (e.g. due to class change/redefinition).
;; Also seems like we'll need an additional point,
;; to remove instances from the prevalence system.

(defmacro with-ignored-prevalence (&rest body)
  `(let ((*persisting-p* nil)
         (*prevalencing-p* nil))
     ,@body))

(defun prevalence-writer (&rest args)
  (format t "Dummy-writer: ~S" args))

(defun prevalence-insert (&rest args)
  (format t "Dummy-insert: ~S" args))

(defmethod c2mop:validate-superclass ((class prevalence-class)
                                           (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)

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

(defmethod %acceptable-persistent-slot-value-type-p ((metaclass standard-class) new-value)
  (error "Can't use ~S as slot-value for persistent object, since it's metaclass ~S"
         new-value metaclass))

(defmethod %acceptable-persistent-slot-value-type-p ((metaclass structure-class) new-value)
  (error "Can't use ~S as slot-value for persistent object, since it's metaclass ~S"
         new-value metaclass))

(defmethod %acceptable-persistent-slot-value-type-p ((metaclass built-in-class) new-value)
  t)

(defmethod %acceptable-persistent-slot-value-type-p ((metaclass prevalence-class) new-value)
  t)


(defmethod (setf c2mop:slot-value-using-class) (new-value
                                                (class prevalence-class)
                                                instance
                                                slot-name)
  "Responsible for validation, calling the standard-class setf,
   and for making and commiting a transaction."
  ;; Must both clean up previous system-entry as well as set new entry.
  ;; And write to the persistence log (potentially).

  (acceptable-persistent-slot-value-type-p new-value) ; Provisional

  ;; TEMP
  (format t "Setting: ~S~%    ~S~%    ~S~%    ~S~%" new-value class instance slot-name)
  ;; Pre-check validity of move if slot is :unique. Might need a touch of locking here.

  ;; TODO: Naive! Slot might be unbound. We'll need some sort of sentinel for that,
  ;; along with making the slot unbound again in the case of an error.
  (let ((old-value (c2mop:slot-value-using-class class instance slot-name))
        ;; CLHS specifies that setf'ing can return multiple values
        (results (multiple-value-list (call-next-method))))
    (ecase (key slot-name)
      (:unique (format t "UNIQUE KEY DETECTED~%"))
      (:index  (format t "INDEX  KEY DETECTED~%"))
      ((nil)   (format t "NIL    KEY DETECTED~%")))

    ;; Move instance if indexing slot
    ;; Make and commit transaction
    (values-list results)))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  (declare (ignorable class initargs))
  (let ((instance (with-ignored-prevalence (call-next-method))))
    ;; Can probably lock here, check all uniques,
    ;; insert for uniques. Insert for indexes after.
    ;; Then serialize/persist.
    (apply #'prevalence-writer 'make-instance class initargs)
    instance))

(defclass ptest-class ()
  ((a
    :initarg :a
    :key :unique
    :accessor a))
  (:metaclass prevalence-class))

(defclass ptester-class2 (test-class)
  ((b
    :initarg :b
    :key :index
    :accessor b))
  (:metaclass prevalence-class))

(defclass ptester-class3 (test-class)
  ((c
    :initarg :c
    :key nil
    :accessor c))
  (:metaclass prevalence-class))


;;;; 2. PREVALENCE-SYSTEM section

(defclass prevalence-system ()
  ((lookup-hash
    :initform (make-hash-table)
    :reader lookup-hash))
  (:documentation
   "LOOKUP-HASH is a hash by persistent class with hash by slot as value."))

(defvar *prevalence-system* (make-instance 'prevalence-system))

(defun lookup-class-slot (class slot value)
  "We use class-name and slot-name rather than object,
   to ease redefinitions."
  ;; TODO: sanity checking
  (ignore-errors
   (gethash value
            (gethash (c2mop:slot-definition-name slot)
                     (gethash (class-name class)
                              (lookup-hash *prevalence-system*))))))

;; Something like this. Perhaps fewer args.
;; Has to handle a little locking behaviour, though.
;; Or do we relegate locking behaviour to calling layer? (e.g. setf and similar)
(defun insert-class-slot (class slot value object)
  (ccase (key slot)
    (:unique 'todo)
    (:index  'todo)
    ((nil) (cerror "Continue without insertion"
                   "INSERT-CLASS-SLOT called with class ~S and slot ~S, ~
                    but ~S is not a key."
                   class slot))))
