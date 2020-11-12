(in-package :spiclum)

;;;; 0. Basic definitions

(defclass keyable-slot (c2mop:standard-slot-definition)
  ((key
    :initarg :key
    :accessor key
    :initform nil
    :documentation
    "Specifies whether a slot is a key. See KEYABLE-SLOT-KEY.")
   (equality
    :initarg :equality
    :accessor equality
    :initform #'eql
    :documentation
    "Specifies how to compare the slot values for equality.
    Used for indexing/keying by the prevalence system.")))

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

(defmethod initialize-instance :before ((slot keyable-slot) &key key allocation &allow-other-keys)
  "Sanity checks KEY."
  (check-type key keyable-slot-key)
  (assert (not (and (eq :class allocation) key))))

;;;; 1. Convenience access

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
