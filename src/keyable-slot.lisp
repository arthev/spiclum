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
  (member x '(nil :unique :class-unique :index)))

(deftype keyable-slot-key ()
  '(satisfies %member-of-legal-keyable-slot-key-values))

(defclass keyable-direct-slot (keyable-slot
                               c2mop:standard-direct-slot-definition)
  ())

(defclass keyable-effective-slot (keyable-slot
                                  c2mop:standard-effective-slot-definition)
  ())

(defmethod initialize-instance :before ((slot keyable-slot) &key key allocation &allow-other-keys)
  "Sanity checks KEY.

This single :before method on INITIALIZE-INSTANCE is presumed sufficient,
since since http://mop.lisp.se/www.alu.org/mop/dictionary.html says
portable programs must not define methods on shared-initialize or
reinitialize-instance for slot definition metaobjects."
  (check-type key keyable-slot-key)
  (assert (not (and (eq :class allocation) key))))

;; 1. To enable a persistent object to also inherit from non-persistent ones...

(defmethod key (c2mop:standard-direct-slot-definition)
  nil)

(defmethod equality (c2mop:standard-direct-slot-definition)
  #'eql)
