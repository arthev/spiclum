(in-package :bmcg)

;;;; Class Hierarchy for Testing

(defclass top ()
  ((pu-top
    :initarg :pu-top
    :key :precedence-unique
    :equality #'equal
    :accessor pu-top)
   (i-top
    :initarg :i-top
    :key :index
    :equality #'equalp
    :accessor i-top)
   (cu-top
    :initarg :cu-top
    :key :class-unique
    :accessor cu-top)
   (nil-top
    :initarg :nil-top
    :accessor nil-top))
  (:metaclass prevalence-class))

(defclass left-mid (top)
  ((pu-left-mid
    :initarg :pu-left-mid
    :key :precedence-unique
    :equality #'equal
    :accessor pu-left-mid))
  (:metaclass prevalence-class))

(defclass right-mid (top)
  ((pu-right-mid
    :initarg :pu-right-mid
    :key :precedence-unique
    :equality #'equal
    :accessor pu-right-mid))
  (:metaclass prevalence-class))

(defclass bottom (left-mid right-mid)
  ((i-bottom
    :initarg :i-bottom
    :key :index
    :equality #'equalp
    :accessor i-bottom))
  (:metaclass prevalence-class))

(defun hierarchy ()
  (mapcar #'find-class '(top left-mid right-mid bottom)))

(dolist (class (hierarchy))
  (c2mop:finalize-inheritance class))

;;;; Helpers

(defun slot-by-name (class name)
  (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))

;;;; Tests for Prevalence System

(5am:test :find-slot-defining-class-finds-expected-classes
  (destructuring-bind (top left right bottom) (hierarchy)
    (dolist (class (hierarchy))
      (5am:is (eq top
                  (find-slot-defining-class class (slot-by-name top 'pu-top)))))))
