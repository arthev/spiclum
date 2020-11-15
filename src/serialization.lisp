(in-package :spiclum)

;;;; 0. Utilities

;;; Some lazy ones to handle lookups when recreating the world

(defclass thunk ()
  ((value :initarg :value)
   (%forced-p :initform nil)))

(defmacro thunk (&body body)
  `(make-instance 'thunk
                  :value (lambda () ,@body)))

(defgeneric force (object))

(defmethod force (object)
  object)

(defmethod force ((thunk thunk))
  (with-slots (value %forced-p) thunk
    (if %forced-p
        (force value)
        (progn (setf value (force (funcall value))
                     %forced-p t)
               value))))

(defun instance->make-instance-form (instance)
  ;; Complications since not all slots have initargs,
  ;; and generally supplying a value as an initarg
  ;; doesn't mean that becomes the value of the associated slot.
  (let* ((class
           (class-of instance))
         (slot-map
           (mapappend
            (lambda (slotd)
              (let ((slot-name (c2mop:slot-definition-name slotd)))
                (when (slot-boundp instance slot-name)
                  `((,slot-name
                     :value ,(serialize-object (slot-value instance slot-name))
                     :initarg ,(car (c2mop:slot-definition-initargs slotd)))))))
            (c2mop:class-slots class)))
         (initargs
           (mapappend
            (lambda (map)
              (destructuring-bind (name &key value initarg) map
                (declare (ignore name))
                (when initarg
                  `(,initarg ,value))))
            slot-map))
         (setf-forms
           (mapcar
            (lambda (map)
              (destructuring-bind (name &key value initarg) map
                (declare (ignore initarg))
                `(setf (slot-value reloaded-instance ',name) ,value)))
            slot-map)))
    `(let ((reloaded-instance
             (make-instance ',(class-name class)
                            ,@initargs)))
       ,@setf-forms
       reloaded-instance)))

;;;; 1. Serialization

;;;; Chapter 2 of CLTL2 should be highly relevant...
;;;; It lists common lisp data types, after all!
;;;; Also: http://www.lispworks.com/documentation/lw50/CLHS/Body/04_bb.htm
;;;; http://www.lispworks.com/documentation/lw50/CLHS/Body/04_bc.htm

(defparameter *persisting-p* t
  "Toggle for whether to persist data or not.")

(defparameter *prevalence->lookup-serialization-p* nil)

(defgeneric serialize-object (object)
  (:documentation "Generic to serialize arbitrary objects.

Recursive in many cases - e.g. the elements of a list must
be serializable if the list is to be serializable.

For a prevalence-object to accept a value in one of its slots,
the value must be serializable as per this generic having an
applicable method for that value. See
acceptable-persistent-slot-value-type-p."))

(defmethod serialize-object (object)
  :undef)

(defmethod serialize-object ((number number))
  number)

(defmethod serialize-object ((symbol symbol))
  `',symbol)

(defmethod serialize-object ((string string))
  ;; TODO: Handle both simple and non-simple strings appropriately
  string)

(defmethod serialize-object ((list list))
  (let ((*prevalence->lookup-serialization-p* t))
    (cons 'list (mapcar #'serialize-object list))))

(defmethod serialize-object ((class standard-class))
  (assert (class-name class))
  `(find-class ',(class-name class)))

(defmethod serialize-object ((thunk thunk))
  (serialize-object (force thunk)))

(defmethod serialize-object ((instance prevalence-object))
  (if *prevalence->lookup-serialization-p*
      `(thunk (find-by-uuid ,(uuid instance)))
      (let ((*prevalence->lookup-serialization-p*))
        (instance->make-instance-form instance))))
