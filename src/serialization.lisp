(in-package :spiclum)

(defparameter *persisting-p* t
  "Toggle for whether to persist data or not.")

(defparameter *prevalence->lookup-serialization-p* nil
  "Whether to serialize prevalence-objects as lookups by uuid or not.")

(defparameter *saving-world-p* nil
  "Whether we're in the process of saving the world.")

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

;;; Miscellaneous

;; TODO introduce macro like hashify
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
    (values
     `(let ((reloaded-instance
              (make-instance ',(class-name class)
                             ,@initargs)))
        ,@setf-forms
        reloaded-instance)
     initargs)))

(defmacro hashify ((&key test size rehash-size rehash-threshold) &body pairs)
  ;; TODO: Can probably be a function
  (assert (evenp (length pairs)))
  (let ((ht (gensym)))
    `(let ((,ht (make-hash-table :test ,test
                                 :size ,size
                                 :rehash-size ,rehash-size
                                 :rehash-threshold ,rehash-threshold)))
       ,@(loop for (key value) on pairs by #'cddr
       collect `(setf (gethash ,key ,ht) ,value))
       ,ht)))

(defun arrayify (array-args &rest elts)
  (let ((array (apply #'make-array array-args)))
    (loop for i from 0
          for elt in elts
          do (setf (row-major-aref array i) elt))
    array))

;;;; 1. Data Serialization

(defgeneric serialize-object (object)
  (:documentation "Generic to serialize arbitrary objects.

Recursive in many cases - e.g. the elements of a list must
be serializable if the list is to be serializable.

For a prevalence-object to accept a value in one of its slots,
the value must be serializable as per this generic having an
applicable method for that value. See
acceptable-persistent-slot-value-type-p."))

;;;; TODO:
;;;; Pathnames???? No idea

;;;; conditions - http://www.lispworks.com/documentation/lw50/CLHS/Body/e_cnd.htm
;;;;              makes it seem like traversing arbitrary slots etc. might be quite hard

;;;; structures are decently complicated due to non-standard readers/writers
;;;;    Structs have terrible introspection. But by default they have a readable printable representation.
;;;;      maybe we can hack that by: copy a struct, iterate its slots (not portable!), populate the new struct
;;;;      with the serialize-object calls, and then use _that_ as the serialization.
;;;;    Alternatively, maybe def-ser-struct which just adds inheritance of a struct with slots specifically
;;;;      to carry the introspection information. Oh, but wait, structs only allow single inheritance what fucking junk.
;;;;    Alright, we can add a def-serializable-struct that parses the def-struct form similarly to defstruct, and
;;;;      then we can use THAT information to build a defmethod that yields the information we need to serializable the
;;;;      fucking stuff.

(defmethod serialize-object ((number number))
  number)

(defmethod serialize-object ((symbol symbol))
  (if (keywordp symbol)
      symbol
      `',symbol))

(defmethod serialize-object ((character character))
  ;; TODO?: Use the printer-escaping behaviour as per
  ;; lispworks.com/documentation/lw50/CLHS/Body/22_acb.htm
  character)

(defmethod serialize-object ((string string))
  (if (simple-string-p string)
      string
      (call-next-method)))

(defmethod serialize-object ((state random-state))
  ;; http://www.lispworks.com/documentation/lw50/CLHS/Body/t_rnd_st.htm
  ;; requires random-states to have a readable printable representation.
  state)

(defmethod serialize-object ((list list))
  ;: TODO: Handle circularity
  ;; See the following link for discussion on different list types that need special handling:
  ;; https://stackoverflow.com/questions/60247877/check-for-proper-list-in-common-lisp
  (let ((*prevalence->lookup-serialization-p* t))
    (cond ((null list) nil)
          ((null (cdr (last list))) `(list ,@(mapcar #'serialize-object list)))
          (t `(cons ,(serialize-object (car list))
                    ,(serialize-object (cdr list)))))))

(defmethod serialize-object ((array array))
  (let* ((*prevalence->lookup-serialization-p* t)
         (fill-pointer (ignore-errors (fill-pointer array))))
    `(arrayify
      '(,(if fill-pointer
             fill-pointer
             (array-dimensions array))
        :element-type ,(array-element-type array)
        :adjustable ,(adjustable-array-p array)
        :fill-pointer ,fill-pointer)
      ,@(loop for i from 0 below (if fill-pointer
                                     fill-pointer
                                     (reduce #'* (array-dimensions array)))
              collect (serialize-object (row-major-aref array i))))))

(defmethod serialize-object ((ht hash-table))
  (let ((*prevalence->lookup-serialization-p* t))
    `(hashify (:test ,(list 'function (hash-table-test ht))
                :size ,(hash-table-size ht)
                :rehash-size ,(hash-table-rehash-size ht)
                :rehash-threshold ,(hash-table-rehash-threshold ht))
       ,@(loop for key being the hash-keys of ht
               collect (serialize-object key)
               collect (serialize-object (gethash key ht))))))

(defmethod serialize-object ((class standard-class))
  (assert (class-name class))
  `(find-class ',(class-name class)))

(defmethod serialize-object ((thunk thunk))
  (serialize-object (force thunk)))

(defmethod serialize-object ((instance prevalence-object))
  (if *prevalence->lookup-serialization-p*
      (if *saving-world-p*
          `(thunk (find-by-uuid ,(uuid instance)))
          `(find-by-uuid ,(uuid instance)))
      (let ((*prevalence->lookup-serialization-p*))
        (instance->make-instance-form instance))))

;;;; 2. MOPy Action Serialization

(defun serialize-slot-makunbound-using-class (class object slotd)
  (let ((*prevalence->lookup-serialization-p* t))
    (serialize-write
     `(c2mop:slot-makunbound-using-class ,(serialize-object class)
                                         ,(serialize-object object)
                                         (slot-by-name ,(serialize-object class)
                                                       ',(c2mop:slot-definition-name slotd))))))

(defun serialize-setf-slot-value-using-class (new-value class instance slotd)
  (let ((*prevalence->lookup-serialization-p* t))
    (serialize-write
     `(setf (c2mop:slot-value-using-class ,(serialize-object class)
                                          ,(serialize-object instance)
                                          (slot-by-name ,(serialize-object class)
                                                        ',(c2mop:slot-definition-name slotd)))
            ,(serialize-object new-value)))))

(defun serialize-make-instance (instance initargs)
  (let ((*prevalence->lookup-serialization-p* t))
    (multiple-value-bind (serialization-form serialized-initargs)
        (instance->make-instance-form instance)
      ;; TODO: Throw a specific serialization error instead?
      (assert (subsetp (plist-keys initargs)
                       (plist-keys serialized-initargs)))
      (serialize-write serialization-form))))

(defun serialize-ensure-class-using-metaclass (class name args)
  ;; Yeah I realize this looks a bit weird, but until we change the MOP...
  (let ((*prevalence->lookup-serialization-p* t))
    (serialize-write
     `(c2mop:ensure-class-using-class ,(serialize-object class)
                                      ,(serialize-object name)
                                      ,@(mapcar #'serialize-object args)))))
;;;; 3. IO

(defun serialize-write (form)
  (format t "~S~%" form))
