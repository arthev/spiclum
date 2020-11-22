(in-package :spiclum)

(defvar *persisting-p* t
  "Toggle for whether to persist data or not.")

(defvar *prevalence->lookup-serialization-p* nil
  "Whether to serialize prevalence-objects as lookups by uuid or not.")

(defvar *saving-world-p* nil
  "Whether we're in the process of saving the world.")

(defvar *serialization-lock* (bt:make-lock "serialization-lock")
  "Lock to prevent multiple access to the transaction log.")

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

(defun instance->make-instance-form (instance)
  ;; Not all slots have initargs, not all initargs become slot-values
  (let ((class (class-of instance))
        initargs value-maps)
    (dolist (slotd (c2mop:class-slots class))
      (let ((slot-name (c2mop:slot-definition-name slotd)))
        (multiple-value-bind (slot-value slot-boundp)
            (guarded-slot-value instance slot-name)
          (when slot-boundp
            (prependf value-maps `(',slot-name ,slot-value))
            (lwhen (initarg (car (c2mop:slot-definition-initargs slotd)))
              (prependf initargs `(,initarg ,slot-value)))))))
    (values
     `(instancify
       (list ',(class-name class) ,@initargs)
       ,@value-maps)
     initargs)))

(defun instancify (instance-args &rest pairs)
  (assert (evenp (length pairs)))
  (let ((instance (apply #'make-instance instance-args)))
    (loop for (slot-name value) on pairs by #'cddr
          do (setf (slot-value instance slot-name) value))
    instance))

(defun hashify (hash-args &rest pairs)
  (assert (evenp (length pairs)))
  (assert (equal '(:test :size :rehash-size :rehash-threshold)
                 (plist-keys hash-args)))
  (let ((ht (apply #'make-hash-table hash-args)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key ht) value))
    ht))

(defun arrayify (array-args &rest elts)
  (assert (equal '(:element-type :adjustable :fill-pointer)
                 (plist-keys (cdr array-args))))
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
;;;;    -- should be simple. Just serialize as `(make-pathname :name ,name etc)

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
    `(hashify
         (list
          :test ,`(function ,(hash-table-test ht))
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

(defun serialize-change-class (instance new-class initargs)
  (let ((*prevalence->lookup-serialization-p* t))
    (serialize-write
     `(change-class ,(serialize-object instance)
                    ,(serialize-object new-class)
                    ,@(mapcar #'serialize-object initargs)))))

(defun serialize-reinitialize-instance (instance initargs)
  (let ((*prevalence->lookup-serialization-p* t))
    (serialize-write
     `(reinitialize-instance ,(serialize-object instance)
                             ,@(mapcar #'serialize-object initargs)))))
;;;; 3. IO

(defun serialize-write (form)
  (bt:with-lock-held (*serialization-lock*)
    (with-open-file (out (if *saving-world-p*
                             (world-file *prevalence-system*)
                             (log-file *prevalence-system*))
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format out "~S~%" form))))
