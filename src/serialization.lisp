(in-package :spiclum)

(defvar *persisting-p* t
  "Toggle for whether to persist data or not.")

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

(defun thunkp (obj)
  (typep obj 'thunk))

(defgeneric force (object))

(defmethod force (object)
  object)

(defmethod force ((string string))
  string)

(defmethod force ((thunk thunk))
  (with-slots (value %forced-p) thunk
    (unless %forced-p
      (setf value (force (funcall value))
            %forced-p t))
    (force value)))

(defmethod force ((tree list))
  ;; TODO: Handle circularity
  (unless (null tree)
    (setf (car tree) (force (car tree))
          (cdr tree) (force (cdr tree))))
  tree)

(defmethod force ((array array))
  (loop for i from 0 below (reduce #'* (mklist (array-dimensions array)))
        do (setf (row-major-aref array i)
                 (force (row-major-aref array i))))
  array)

(defmethod force ((ht hash-table))
  (dolist (key (hash-keys ht))
    (let ((value (gethash key ht)))
      ;; modified key implies potentially modified hash-value
      (remhash key ht)
      (setf (gethash (force key) ht)
            (force value))))
  ht)

(defun force-all-thunks ()
  (mapc #'force (find-all (find-class 'prevalence-object))))

;;; Miscellaneous

(defun instance->make-instance-form (instance)
  (let (initargs value-maps)
    (do-bound-slots (slotd instance :name slot-name :value slot-value)
      (let ((serialized-slot-value (serialize-object slot-value)))
        (prependf value-maps `(',slot-name ,serialized-slot-value))
        (lwhen (initarg (car (c2mop:slot-definition-initargs slotd)))
          (prependf initargs `(,initarg ,serialized-slot-value)))))
    `(instancify
      (list ',(class-name (class-of instance)) ,@initargs)
      ,@value-maps)))

(defun instancify (instance-args &rest pairs)
  (assert (evenp (length pairs)))
  (prog1-let (instance (apply #'make-instance instance-args))
    (loop for (slot-name value) on pairs by #'cddr
          do (setf (slot-value instance slot-name) value))))

(defun hashify (hash-args &rest pairs)
  (assert (evenp (length pairs)))
  (prog1-let (ht (apply #'make-hash-table hash-args))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key ht) value))))

(defun arrayify (array-args &rest elts)
  (prog1-let (array (apply #'make-array array-args))
    (loop for i from 0
          for elt in elts
          do (setf (row-major-aref array i) elt))))

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
  (if (and (boundp symbol)
           (eq symbol (symbol-value symbol)))
      symbol
      `',symbol))

(defmethod serialize-object ((character character))
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
  ;; See the following link for discussion on different list types that need special handling:
  ;; https://stackoverflow.com/questions/60247877/check-for-proper-list-in-common-lisp
  (cond ((null list) nil)
        ((null (cdr (last list))) `(list ,@(mapcar #'serialize-object list)))
        (t `(cons ,(serialize-object (car list))
                  ,(serialize-object (cdr list))))))

(defmethod serialize-object ((array array))
  (let ((fill-pointer (ignore-errors (fill-pointer array))))
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
  `(hashify
    (list
     :test ,`(function ,(hash-table-test ht))
     :size ,(hash-table-size ht)
     :rehash-size ,(hash-table-rehash-size ht)
     :rehash-threshold ,(hash-table-rehash-threshold ht))
    ,@(loop for key being the hash-keys of ht
            collect (serialize-object key)
            collect (serialize-object (gethash key ht)))))

(defmethod serialize-object ((class standard-class))
  (assert (class-name class))
  `(find-class ',(class-name class)))

;;;; 2. MOPy Action Serialization

(defgeneric serialize (generic &key &allow-other-keys)
  (:documentation "keyword params per method generally match GENERIC."))

(defmethod serialize :around (generic &key &allow-other-keys)
  (when *persisting-p*
    (prog1-let (form (call-next-method))
      (serialize-write form))))

(defmethod serialize ((generic (eql :slot-makunbound-using-class))
                      &key class object slotd)
  `(c2mop:slot-makunbound-using-class ,(serialize-object class)
                                      ,(serialize-object object)
                                      (slot-by-name ,(serialize-object class)
                                                    ',(c2mop:slot-definition-name slotd))))

(defmethod serialize ((generic (eql :setf-slot-value-using-class))
                      &key new-value class instance slotd)
  `(setf (c2mop:slot-value-using-class ,(serialize-object class)
                                       ,(serialize-object instance)
                                       (slot-by-name ,(serialize-object class)
                                                     ',(c2mop:slot-definition-name slotd)))
         ,(serialize-object new-value)))

(defmethod serialize ((generic (eql :make-instance))
                      &key instance)
  (instance->make-instance-form instance))

(defparameter *e-c-u-m-args-filter*
  #+sbcl
  '(sb-pcl::safe-p sb-pcl::source)
  #-sbcl
  nil)

(defparameter *e-c-u-m-slots-filter*
  #+sbcl
  '(sb-pcl::source)
  #-sbcl
  nil)

(defun filter-e-c-u-m-args (args)
  (let* ((args (remove-properties *e-c-u-m-args-filter* args))
         (slot-arg (mapcar (lfix #'remove-properties *e-c-u-m-slots-filter*)
                           (getf args :direct-slots))))
    (replace-property :direct-slots slot-arg args)))

(defun serialize-e-c-u-m-args (args)
  (let* ((most-args (mapcar #'serialize-object
                            (replace-property :direct-slots nil args)))
         (slot-args
           `(list ,@(mapcar #'serialize-slot-spec
                            (getf args :direct-slots)))))
    (replace-property :direct-slots slot-args most-args)))

(defun serialize-slot-spec (slot-spec)
  (apply #'append '(list)
         (loop for (prop value) on slot-spec by #'cddr
               collect (serialize-slot-spec-prop prop value slot-spec))))

(defun serialize-slot-spec-prop (prop value slot-spec)
  (list prop (if (eq :initfunction prop)
                 `(lambda () ,(getf slot-spec :initform))
                 (serialize-object value))))

(defmethod serialize ((generic (eql :ensure-class-using-metaclass))
                      &key name args)
  ;; Serialize as ensure-class since that handles whether class exists.
  (let ((args (filter-e-c-u-m-args args)))
    `(c2mop:ensure-class
      ,(serialize-object name)
      ,@(serialize-e-c-u-m-args args))))

(defmethod serialize ((generic (eql :change-class))
                      &key instance new-class initargs)
  `(change-class ,(serialize-object instance)
                 ,(serialize-object new-class)
                 ,@(mapcar #'serialize-object initargs)))

(defmethod serialize ((generic (eql :reinitialize-instance))
                      &key instance initargs)
  `(reinitialize-instance ,(serialize-object instance)
                          ,@(mapcar #'serialize-object initargs)))
;;;; 3. IO

(defun serialize-write (form)
  (bt:with-lock-held (*serialization-lock*)
    (with-open-file (out (if *saving-world-p* ; can stuff stream into a global to save on opens/closes
                             (world-file *prevalence-system*)
                             (log-file *prevalence-system*))
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (let ((*package* (find-package :spiclum)))
        (format out "~S~%" form)
        ;; gives durability dependent on the lisp implementation treats  finish-output
        (finish-output out)))))
