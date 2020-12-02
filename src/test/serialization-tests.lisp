(in-package :spiclum)

(5am:def-suite :prevalence.unit.serialization :in :prevalence.unit)

(5am:in-suite :prevalence.unit.serialization)

;;;; Utils

(defmacro data-tests (obj &optional (comp '#'equalp))
  `(progn
     (5am:is (not (eq ,obj (serialize-object ,obj))))
     (5am:is (funcall ,comp ,obj (eval (serialize-object ,obj))))))

(defun instance-slot-equal (slotd instance1 instance2 &key (test #'eql))
  (let ((slot-name (c2mop:slot-definition-name slotd)))
    (or (not (or (slot-boundp instance1 slot-name)
                 (slot-boundp instance2 slot-name)))
        (and (slot-boundp instance1 slot-name)
             (slot-boundp instance2 slot-name)
             (funcall test
                      (slot-value instance1 slot-name)
                      (slot-value instance2 slot-name))))))

(defun instance-equal (instance1 instance2 &key (test #'eql))
  (and (eq (class-of instance1) (class-of instance2))
       (every (rfix #'instance-slot-equal instance1 instance2 :test test)
              (c2mop:class-slots (class-of instance1)))))

(defun analogue-call-data (original serialized)
  (if (symbolp original)
      (or (eq original serialized)
          (and (listp serialized)
               (eq 'quote (car serialized))
               (eq original (eval serialized))))
      (let ((serialized (eval serialized)))
        (funcall (typecase original
                   (prevalence-object #'eq)
                   (standard-class #'eq)
                   (string #'string=)
                   (array #'equalp)
                   (hash-table #'equalp)
                   (number #'eql)
                   (character #'eql)
                   (random-state #'equalp)
                   (c2mop:standard-slot-definition #'slot-name-equivalence)
                   (t (ignore-args (constantly nil))))
                 original serialized))))

(defun call-equal (original serialized)
  ;; This is similar to tree-equal, except one-sided.
  ;; Rely on ORIGINAL's structure rather than demanding same leaves.
  (if (atom original)
      (analogue-call-data original serialized)
      (and (call-equal (car original) (car serialized))
           (call-equal (cdr original) (cdr serialized)))))

(defmacro call-tests (&key instance call destructuring-lambda generic)
  (let* ((relevant-method
           (find-if (lambda (specializers)
                      (let ((generic-specializer (car specializers)))
                        (and (typep generic-specializer
                                    'c2mop:eql-specializer)
                             (eq generic
                                 (c2mop:eql-specializer-object
                                  generic-specializer)))))
                    (c2mop:generic-function-methods #'serialize)
                    :key #'c2mop:method-specializers))
         (relevant-lambda-list
           (c2mop:method-lambda-list relevant-method))
         (method-keyword-params
           (cdr (member '&key relevant-lambda-list)))
         (ignore
           (set-difference
            (flatten destructuring-lambda)
            (append '(&optional &rest &body &key &aux &whole &environment)
                    method-keyword-params))))
    `(let* ((*prevalence-system* (test-prevalence-system))
            (instance ,instance)
            (call ,call))
       (destructuring-bind ,destructuring-lambda call
         (declare (ignore ,@ignore))
         (let ((serialized (key-args ,method-keyword-params
                                     serialize ,generic)))
           (5am:is (call-equal call serialized)))))))


;;;; Tests

(5am:test :list-serialization
  (let ((list '(a b c 59 2.4 "hello" #(1 2 3))))
    (data-tests list))
  (5am:is (eq nil (eval (serialize-object nil)))))

(5am:test :tree-serialization
  (let ((tree '((((((() . ())))) . ())
                3 . (a b c (59) . (631 631 631 ("hello") . #("oi" "oy"))))))
    (data-tests tree)))

(5am:test :array-serialization
  (let ((array #(1 2 3)))
    (data-tests array)
    (let ((string (make-array 3 :element-type 'character
                                :adjustable t
                                :fill-pointer t)))
      (format string "hehehe")
      (data-tests string)
      (5am:is (string= string (eval (serialize-object string))))
      (let ((marray (make-array '(2 2))))
        (setf (aref marray 0 0) array
              (aref marray 0 1) string
              (aref marray 1 0) '(a b c "hmm" 3.3)
              (aref marray 1 1) 'sobering)
        (data-tests marray)))))

(5am:test :hash-table-serialization
  (let ((ht (make-hash-table :test #'equal))
        (data '(:a a
                :c c
                (5 . (symb . #\Space)) ("somebody toucha" . "ma spaghett")
                "yes" no)))
    (loop for (key value) on data by #'cddr
          do (setf (gethash key ht) value))
    (data-tests ht)))

(5am:test :make-instance-serialization
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (apply #'make-instance 'bottom
                        *sample-bottom-unoccupied-plist*))
         (*prevalence-system* (test-prevalence-system))
         (serialized (serialize :make-instance :instance bottom))
         (new-bottom (eval serialized)))
    (5am:is (not (eq bottom serialized)))
    (5am:is (not (eq bottom new-bottom)))
    (5am:is (instance-equal bottom new-bottom :test #'equalp))))

(5am:test :instance-as-lookup-serialization
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (apply #'make-instance 'bottom
                        *sample-bottom-unoccupied-plist*)))
    (data-tests bottom #'eq)))

(5am:test :slot-makunbound-using-class-serialization
  (call-tests :generic :slot-makunbound-using-class
              :instance (apply #'make-instance 'bottom
                               *sample-bottom-unoccupied-plist*)
              :call (list 'c2mop:slot-makunbound-using-class
                          (find-class 'bottom)
                          instance
                          (slot-by-name (find-class 'bottom) 'i-bottom))
              :destructuring-lambda (fn class object slotd)))

(5am:test :setf-slot-value-using-class-serialization
  (call-tests :generic :setf-slot-value-using-class
              :instance (apply #'make-instance 'bottom
                               *sample-bottom-unoccupied-plist*)
              :call (list 'setf
                          (list 'c2mop:slot-value-using-class
                                (class-of instance)
                                instance
                                (slot-by-name (class-of instance) 'nil-top))
                          'christmas-pudding)
              :destructuring-lambda (setf (fn class instance slotd) new-value)))

(5am:test :change-class-serialization
  (call-tests :generic :change-class
              :instance (make-instance 'left :pu-left "i-pu-left"
                                             :middle 'tja
                                             :i-top 19.0d0)
              :call (list 'change-class
                          instance
                          (find-class 'right)
                          :pu-right "i-pu-right")
              :destructuring-lambda (fn instance new-class &rest initargs)))

(5am:test :reinitialize-instance-serialization
  (call-tests :generic :reinitialize-instance
              :instance (apply #'make-instance 'bottom
                               *sample-bottom-unoccupied-plist*)
              :call (list 'reinitialize-instance
                          instance
                          :i-bottom 'yoodledy-moo)
              :destructuring-lambda (fn instance &rest initargs)))

(5am:test :ensure-class-using-metaclass-serialization
  (labels ((e-c-u-m-s-compare (original serialized)
             (cond ((functionp original)
                    (equalp (funcall original) (funcall serialized)))
                   ((atom original)
                    (equalp original serialized))
                   (t
                    (and (e-c-u-m-s-compare (car original) (car serialized))
                         (e-c-u-m-s-compare (cdr original) (cdr serialized)))))))
    (let* ((*prevalence-system* (test-prevalence-system))
           (class-def '(defpclass bottom (left right)
                        ((i-bottom
                          :initarg :i-bottom
                          :key :index
                          :equality #'equalp
                          :accessor i-bottom)
                         (some-slot
                          :initarg :some-slot
                          :accessor some-slot
                          :initform 42))))
           (call (progn
                   (eval class-def)
                   (list 'c2mop:ensure-class
                         'bottom
                         (filter-e-c-u-m-args
                          (cdr (last-class-definition 'bottom))))))
           (serialized (destructuring-bind (fn name args) call
                         (declare (ignore fn))
                         (key-args (name args) serialize :ensure-class-using-metaclass))))
      (5am:is (not (eq call serialized)))
      (5am:is (eq (car call) (car serialized)))
      (5am:is (eq (cadr call) (eval (cadr serialized))))
      (5am:is (e-c-u-m-s-compare (caddr call) (eval (caddr serialized)))))))
