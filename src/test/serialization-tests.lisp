(in-package :spiclum)

(5am:def-suite :prevalence.unit.serialization :in :prevalence.unit)

(5am:in-suite :prevalence.unit.serialization)

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

(defmacro call-tests () ())

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
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (apply #'make-instance 'bottom
                        *sample-bottom-unoccupied-plist*))
         (call (list 'c2mop:slot-makunbound-using-class
                     (find-class 'bottom)
                     bottom
                     (slot-by-name (find-class 'bottom) 'i-bottom))))
    (destructuring-bind (fn-name class object slotd) call
      (declare (ignore fn-name))
      (let ((serialized
              (key-args (class object slotd) serialize :slot-makunbound-using-class)))
        (5am:is (call-equal call serialized))))))

(5am:test :setf-slot-value-using-class-serialization
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (apply #'make-instance 'bottom
                        *sample-bottom-unoccupied-plist*))
         (call `(setf (c2mop:slot-value-using-class
                       ,(class-of bottom)
                       ,bottom
                       ,(slot-by-name (class-of bottom) 'nil-top))
                      christmas-pudding)))
    (destructuring-bind (setf (fn-name class instance slotd) new-value) call
      (declare (ignore setf fn-name))
      (let ((serialized (key-args (new-value class instance slotd)
                                  serialize :setf-slot-value-using-class)))
        (5am:is (call-equal call serialized))))))

(5am:test :change-class-serialization
  (let* ((*prevalence-system* (test-prevalence-system))
         (instance (make-instance 'left :pu-left "i-pu-left"
                                        :middle 'tja
                                        :i-top 19.0d0))
         (call `(change-class ,instance
                              ,(find-class 'right)
                              :pu-right "i-pu-right")))
    (destructuring-bind (fn-name instance new-class &rest initargs) call
      (declare (ignore fn-name))
      (let ((serialized (key-args (instance new-class initargs)
                                  serialize :change-class)))
        (5am:is (call-equal call serialized))))))

;; ensure-class-using-metaclass

;; reinitialize-instance
