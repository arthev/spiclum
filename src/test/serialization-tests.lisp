(in-package :spiclum)

(5am:def-suite :prevalence.unit.serialization :in :prevalence.unit)

(5am:in-suite :prevalence.unit.serialization)

(defmacro serialization-tests (obj &optional (comp 'equalp))
  `(progn
     (5am:is (not (eq ,obj (serialize-object ,obj))))
     (5am:is (,comp ,obj (eval (serialize-object ,obj))))))

(5am:test :list-serialization
  (let ((list '(a b c 59 2.4 "hello" #(1 2 3))))
    (serialization-tests list))
  (5am:is (eq nil (eval (serialize-object nil)))))

(5am:test :tree-serialization
  (let ((tree '((((((() . ())))) . ())
                3 . (a b c (59) . (631 631 631 ("hello") . #("oi" "oy"))))))
    (serialization-tests tree)))

(5am:test :array-serialization
  (let ((array #(1 2 3)))
    (serialization-tests array)
    (let ((string (make-array 3 :element-type 'character
                                :adjustable t
                                :fill-pointer t)))
      (format string "hehehe")
      (serialization-tests string)
      (5am:is (string= string (eval (serialize-object string))))
      (let ((marray (make-array '(2 2))))
        (setf (aref marray 0 0) array
              (aref marray 0 1) string
              (aref marray 1 0) '(a b c "hmm" 3.3)
              (aref marray 1 1) 'sobering)
        (serialization-tests marray)))))

(5am:test :hash-table-serialization
  (let ((ht (make-hash-table :test #'equal))
        (data '(:a a
                :c c
                (5 . (symb . #\Space)) ("somebody toucha" . "ma spaghett")
                "yes" no)))
    (loop for (key value) on data by #'cddr
          do (setf (gethash key ht) value))
    (serialization-tests ht)))

(5am:test :instance-as-lookup-serialization
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (apply #'make-instance 'bottom
                        *sample-bottom-unoccupied-plist*)))
    (serialization-tests bottom eq)))

(5am:test :slot-makunbound-using-class-serialization
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (apply #'make-instance 'bottom
                        *sample-bottom-unoccupied-plist*))
         (call (list 'c2mop:slot-makunbound-using-class
                     (find-class 'bottom)
                     bottom
                     (slot-by-name (find-class 'bottom) 'i-bottom))))
    (destructuring-bind (fn-name class object slotd) call
      (let ((serialized
              (key-args (class object slotd) serialize :slot-makunbound-using-class)))
        (5am:is (not (eq call serialized)))
        (5am:is (eq fn-name (car serialized)))
        (5am:is (equal (cdr call) (mapcar #'eval (cdr serialized))))))))

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
      (let ((serialized
              (key-args (new-value class instance slotd) serialize :setf-slot-value-using-class)))
        (5am:is (not (eq call serialized)))
        (5am:is (eq 'setf (car serialized)))
        (5am:is (eq fn-name (caadr serialized)))
        ;; and now we must check each value for eq of some type...
        ))))

;; setf-slot-value-using-class

;; make-instance

;; ensure-class-using-metaclass

;; change-class

;; reinitialize-instance
