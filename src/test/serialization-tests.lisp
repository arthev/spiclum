(in-package :spiclum)

(5am:def-suite :prevalence.unit.serialization :in :prevalence.unit)

(5am:in-suite :prevalence.unit.serialization)

(defmacro serialization-tests (obj)
  `(progn
     (5am:is (not (eq ,obj (serialize-object ,obj))))
     (5am:is (equalp ,obj (eval (serialize-object ,obj))))))

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

;; instance as lookup

;; slot-makunbound-using-class

;; setf-slot-value-using-class

;; make-instance

;; ensure-class-using-metaclass

;; change-class

;; reinitialize-instance
