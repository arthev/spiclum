(in-package :spiclum)

(5am:def-suite :prevalence.unit.atomics :in :prevalence.unit)

(5am:in-suite :prevalence.unit.atomics)

(5am:test :exercise-multi-setf
  (with-fixture-system (sb1 sb2)
    (let ((x 'initialized) (y (list 1 2 3)) (z '(a b c)))
      (multi-setf ()
        (i-bottom sb1) 6
        x              'resetfed
        (middle sb2)   'bowzinga
        (slot-value sb2 'pu-top) 'tap
        x              'cowabunga
        (c2mop:slot-value-using-class (class-of sb1)
                                      sb1
                                      (slot-by-name (class-of sb1)
                                                    'nil-top))
                       nil
        (cadr y)       5
        z              (cons 'e z)
        z              (cons 'f z))
      (5am:is (eq 'cowabunga x))
      (5am:is (eq 'bowzinga (middle sb2)))
      (5am:is-true (check-lookup-finds-object-slotd-value-p
                    sb1 (slot-by-name (class-of sb1) 'i-bottom) 6))
      (5am:is (eq 'tap (pu-top sb2)))
      (5am:is (eq nil (nil-top sb1)))
      (5am:is (equal '(1 5 3) y))
      (5am:is (equal '(f e a b c) z)))))

(5am:test :multi-setf-consistent-with-setf-for-empty-body
  (5am:is (eq nil (multi-setf ()))))

(5am:test :multi-setf-unrolls-correctly
  (with-fixture-system (sb1 sb2)
    (let ((x 'initialized) (y (list 1 2 3)))
      (handler-case
          (multi-setf ()
            (i-bottom sb1) 6
            x              'resetfed
            (middle sb2)   'bowzinga
            (slot-value sb2 'pu-top) 'tap
            x              'cowabunga
            (c2mop:slot-value-using-class (class-of sb1)
                                          sb1
                                          (slot-by-name (class-of sb1)
                                                        'nil-top))
                           nil
            (cadr y)       5
            (middle sb1)   'bowzinga)
        (non-unique-unique-keys ()
          (5am:pass "Middle collision generated expected condition"))
        (error (e)
          (5am:fail "Unexpected error: ~A" e)))
      (5am:is (eq 'initialized x))
      (5am:is (eq 'neida (middle sb2)))
      (5am:is-true (check-lookup-finds-object-slotd-value-p
                    sb1 (slot-by-name (class-of sb1) 'i-bottom) 5))
      (5am:is (string= "tip" (pu-top sb2)))
      (5am:is (= 55 (nil-top sb1)))
      (5am:is (equal '(1 2 3) y)))))

(5am:test :multi-setf-locking-checks
  (with-fixture-system (sb1 sb2)
    (let* ((x 'initialized)
           (y (list 1 2 3))
           (extra-lock (bt:make-lock "test-lock"))
           (expected-locks (cons extra-lock
                                 (prevalence-slot-locks
                                  (find-class 'bottom)
                                  (mapcar (lfix #'slot-by-name
                                                (find-class 'bottom))
                                          '(pu-right
                                            i-bottom
                                            middle
                                            pu-top
                                            cu-top))))))
      (cl-mock:with-mocks ()
        (cl-mock:register-mock 'call-with-recursive-locks)
        (multi-setf (:locks (list extra-lock)
                     :indirects ((pu-right sb1)))
          (i-bottom sb1) 6
          (middle sb2)   'bowzinga
          (slot-value sb2 'pu-top) 'tap
          x              'cowabunga
          (c2mop:slot-value-using-class (class-of sb1)
                                        sb1
                                        (slot-by-name (class-of sb1)
                                                      'cu-top))
                         'jaja
          (cadr y)       5)
        (let* ((invocations (cl-mock:invocations 'call-with-recursive-locks))
               (invoked-locks (cadar invocations)))
          (5am:is (= 1 (length invocations)))
          (5am:is (eq nil
                      (union
                       (set-difference expected-locks invoked-locks)
                       (set-difference invoked-locks expected-locks)))))))))

(5am:test :exercise-multi-psetf
  (with-fixture-system (sb1 sb2)
    (let ((x 'initialized) (y (list 1 2 3)) (z '(a b c)))
      (multi-psetf ()
        (i-bottom sb1) 6
        x              'resetfed
        (middle sb2)   'bowzinga
        (slot-value sb2 'pu-top) 'tap
        x              'cowabunga
        (c2mop:slot-value-using-class (class-of sb1)
                                      sb1
                                      (slot-by-name (class-of sb1)
                                                    'nil-top))
                       nil
        (cadr y)       5
        z              (cons 'e z)
        z              (cons 'f z))
      (5am:is (eq 'cowabunga x))
      (5am:is (eq 'bowzinga (middle sb2)))
      (5am:is-true (check-lookup-finds-object-slotd-value-p
                    sb1 (slot-by-name (class-of sb1) 'i-bottom) 6))
      (5am:is (eq 'tap (pu-top sb2)))
      (5am:is (eq nil (nil-top sb1)))
      (5am:is (equal '(1 5 3) y))
      (5am:is (equal '(f a b c) z)))))

(5am:test :multi-psetf-consistent-with-psetf-for-empty-body
  (5am:is (eq nil (multi-psetf ()))))

(5am:test :multi-psetf-unrolls-correctly
  (with-fixture-system (sb1 sb2)
    (let ((x 'initialized) (y (list 1 2 3)))
      (handler-case
          (multi-psetf ()
            (i-bottom sb1) 6
            x              'resetfed
            (middle sb2)   'bowzinga
            (slot-value sb2 'pu-top) 'tap
            x              'cowabunga
            (c2mop:slot-value-using-class (class-of sb1)
                                          sb1
                                          (slot-by-name (class-of sb1)
                                                        'nil-top))
                           nil
            (cadr y)       5
            (middle sb1)   'bowzinga)
        (non-unique-unique-keys ()
          (5am:pass "Middle collision generated expected condition"))
        (error (e)
          (5am:fail "Unexpected error: ~A" e)))
      (5am:is (eq 'initialized x))
      (5am:is (eq 'neida (middle sb2)))
      (5am:is-true (check-lookup-finds-object-slotd-value-p
                    sb1 (slot-by-name (class-of sb1) 'i-bottom) 5))
      (5am:is (string= "tip" (pu-top sb2)))
      (5am:is (= 55 (nil-top sb1)))
      (5am:is (equal '(1 2 3) y)))))

(5am:test :multi-psetf-locking-checks
  (with-fixture-system (sb1 sb2)
    (let* ((x 'initialized)
           (y (list 1 2 3))
           (extra-lock (bt:make-lock "test-lock"))
           (expected-locks (cons extra-lock
                                 (prevalence-slot-locks
                                  (find-class 'bottom)
                                  (mapcar (lfix #'slot-by-name
                                                (find-class 'bottom))
                                          '(pu-right
                                            i-bottom
                                            middle
                                            pu-top
                                            cu-top))))))
      (cl-mock:with-mocks ()
        (cl-mock:register-mock 'call-with-recursive-locks)
        (multi-psetf (:locks (list extra-lock)
                     :indirects ((pu-right sb1)))
          (i-bottom sb1) 6
          (middle sb2)   'bowzinga
          (slot-value sb2 'pu-top) 'tap
          x              'cowabunga
          (c2mop:slot-value-using-class (class-of sb1)
                                        sb1
                                        (slot-by-name (class-of sb1)
                                                      'cu-top))
                         'jaja
          (cadr y)       5)
        (let* ((invocations (cl-mock:invocations 'call-with-recursive-locks))
               (invoked-locks (cadar invocations)))
          (5am:is (= 1 (length invocations)))
          (5am:is (eq nil
                      (union
                       (set-difference expected-locks invoked-locks)
                       (set-difference invoked-locks expected-locks)))))))))
