(in-package :spiclum)

(5am:def-suite :prevalence.unit.queries :in :prevalence.unit)

(5am:in-suite :prevalence.unit.queries)

(5am:test :query-test-single-simple-filter
  (with-fixture-system (sb1 sb2)
    (let ((query-results
            (query :class bottom
                   :where (i-bottom 5))))
      (5am:is-true (find sb1 query-results))
      (5am:is-true (find sb2 query-results)))))

(5am:test :query-test-compound-filter
  (with-fixture-system (sb1 sb2)
    (let ((query-results
            (query :class bottom
                   :where (:and (i-bottom 5)
                                (pu-left "hmm")))))
      (5am:is-true (find sb1 query-results))
      (5am:is-false (find sb2 query-results)))
    (let ((query-results
            (query :class bottom
                   :where (:or (middle 'oida)
                               (middle 'neida)))))
      (5am:is-true (find sb1 query-results))
      (5am:is-true (find sb2 query-results)))
    (let ((query-results
            (query :class bottom
                   :where (:not (middle 'oida)))))
      (5am:is-false (find sb1 query-results))
      (5am:is-true (find sb2 query-results)))
    (let ((query-results
            (query :class bottom
                   :where (:not (:or (:and (i-bottom 5)
                                           (pu-left "hmm"))
                                     (:not (middle 'oida)))))))
      (5am:is (eq nil query-results)))))

(5am:test :query-test-select-the
  (with-fixture-system (sb1 sb2)
    (5am:is (eq sb1
                (query :select :the
                       :class bottom
                       :where (:and (i-bottom 5)
                                    (pu-left "hmm")))))))

(5am:test :query-test-no-filter
  (with-fixture-system (sb1 sb2)
    (let ((query-results
            (query :class bottom)))
      (5am:is-true (find sb1 query-results))
      (5am:is-true (find sb2 query-results)))))

(5am:test :query-test-strict-option
  (with-fixture-system (sb1 sb2)
    (let* ((t1 (make-instance 'top
                              :i-top 19
                              :pu-top "typ"
                              :cu-top :moo
                              :nil-top 555))
           (query-results-strict
             (query :class top :strict t :where (i-top 19)))
           (query-results
             (query :class top :where (i-top 19))))
      (5am:is (equalp (list t1)
                      query-results-strict))
      (5am:is (= 3 (length query-results)))
      (5am:is-true (subsetp query-results (list t1 sb1 sb2))))))

(5am:test :query-test-function-filter
  (with-fixture-system (sb1 sb2)
    (let ((query-results
            (query :class bottom
                   :where (i-bottom (lfix #'= 5)))))
      (5am:is-true (find sb1 query-results))
      (5am:is-true (find sb2 query-results)))))
