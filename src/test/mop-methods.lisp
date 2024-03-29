(in-package :spiclum)

(5am:def-suite :prevalence.unit.mop-methods :in :prevalence.unit)

(5am:in-suite :prevalence.unit.mop-methods)

(5am:test :find-slot-defining-class-finds-expected-classes
  (destructuring-bind (top left right bottom) (hierarchy)
    (flet ((check-findings (slot-defining-class slot-name expected)
             (declare (ignore slot-defining-class))
             (5am:is (equal expected
                            (mapcar (lambda (class)
                                      (let ((slotd (slot-by-name class slot-name)))
                                        (when slotd
                                          (find-slot-defining-class class slotd))))
                                    (hierarchy))))))
      (check-findings top    'pu-top   (list top  top   top   top))
      (check-findings top    'i-top    (list top  top   top   top))
      (check-findings top    'cu-top   (list top  top   top   top))
      (check-findings top    'nil-top  (list top  top   top   top))
      (check-findings left   'pu-left  (list nil  left  nil   left))
      (check-findings left   'middle   (list nil  left  right left))
      (check-findings right  'pu-right (list nil  nil   right right))
      (check-findings right  'middle   (list nil  left  right left))
      (check-findings bottom 'i-bottom (list nil  nil   nil   bottom)))))

(5am:test :prevalence-slot-locks-finds-locks-with-expected-names
  (let* ((*prevalence-system* (test-prevalence-system))
         (bottom (find-class 'bottom)))
    (destructuring-bind (pu-top-lock middle-lock i-bottom-lock)
        (prevalence-slot-locks bottom (mapcar (lfix #'slot-by-name bottom)
                                              '(pu-top middle i-bottom)))
      (5am:is-true (search "TOP PU-TOP"      (lock-name pu-top-lock)))
      (5am:is-true (search "LEFT MIDDLE"     (lock-name middle-lock)))
      (5am:is-true (search "BOTTOM I-BOTTOM" (lock-name i-bottom-lock))))))

(5am:test :prevalence-lookup-class-slot-locates-expected-object
  (with-fixture-system (sample-bottom-1 sample-bottom-2)
    (check-lookup-finds-object sample-bottom-1)
    (check-lookup-finds-object sample-bottom-2)))

(5am:test :setfing-index-keyable-slot-updates-lookups-as-expected
  (with-fixture-system (sb1 sb2)
    (let ((*persisting-p* nil)
          (bottom (find-class 'bottom)))
      (setf (i-bottom sb1) 6)
      (5am:is-true (member sb1
                           (prevalence-lookup-class-slot
                            bottom (slot-by-name bottom 'i-bottom) 6))
                   "sb1 not found under 6 after (setf (i-bottom sb1) 6)")
      (5am:is-true (member sb2
                           (prevalence-lookup-class-slot
                            bottom (slot-by-name bottom 'i-bottom) 5))
                   "sb2 not remaining under 5 after (setf (i-bottom sb1) 6)")
      (5am:is-false (member sb1
                            (prevalence-lookup-class-slot
                             bottom (slot-by-name bottom 'i-bottom) 5))
                    "sb1 remains under 5 after (setf (i-bottom sb1) 6)")
      (setf (i-bottom sb2) 6)
      (5am:is-true (member sb2
                           (prevalence-lookup-class-slot
                            bottom (slot-by-name bottom 'i-bottom) 6))
                   "sb2 not found under 6 after (setf (i-bottom sb2) 6)")
      (5am:is-true (member sb1
                           (prevalence-lookup-class-slot
                            bottom (slot-by-name bottom 'i-bottom) 6))
                   "sb2 not remaining under 6 after (setf (i-bottom sb2) 6)")
      (5am:is-false (member sb2
                            (prevalence-lookup-class-slot
                             bottom (slot-by-name bottom 'i-bottom) 5))
                    "sb2 remains under 5 after (setf (i-bottom sb2) 6)"))))

(5am:test :setfing-class-unique-keyable-slot-updates-lookups-as-expected
  (with-fixture-system (sb1 _)
    (let ((*persisting-p* nil)
          (bottom (find-class 'bottom)))
      (setf (cu-top sb1) 'big-oof)
      (5am:is-true (eq sb1 (prevalence-lookup-class-slot
                            bottom (slot-by-name bottom 'cu-top) 'big-oof))
                   "sb1 not found under 'big-oof after (setf (cu-top sb1) 'big-oof)")
      (5am:is-false (prevalence-lookup-class-slot
                     bottom (slot-by-name bottom 'cu-top) 'hmm)
                    "something found under 'hmm after (setf (cu-top sb1) 'big-oof)"))))

(5am:test :setfing-precedence-unique-keyable-slot-updates-lookups-as-expected
  (with-fixture-system (sb1 _)
    (let ((*persisting-p* nil)
          (top (find-class 'top)))
      (setf (pu-top sb1) "didgeridoo")
      (5am:is-true (eq sb1 (prevalence-lookup-class-slot
                            top (slot-by-name top 'pu-top) "didgeridoo"))
                   "sb1 not found under \"didgeridoo\" after (setf (pu-top sb1) \"didgeridoo\")")
      (5am:is-false (prevalence-lookup-class-slot
                     top (slot-by-name top 'pu-top) "top")
                    "something found under \"top\" after (setf (pu-top sb1) \"didgeridoo\")"))))

(5am:test :setfing-unique-keyable-slot-throws-appropriate-error1
  (with-fixture-system (sb1 _)
    (let ((*persisting-p* nil))
      (handler-case
          (progn
            (setf (cu-top sb1) 'nei)
            (5am:fail "(setf (cu-top sb1) 'nei) didn't throw expected error!"))
        (non-unique-unique-keys ()
          (5am:pass "(setf (cu-top sb1) 'nei) threw appropriate error, as expected")))
      (handler-case
          (progn
            (setf (pu-top sb1) "tip")
            (5am:fail "(setf (pu-top sb1) \"tip\") didn't throw expected error!"))
        (non-unique-unique-keys ()
          (5am:pass "(setf (pu-top sb1) \"tip\") threw appropriate error, as expected"))))))

(5am:test :setf-to-equality-value-as-before-does-not-interfere
  (with-fixture-system (sb1 _)
    (let ((*persisting-p* nil))
      (dolist (slotd (c2mop:class-slots (class-of sb1)))
        (setf (slot-value sb1 (c2mop:slot-definition-name slotd))
              (slot-value sb1 (c2mop:slot-definition-name slotd))))
      (check-lookup-finds-object sb1))))

(5am:test :make-instance-correctly-inserts-into-prevalence-system
  (with-fixture-system (_1 _2)
    (let* ((*persisting-p* nil)
           (plist *sample-bottom-unoccupied-plist*)
           (bottom (apply #'make-instance 'bottom plist)))
      (check-lookup-finds-object bottom))))

(5am:test :make-instance-with-occupied-values-throws-appropriate-error
  'todo)

(5am:test :reinitialize-instance-correctly-updates-lookups
  (with-fixture-system (sb _)
    (flet ((slot->value-map (obj)
             (mapcar (lambda (slotd)
                       (cons slotd (slot-value sb (c2mop:slot-definition-name slotd))))
                     (c2mop:class-slots (class-of obj)))))
      (let* ((*persisting-p* nil)
             (old-values (slot->value-map sb)))
        (apply #'reinitialize-instance sb (nthcdr 4 *sample-bottom-unoccupied-plist*))
        (5am:is-false (equalp old-values
                              (slot->value-map sb)))
        (check-lookup-finds-object sb)

        (dolist (slotd->value old-values)
          (destructuring-bind (slotd . value) slotd->value
            (when (and (key slotd)
                       (not (funcall (equality slotd)
                                     value
                                     (slot-value sb (c2mop:slot-definition-name slotd)))))
              (5am:is-false (check-lookup-finds-object-slotd-value-p sb slotd value)))))))))

;; Three tests for change-class: One to make sure can be found in appropriate new lookups

(5am:test :change-class-correctly-updates-lookups
  (destructuring-bind (top left right bottom) (hierarchy)
    (declare (ignorable top right bottom))
    (let* ((*prevalence-system* (test-prevalence-system))
           (obj (make-instance 'left :pu-left "initial-pu-left" :middle 'tja :pu-top 5
                                     :i-top 19.0d0 :cu-top 'well :nil-top "didgeridoo"))
           (class/slotd/value-map (list (list (find-slot-defining-class left (slot-by-name left 'pu-left))
                                              (slot-by-name left 'pu-left)
                                              (slot-value obj 'pu-left))
                                        (list (find-slot-defining-class left (slot-by-name left 'middle))
                                              (slot-by-name left 'middle)
                                              (slot-value obj 'middle)))))
      (check-lookup-finds-object obj)
      (change-class obj 'right :pu-right "initial-pu-right")
      (check-lookup-finds-object obj)
      (dolist (mapping class/slotd/value-map)
        (5am:is-false (apply #'prevalence-lookup-class-slot mapping))))))


(5am:test :change-class-correctly-restores-instance-on-error
  (destructuring-bind (top left right bottom) (hierarchy)
    (declare (ignorable top bottom))
    (let* ((*prevalence-system* (test-prevalence-system))
           (left-obj  (make-instance 'left  :pu-left  "leftie" :middle 'tja))
           (right-obj (make-instance 'right :pu-right "right"  :middle 'tja)))
      (handler-case
          (progn (change-class left-obj 'right :middle 'tja)
                 (5am:fail "change-class call didn't throw expected error"))
        (error (error)
          (5am:is (eq 'non-unique-unique-keys
                      (type-of error)))
          (5am:is (eq right-obj
                      (prevalence-lookup-class-slot right (slot-by-name right 'middle) 'tja)))
          (5am:is (eq left-obj
                      (prevalence-lookup-class-slot left (slot-by-name left 'middle) 'tja)))
          (5am:is (eq left-obj
                      (prevalence-lookup-class-slot left (slot-by-name left 'pu-left) "leftie"))))))))

;;;; Let's test class (re)definition

(5am:test :class-definition-registers-in-prevalence-system
  (let ((*prevalence-system* (test-prevalence-system)))
    (ignore-errors (setf (find-class 'class-definition-test-class) nil))
    (5am:is (zerop (hash-table-count (class-definition-store *prevalence-system*))))
    (eval '(defpclass class-definition-test-class ()
            (a b c)))
    (5am:is (plusp (hash-table-count (class-definition-store *prevalence-system*))))
    (destructuring-bind (name &key direct-slots &allow-other-keys)
        (gethash 'class-definition-test-class (class-definition-store *prevalence-system*))
      (declare (ignorable name))
      (5am:is (null (set-exclusive-or
                     '(a b c)
                     (mapcar (rfix #'getf :name) direct-slots)))))
    (eval '(defpclass class-definition-test-class ()
            (a b c d e)))
    (destructuring-bind (name &key direct-slots &allow-other-keys)
        (gethash 'class-definition-test-class (class-definition-store *prevalence-system*))
      (declare (ignorable name))
      (5am:is (null (set-exclusive-or
                     '(a b c d e)
                     (mapcar (rfix #'getf :name) direct-slots)))))))

(5am:test :class-definition-updates-object-store-indexes
  (let ((*prevalence-system* (test-prevalence-system)))
    (ignore-errors (setf (find-class 'class-definition-test-class) nil))
    (5am:is (zerop (hash-table-count (class-definition-store *prevalence-system*))))
    (eval '(defpclass class-definition-test-class ()
            ((a :initarg :a :key :index :equality #'equalp)
             (b :initarg :b :key :unique :equality #'equalp)
             (c :initarg :c :key :class-unique :equality #'equalp))))
    (let* ((class (find-class 'class-definition-test-class))
           (instance (make-instance 'class-definition-test-class
                                    :a :a :b :b :c :c))
           (a-slot (slot-by-name class 'a)))
      (check-lookup-finds-object instance)
      (eval '(defpclass class-definition-test-class ()
              ((b :initarg :b :key :unique :equality #'equalp)
               (c :initarg :c :key :class-unique :equality #'equalp)
               (oof :initarg :oof :key :index :equality #'equalp))))
      (check-lookup-finds-object instance)
      (5am:is (null (prevalence-lookup-class-slot
                     class a-slot :a))))))


(5am:test :class-definition-errors-on-index-collision
  (let ((*prevalence-system* (test-prevalence-system)))
    (ignore-errors (setf (find-class 'class-definition-test-class) nil))
    (5am:is (zerop (hash-table-count (class-definition-store *prevalence-system*))))
    (eval '(defpclass class-definition-test-class ()
            ((a :initarg :a))))
    (5am:is (plusp (hash-table-count (class-definition-store *prevalence-system*))))
    (let ((i1 (make-instance 'class-definition-test-class :a 5))
          (i2 (make-instance 'class-definition-test-class :a 5))
          (the-class (find-class 'class-definition-test-class)))
      (check-lookup-finds-object i1)
      (check-lookup-finds-object i2)
      (5am:is (= 2
                 (length
                  (find-all
                   (find-class 'class-definition-test-class)))))
      (handler-case
          (progn
            (eval '(defpclass class-definition-test-class ()
                    ((a :initarg :a
                        :key :unique
                        :equality #'eql))))
            (5am:fail "Expected non-unique-unique keys error, but no error happened."))
        (non-unique-unique-keys ()
          (5am:pass "class redefinition resulted in expected error.")))
      (5am:is (eq the-class
                  (find-class 'class-definition-test-class)))
      (5am:is (null (key
                     (car
                      (c2mop:class-direct-slots
                       (find-class 'class-definition-test-class))))))
      (5am:is (= 2
                 (length
                  (find-all
                   (find-class 'class-definition-test-class))))))))

(5am:test :class-redefinition-works-with-inheritance
  (let ((*prevalence-system* (test-prevalence-system)))
    (ignore-errors (setf (find-class 'a-class) nil))
    (ignore-errors (setf (find-class 'b-class) nil))
    (eval '(defpclass a-class ()
            ((a :initarg :a :key :index :equality #'equalp))))
    (eval '(defpclass b-class (a-class)
            ((b :initarg :b :key :unique :equality #'equalp))))
    (let ((original-a-definition (gethash 'a-class (class-definition-store *prevalence-system*)))
          (i1 (make-instance 'b-class :a 5 :b 2))
          (i2 (make-instance 'b-class :a 5 :b 8)))
      (check-lookup-finds-object i1)
      (check-lookup-finds-object i2)
      (5am:is (= 2
                 (length
                  (find-all
                   (find-class 'a-class)))))
      (5am:is (= 2
                 (length
                  (find-all
                   (find-class 'b-class)))))
      (handler-case
          (progn
            (eval '(defpclass a-class ()
                    ((a :initarg :a :key :unique :equality #'equalp))))
            (5am:fail "Expected non-unique-unique keys error, but no error happened."))
        (non-unique-unique-keys ()
          (5am:pass "class redefinition resulted in expected error.")))
      (5am:is (eq original-a-definition
                  (gethash 'a-class (class-definition-store *prevalence-system*))))
      (5am:is (= 2
                 (length
                  (find-all
                   (find-class 'a-class)))))
      (5am:is (= 2
                 (length
                  (find-all
                   (find-class 'b-class)))))
      (check-lookup-finds-object i1)
      (check-lookup-finds-object i2)
      (setf (slot-value i1 'a) 12)
      (eval '(defpclass a-class ()
              ((a :initarg :a :key :unique :equality #'equalp))))
      (check-lookup-finds-object i1)
      (check-lookup-finds-object i2))))

(5am:test :slot-makunbound-using-class-updates-lookups
  (with-fixture-system (sb _)
    (let ((slotd (slot-by-name (class-of sb) 'pu-left))
          (value (pu-left sb)))
      (5am:is-true (check-lookup-finds-object-slotd-value-p sb slotd value))
      (slot-makunbound sb 'pu-left)
      (5am:is-false (check-lookup-finds-object-slotd-value-p sb slotd value)))))
