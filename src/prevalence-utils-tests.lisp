(in-package :bmcg)

;;;; Class Hierarchy for Testing

(defclass top ()
  ((pu-top
    :initarg :pu-top
    :key :precedence-unique
    :equality #'equal
    :accessor pu-top)
   (i-top
    :initarg :i-top
    :key :index
    :equality #'equalp
    :accessor i-top)
   (cu-top
    :initarg :cu-top
    :key :class-unique
    :accessor cu-top)
   (nil-top
    :initarg :nil-top
    :accessor nil-top))
  (:metaclass prevalence-class))

(defclass left (top)
  ((pu-left
    :initarg :pu-left
    :key :precedence-unique
    :equality #'equal
    :accessor pu-left)
   (middle
    :initarg :middle
    :key :precedence-unique
    :accessor middle))
  (:metaclass prevalence-class))

(defclass right (top)
  ((pu-right
    :initarg :pu-right
    :key :precedence-unique
    :equality #'equal
    :accessor pu-right)
   (middle
    :initarg :middle
    :key :precedence-unique
    :accessor middle))
  (:metaclass prevalence-class))

(defclass bottom (left right)
  ((i-bottom
    :initarg :i-bottom
    :key :index
    :equality #'equalp
    :accessor i-bottom))
  (:metaclass prevalence-class))

(defun hierarchy ()
  (mapcar #'find-class '(top left right bottom)))

(dolist (class (hierarchy))
  (c2mop:finalize-inheritance class))

;;;; Fixtures

;; We might need functions to generate fresh sets of these (return multiple values)? for use in different tests
(with-ignored-prevalence
  (let* ((plist1 (list :i-bottom 5 :middle 'oida  :pu-left "hmm" :pu-right "rett" :pu-top "top" :i-top 19   :cu-top 'hmm :nil-top 55))
         (plist2 (list :i-bottom 5 :middle 'neida :pu-left "hum" :pu-right "righ" :pu-top "tip" :i-top 19.0 :cu-top 'nei :nil-top 55)))
    (defparameter *sample-bottom-1* (apply #'make-instance 'bottom plist1))
    (defparameter *sample-bottom-2* (apply #'make-instance 'bottom plist2))))

(defparameter *simple-sample-hash-store*
  (with-ignored-prevalence
    (let ((hash-store (make-hash-table)))
      (flet ((insert-obj (obj)
               (pushnew obj (gethash (i-bottom obj) (gethash 'i-bottom (gethash 'bottom hash-store))))
               (pushnew obj (gethash (i-top    obj) (gethash 'i-top    (gethash 'top    hash-store))))
               (setf (gethash (cu-top obj)   (gethash 'cu-top   (gethash (type-of obj) hash-store))) obj)
               (setf (gethash (pu-left obj)  (gethash 'pu-left  (gethash 'left         hash-store))) obj)
               (setf (gethash (pu-right obj) (gethash 'pu-right (gethash 'right        hash-store))) obj)
               (setf (gethash (middle  obj)  (gethash 'middle   (gethash 'left         hash-store))) obj)
               (setf (gethash (pu-top  obj)  (gethash 'pu-top   (gethash 'top          hash-store))) obj))
             (set-hash (slot-name class-name &optional (test #'eql))
               (setf (gethash slot-name (gethash class-name hash-store))
                     (make-hash-table :test test))))
        (dolist (class (hierarchy))
          (setf (gethash (class-name class) hash-store) (make-hash-table)))
        (set-hash 'i-bottom 'bottom #'equalp)
        (set-hash 'middle   'left   #'eql)
        (set-hash 'pu-left  'left   #'equal)
        (set-hash 'pu-right 'right  #'equal)
        (set-hash 'pu-top   'top    #'equal)
        (set-hash 'i-top    'top    #'equalp)
        (set-hash 'cu-top   'bottom #'eql)
        (insert-obj *sample-bottom-1*)
        (insert-obj *sample-bottom-2*)
        hash-store))))

;;;; Helpers

(defun slot-by-name (class name)
  (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))

;;;; Tests for Prevalence System

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
  (let ((bottom (find-class 'bottom)))
    (destructuring-bind (pu-top-lock middle-lock i-bottom-lock)
        (apply #'prevalence-slot-locks bottom (mapcar (lfix #'slot-by-name bottom)
                                                      '(pu-top middle i-bottom)))
      (5am:is-true (search "TOP PU-TOP"      (lock-name pu-top-lock)))
      (5am:is-true (search "LEFT MIDDLE"     (lock-name middle-lock)))
      (5am:is-true (search "BOTTOM I-BOTTOM" (lock-name i-bottom-lock))))))

(5am:test :prevalence-lookup-class-slot-locates-expected-object
  (let ((*prevalence-system* (make-instance 'prevalence-system)))
    (with-slots (hash-store) *prevalence-system*
      (setf hash-store *simple-sample-hash-store*))
    (flet ((check-object (obj)
             (dolist (slotd (set-difference
                             (c2mop:class-slots (class-of obj))
                             (nil-key-slots obj)))
               (let ((slot-name (c2mop:slot-definition-name slotd))
                     (using-class
                       (ccase (key slotd)
                         ((:index :precedence-unique)
                          (find-slot-defining-class (class-of obj) slotd))
                         (:class-unique
                          (class-of obj)))))
                 (5am:is (member obj
                                 (mklist (prevalence-lookup-class-slot
                                          using-class
                                          slotd
                                          (slot-value obj slot-name)))))))))
      (check-object *sample-bottom-1*)
      (check-object *sample-bottom-2*))))


;; Tests for make-instance
;; Tests for setf
;; Tests for metaclass-of
