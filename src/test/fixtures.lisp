(in-package :spiclum)

;;;; Top-level test definitions

(5am:def-suite :prevalence
  :description "Test suite to hold all tests of the prevalence system")

(5am:def-suite :prevalence.unit :in :prevalence)

(5am:def-suite :prevalence.integration :in :prevalence)

;;;; Class Hierarchy for Testing

(defpclass top ()
  ((pu-top
    :initarg :pu-top
    :key :unique
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
    :accessor nil-top)))

(defpclass left (top)
  ((pu-left
    :initarg :pu-left
    :key :unique
    :equality #'equal
    :accessor pu-left)
   (middle
    :initarg :middle
    :key :unique
    :accessor middle)))

(defpclass right (top)
  ((pu-right
    :initarg :pu-right
    :key :unique
    :equality #'equal
    :accessor pu-right)
   (middle
    :initarg :middle
    :key :unique
    :accessor middle)))

(defpclass bottom (left right)
  ((i-bottom
    :initarg :i-bottom
    :key :index
    :equality #'equalp
    :accessor i-bottom)))


(defun hierarchy ()
  (mapcar #'find-class '(top left right bottom)))

(dolist (class (hierarchy))
  (c2mop:finalize-inheritance class))

;;;; Fixtures

(defparameter *sample-bottom-unoccupied-plist*
  (list :i-bottom #(1 2 3) :middle 'joda :pu-left "ham" :pu-right "hogr" :pu-top "tap" :i-top 19.0d0 :cu-top 'tja :nil-top 55))

(defun simple-sample-hash-store ()
  (let* ((plist1 (list :i-bottom 5 :middle 'oida  :pu-left "hmm" :pu-right "rett" :pu-top "top" :i-top 19   :cu-top 'hmm :nil-top 55 :uuid -1))
         (plist2 (list :i-bottom 5 :middle 'neida :pu-left "hum" :pu-right "righ" :pu-top "tip" :i-top 19.0 :cu-top 'nei :nil-top 55 :uuid -2))
         (sample-bottom-1 (with-ignored-prevalence (apply #'make-instance 'bottom plist1)))
         (sample-bottom-2 (with-ignored-prevalence (apply #'make-instance 'bottom plist2)))
         (hash-store (make-hash-table)))
    (flet ((insert-obj (obj)
             (pushnew obj (gethash (i-bottom obj) (gethash 'i-bottom (gethash 'bottom hash-store))))
             (pushnew obj (gethash (i-top    obj) (gethash 'i-top    (gethash 'top    hash-store))))
             (pushnew obj (gethash (reverse-class-lookup obj)
                                                  (gethash 'reverse-class-lookup
                                                                     (gethash 'prevalence-object hash-store))))
             (setf (gethash (cu-top obj)   (gethash 'cu-top   (gethash (type-of obj)      hash-store))) obj)
             (setf (gethash (pu-left obj)  (gethash 'pu-left  (gethash 'left              hash-store))) obj)
             (setf (gethash (pu-right obj) (gethash 'pu-right (gethash 'right             hash-store))) obj)
             (setf (gethash (middle  obj)  (gethash 'middle   (gethash 'left              hash-store))) obj)
             (setf (gethash (pu-top  obj)  (gethash 'pu-top   (gethash 'top               hash-store))) obj)
             (setf (gethash (uuid obj)     (gethash 'uuid     (gethash 'prevalence-object hash-store))) obj))
           (set-hash (slot-name class-name &optional (test #'eql))
             (setf (gethash slot-name (gethash class-name hash-store))
                   (make-hash-table :test test))))
      (dolist (class (hierarchy))
        (setf (gethash (class-name class) hash-store) (make-hash-table)))
      (setf (gethash 'prevalence-object hash-store) (make-hash-table))
      (set-hash 'i-bottom 'bottom            #'equalp)
      (set-hash 'middle   'left              #'eql)
      (set-hash 'pu-left  'left              #'equal)
      (set-hash 'pu-right 'right             #'equal)
      (set-hash 'pu-top   'top               #'equal)
      (set-hash 'i-top    'top               #'equalp)
      (set-hash 'cu-top   'bottom            #'eql)
      (set-hash 'uuid     'prevalence-object #'eql)
      (set-hash 'reverse-class-lookup
                          'prevalence-object #'eq)
      (insert-obj sample-bottom-1)
      (insert-obj sample-bottom-2)
      (values hash-store sample-bottom-1 sample-bottom-2))))

(defmacro with-fixture-system ((sample-bottom-1-var sample-bottom-2-var)
                               &body body)
  (let ((store-var (gensym)))
    `(let ((*prevalence-system* (test-prevalence-system)))
       (multiple-value-bind (,store-var ,sample-bottom-1-var ,sample-bottom-2-var)
           (simple-sample-hash-store)
         (with-slots (hash-store) *prevalence-system*
           (setf hash-store ,store-var))
         ,@body))))

;;;; Helpers

(defun test-prevalence-system ()
  (make-instance
   'prevalence-system
   :storage-path (make-pathname :directory "home/arthur/spiclum-test"
                                :name "spiclum-test")))

(defun check-lookup-finds-object-slotd-value-p (obj slotd value)
  (let ((using-class
          (ccase (key slotd)
            ((:index :unique)
             (find-slot-defining-class (class-of obj) slotd))
            (:class-unique
             (class-of obj)))))
    (member obj (mklist (prevalence-lookup-class-slot
                         using-class slotd value)))))

(defun check-lookup-finds-object (obj)
  (dolist (slotd (remove-if #'null (c2mop:class-slots (class-of obj))
                            :key #'key))
    (let* ((slot-name (c2mop:slot-definition-name slotd))
           (slot-value (slot-value obj slot-name)))
      (5am:is-true (check-lookup-finds-object-slotd-value-p obj slotd slot-value)
                   "Can't find ~S for ~S ~S~%" obj slotd slot-value))))
