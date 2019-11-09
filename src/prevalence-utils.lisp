(in-package :spiclum)

(defclass prevalence-class (standard-class)
  ()
  (:documentation "Meta-class for persistent objects using prevalence."))

(defclass keyable-slot (c2mop:standard-slot-definition)
  ((key :initarg :key
        :accessor key
        :initform nil
        :documentation "Subslot to specify whether a slot is a key.
                        member of +legal-key-slot-values+")))


;;;; 0. KEYABLE-SLOT section

;; See: https://stackoverflow.com/questions/30195608/custom-slot-options-dont-apply-any-reduction-to-its-argument
;; What happens if a class definition changes? Need to auto-update indexes.

(defparameter +legal-key-slot-values+ '(nil :unique :index))

(defclass keyable-direct-slot (keyable-slot
                               c2mop:standard-direct-slot-definition)
  ())

(defclass keyable-effective-slot (keyable-slot
                                  c2mop:standard-effective-slot-definition)
  ())

(defmethod initialize-instance :after ((slot keyable-slot) &key)
  (assert (member (key slot) +legal-key-slot-values+)
          ((key slot))
          "Illegal :KEY value for slot specification: ~S"
          (key slot)))

(defmethod c2mop:direct-slot-definition-class ((class prevalence-class)
                                               &rest initargs)
  "Efficiency here could possibly be increased by checking whether
   a normal slot would suffice, by analyzing INITARGS. Complicates
   both this method and COMPUTE-EFFECTIVE-SLOT-DEFINITION, however."
  (declare (ignore initargs))
  (find-class 'keyable-direct-slot))

(defmethod c2mop:effective-slot-definition-class ((class prevalence-class)
                                               &rest initargs)
  "Efficiency here could possibly be increased by checking whether
   a normal slot would suffice, by analyzing INITARGS. Complicates
   both this method and COMPUTE-EFFECTIVE-SLOT-DEFINITION, however."
  (declare (ignore initargs))
  (find-class 'keyable-effective-slot))

(defmethod c2mop:compute-effective-slot-definition ((class prevalence-class)
                                                    slot-name
                                                    direct-slot-definitions)
  "As for STANDARD-CLASS, except updates the KEY slot
   of the EFFECTIVE-SLOT-DEFINITION."
  (let* ((effective-slot-definition (call-next-method))
         (direct-slot-definition
           (find (c2mop:slot-definition-name effective-slot-definition)
                 direct-slot-definitions
                 :key #'c2mop:slot-definition-name)))
    (setf (key effective-slot-definition)
          (key direct-slot-definition))
    effective-slot-definition))




;;;; 1. PREVALENCE-CLASS section

(defmethod c2mop:validate-superclass ((class prevalence-class)
                                           (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)


(defmethod c2mop:slot-value-using-class ((class prevalence-class)
                                   instance
                                   slot-name)
  (format t "Accessing: ~S~%~S~%~S~%" class instance slot-name)
  (call-next-method))

(defmethod (setf c2mop:slot-value-using-class) (new-value
                                                (class prevalence-class)
                                                instance
                                                slot-name)
  "Responsible for validation, calling the standard-class setf,
   and for making and commiting a transaction."
  (cond ((eq 'standard-class
             (class-name (class-of (class-of new-value))))
         (error "Can't handle ~S since it's often STANDARD-CLASS." new-value))

        )
  (format t "Setting: ~S~%~S~%~S~%~S~%" new-value class instance slot-name)
  ;; CLHS specifies that setf'ing can return multiple values
  (let ((results (multiple-value-list (call-next-method))))
    ;; TODO: Make and commit transaction
    (values-list results)))





(defclass test-class ()
  ((a
    :initarg :a
    :key :unique
    :accessor a))
  (:metaclass prevalence-class))

(defclass tester-class2 (test-class)
  ((b
    :initarg :b
    :key :index
    :accessor b))
  (:metaclass prevalence-class))

(defclass tester-class3 (test-class)
  ((c
    :initarg :c
    :key nil
    :accessor c))
  (:metaclass prevalence-class))





;
