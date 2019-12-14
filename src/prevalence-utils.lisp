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

(defparameter *persisting-p* t
  "Toggle for whether to persist data or not.")

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

(defmethod initialize-instance :before ((slot keyable-slot) &key key &allow-other-keys)
  (assert (member key +legal-key-slot-values+)
          (key)
          "Illegal :KEY value for slot specification:~S"
          key))

(defmethod reinitialize-instance :before ((slot keyable-slot) &key key &allow-other-keys)
  (assert (member key +legal-key-slot-values+)
          (key)
          "Illegal :KEY value for slot specification:~S"
          key))

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

;; Seems like the main intercessory points are setfing values, instantiating, and reinstatiating (e.g. due to class change/redefinition).
;; Also seems like we'll need an additional point, to remove instances from the prevalence system.

(defun prevalence-writer (&rest args)
  (format t "Dummy-writer: ~S" args))

(defun prevalence-insert (&rest args)
  (format t "Dummy-insert: ~S" args))

(defmethod c2mop:validate-superclass ((class prevalence-class)
                                           (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)

(defmethod (setf c2mop:slot-value-using-class) (new-value
                                                (class prevalence-class)
                                                instance
                                                slot-name)
  "Responsible for validation, calling the standard-class setf,
   and for making and commiting a transaction."
  ;; Must both clean up previous system-entry as well as set new entry.
  ;; And write to the persistence log (potentially).

  ;; Validate the setf input (can we handle this data?)
  (cond ((eq 'standard-class
             (class-name (class-of (class-of new-value))))
         (error "Can't handle ~S since it's often STANDARD-CLASS." new-value))

        )

  ;; TEMP
  (format t "Setting: ~S~%    ~S~%    ~S~%    ~S~%" new-value class instance slot-name)
  ;; Pre-check validity of move if slot is :unique. Might need a touch of locking here.

  ;; CLHS specifies that setf'ing can return multiple values
  (let ((results (multiple-value-list (call-next-method))))
    ;; Move instance if indexing slot
    ;; Make and commit transaction
    (values-list results)))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  (declare (ignorable class initargs))
  (prog1 (let ((*persisting-p* nil))
           (call-next-method))
    ;; Insert into prevalence system w/ neccessary pre-check of validity
    (apply #'prevalence-writer 'make-instance class initargs)))

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
