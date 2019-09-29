(in :spiclum)

(defclass prevalence-class (standard-class)
  ())

(defmethod closer-mop:slot-value-using-class ((class prevalence-class)
                                   instance
                                   slot-name)
  (format t "Accessing: ~S~%~S~%~S~%" class instance slot-name)
  (call-next-method))

(defmethod (setf closer-mop:slot-value-using-class) (new-value
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



(defmethod closer-mop:validate-superclass ((class prevalence-class)
                                           (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)


(defclass test-class ()
  ((a
    :initarg :a
    :accessor a))
  (:metaclass prevalence-class))
