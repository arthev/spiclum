(in-package :spiclum)

(defclass prevalence-class (standard-class)
  ()
  (:documentation "Meta-class for persistent objects using prevalence."))

;;;; -1. Helpers

(defmacro with-ignored-prevalence (&rest body)
  `(let ((*persisting-p* nil)
         (*prevalencing-p* nil))
     ,@body))

;;;; 0. Boilerplate

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
  "As for STANDARD-CLASS, but also sets KEY slot of EFFECTIVE-SLOT-DEFINITION"
  (let* ((normal-slot (call-next-method))
         (equality-arg (equality (car direct-slot-definitions)))
         (equality-fn (cond ((functionp equality-arg)
                             equality-arg)
                            ;; Presumably '#'eqtype
                            ((listp equality-arg)
                             (symbol-function (cadr equality-arg)))
                            (t (error "Can't recognise equality-arg: ~S"
                                      equality-arg)))))
    (setf (key normal-slot) (key (car direct-slot-definitions))
          (equality normal-slot) equality-fn)
    normal-slot))

(defmethod c2mop:validate-superclass ((class prevalence-class)
                                      (superclass standard-class))
  "Just boilerplate declaring metaclass compatibility."
  t)

;; NOTE: If we want to allow mixins, we might want the reverse
;; VALIDATE-SUPERCLASS method too. Probably on a 'caveat emptor' basis.
;; Might be rather odd behaviour, particularly if we have different metaclasses
;; on the mixins. For simplicity, designing as if disallowed.

;;;; 1. Acceptable slot values

(defun acceptable-persistent-slot-value-type-p (new-value)
  (%acceptable-persistent-slot-value-type-p (metaclass-of new-value) new-value))

(defgeneric %acceptable-persistent-slot-value-type-p (metaclass new-value)
  (:documentation "Predicate to determine whether NEW-VALUE can be stored
   as a persistent slot value. Exported for further user specification.
   Of course, if something can be stored, it must be (de)serializable as well."))

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'standard-class))) new-value)
  (error "Can't use ~S as slot-value for persistent object,~%~
          since it's metaclass ~S"
         new-value metaclass))

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'structure-class))) new-value)
  (error "Can't use ~S as slot-value for persistent object,~%~
          since it's metaclass ~S"
         new-value metaclass))

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'built-in-class))) new-value)
  t)

(defmethod %acceptable-persistent-slot-value-type-p
    ((metaclass (eql (find-class 'prevalence-class))) new-value)
  t)

;;;; 2. Prevalencing actions specializing on prevalence-class

(defmethod (setf c2mop:slot-value-using-class) :around (new-value
                                                        (class prevalence-class)
                                                        instance
                                                        slotd)
  "SETFs the NEW-VALUE of INSTANCE for SLOTD, as a transaction.

Must atomatically update indexes and persist as appropriate."
  (assert (acceptable-persistent-slot-value-type-p new-value))
  (multiple-value-bind (old-value slot-boundp)
      (guarded-slot-value instance (c2mop:slot-definition-name slotd))
    (let (;; CLHS specifies that SETF may return multiple values
          (results (multiple-value-list (call-next-method))))
      (with-recursive-locks (prevalence-slot-locks class (list slotd))
        (as-transaction
            ((:do (when slot-boundp
                    (prevalence-remove-class-slot class slotd old-value instance))
              :undo (if slot-boundp
                        (progn (call-next-method old-value class instance slotd)
                               (prevalence-insert-class-slot class slotd old-value instance))
                        (slot-makunbound instance (c2mop:slot-definition-name slotd))))
             (:do (prevalence-insert-class-slot class slotd new-value instance)
              :undo (prevalence-remove-class-slot class slotd new-value instance)))
          (values-list results))))))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  "Makes the instance and inserts into the prevalence system as a transaction.

If the transaction fails, remove it and return an error.
Thus, zero references to the object."
  (declare (ignorable class initargs))
  (let ((instance (with-ignored-prevalence (call-next-method))))
    (with-recursive-locks (all-prevalence-slot-locks-for instance)
      (as-transaction
          ((:do (prevalence-insert-instance instance)
            :undo (prevalence-remove-instance instance)))
        instance))))

(defvar *instances-affected-by-redefinition* nil)

(defmethod c2mop:ensure-class-using-metaclass
    ((metaclass prevalence-class) (class null) name &rest args &key)
  (let ((updated-class (call-next-method)))
    (apply #'register-last-class-definition metaclass class name args)
    updated-class))

(defmethod c2mop:ensure-class-using-metaclass
    ((metaclass prevalence-class) (class standard-class) name &rest args &key)
  (with-recursive-locks (all-prevalence-slot-locks-for class)
    (let* ((instances (find-all class))
           (slotds->values-maps (let ((ht (make-hash-table :test #'eq)))
                                  (dolist (instance instances)
                                    (setf (gethash instance ht)
                                          (slotds->values-map instance)))
                                  ht))
           (*instances-affected-by-redefinition* instances))
      (as-transaction
          ((:do (prevalence-remove-instances instances)
            :undo (prevalence-insert-instances instances))
           ;; CLASS (being non-nil) should be the return value of CALL-NEXT-METHOD
           (:do (with-ignored-prevalence (call-next-method))
            :undo (with-ignored-prevalence
                    ;; Might need some special-handling of %ECUC-METHOD
                    (apply #'call-next-method (last-class-definition class name))
                    (dolist (instance instances)
                      (update-instance-for-slotds->values-map
                       instance (gethash instance slotds->values-maps)))))
           (:do (prevalence-insert-instances instances)
            :undo (prevalence-remove-instances instances)))
        (apply #'register-last-class-definition metaclass class name args)
        class))))

(defmethod make-instances-obsolete ((class prevalence-class))
  "Eagerly updates instances of CLASS, so ENSURE-CLASS-USING-METACLASS can be transactional."
  (call-next-method)
  (dolist (instance *instances-affected-by-redefinition*)
    ;; We don't have the argument list for UPDATE-INSTANCE-FOR-REDEFINED-CLASS
    ;; on hand - but we know that instances will update when we try to access
    ;; slot-values. Hence we do precisely that, to force an update.
    (slot-values instance)))
