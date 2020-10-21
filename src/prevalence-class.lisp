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

(defmethod (setf c2mop:slot-value-using-class) (new-value
                                                (class prevalence-class)
                                                instance
                                                slotd)
  "SETFs the NEW-VALUE of INSTANCE for SLOTD, as a transaction.

Must atomatically update indexes and persist as appropriate."
  (assert (acceptable-persistent-slot-value-type-p new-value))
  (multiple-value-bind (old-value slot-boundp)
      (guarded-slot-value instance (c2mop:slot-definition-name slotd))
    (let (;; CLHS specifies that SETF may return multiple values
          (results (multiple-value-list (call-next-method)))
          (same-index-p (and slot-boundp
                             (funcall (equality slotd) new-value old-value)))
          prevalenced-p removed-p success-p)
      (with-recursive-locks (prevalence-slot-locks class (list slotd))
        (unwind-protect
             (progn
               (unless same-index-p
                 (prevalence-insert-class-slot class slotd new-value instance)
                 (setf prevalenced-p t)
                 (when slot-boundp
                   (prevalence-remove-class-slot class slotd old-value instance)
                   (setf removed-p t)))
               :persist ;TEMPER
               (setf success-p t)
               (values-list results))
          (unless success-p
            (when prevalenced-p
              (prevalence-remove-class-slot class slotd new-value instance))
            (if slot-boundp
                (call-next-method old-value class instance slotd)
                (slot-makunbound instance (c2mop:slot-definition-name slotd)))
            (when removed-p
              (prevalence-insert-class-slot class slotd old-value instance))))))))

(defmethod make-instance ((class prevalence-class) &rest initargs &key)
  "Makes the instance and inserts into the prevalence system as a transaction.

If the transaction fails, remove it and return an error.
Thus, zero references to the object."
  (declare (ignorable class initargs))
  (let ((instance (with-ignored-prevalence (call-next-method)))
        success-p)
    (with-recursive-locks (all-prevalence-slot-locks-for instance)
      (unwind-protect
           (progn (prevalence-insert-instance instance)
                  :persist ; TEMPER
                  (setf success-p t)
                  instance)
        (unless success-p
          (prevalence-remove-instance instance))))))
