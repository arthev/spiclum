(in-package :spiclum)

(defclass prevalence-object ()
  ((uuid
    :initform (generate-uuid-for-object-store)
    :initarg :uuid
    :reader uuid
    :key :precedence-unique
    :equality #'eql))
  (:metaclass prevalence-class)
  (:documentation "Class for prevalent objects to inherit, to specialize on
                   reinitialize-instance etc."))

(defmethod serialize-object ((instance prevalence-object))
  (if *prevalence->lookup-serialization-p*
      (if *saving-world-p*
          `(thunk (find-by-uuid ,(uuid instance)))
          `(find-by-uuid ,(uuid instance)))
      (let ((*prevalence->lookup-serialization-p*))
        (instance->make-instance-form instance))))

(defmethod force-thunks ((instance prevalence-object))
  (do-bound-slots (slotd instance :name slot-name :value slot-value)
    (if (thunkp slot-value)
        (setf (slot-value instance slot-name)
              (force slot-value))
        (force-thunks slot-value))))

;;;; -1. Helpers

(defun compute-slot-diff-against-slotds->values-map (instance map)
  "Returns list of slotds that differ between INSTANCE and MAP, by slot equality fns."
  (remove-if
   (lambda (slotd)
     (let ((slot-name (c2mop:slot-definition-name slotd)))
       (multiple-value-bind (value present-p)
           (gethash slotd map)
         (when (and present-p
                    (slot-boundp instance slot-name))
           (funcall (equality slotd)
                    value
                    (slot-value instance slot-name))))))
   (c2mop:class-slots (class-of instance))))

;;;; 0. Prevalencing actions specializing on prevalence-object

(defmethod reinitialize-instance :around ((instance prevalence-object) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((old-values (slotds->values-map instance))
         ;; doing this here to save on computation inside the locks
         (instance (handler-bind
                       ((error (lambda (e)
                                 (declare (ignore e))
                                 (update-instance-for-slotds->values-map instance old-values))))
                     (with-ignored-prevalence (call-next-method))))
         (updated-slots (compute-slot-diff-against-slotds->values-map instance old-values)))
    (with-recursive-locks (prevalence-slot-locks (class-of instance) updated-slots)
      (as-transaction
          ((:do (prevalence-remove-instance instance
                                            :slots updated-slots
                                            :values old-values)
            :undo (progn (update-instance-for-slotds->values-map instance old-values)
                         (prevalence-insert-instance instance :slots updated-slots)))
           (:do (prevalence-insert-instance instance :slots updated-slots)
            :undo (prevalence-remove-instance instance :slots updated-slots)))
        (serialize-reinitialize-instance instance initargs)
        instance))))

(defmethod change-class :around ((instance prevalence-object) (new-class prevalence-class)
                                 &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (let ((old-values (slotds->values-map instance))
        (old-class (class-of instance)))
    (with-recursive-locks (union (all-prevalence-slot-locks-for old-class)
                                 (all-prevalence-slot-locks-for new-class))
      (as-transaction
          ((:do (prevalence-remove-instance instance)
            :undo (prevalence-insert-instance instance))
           (:do (with-ignored-prevalence (call-next-method))
            :undo (with-ignored-prevalence
                      (call-next-method instance old-class)
                    ;; Simpler than constructing initargs for call-next-method,
                    ;; particularly since initializations can run arbitrary code
                    (update-instance-for-slotds->values-map instance old-values)))
           (:do (prevalence-insert-instance instance)
            :undo (prevalence-remove-instance instance)))
        (serialize-change-class instance new-class initargs)
        instance))))
