(in-package :spiclum)

(defclass prevalence-object ()
  ()
  (:metaclass prevalence-class)
  (:documentation "Class for prevalent objects to inherit, to specialize on
                   reinitialize-instance etc."))

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
         (instance (let (success-p)
                     (unwind-protect
                          (progn (with-ignored-prevalence (call-next-method))
                                 (setf success-p t)
                                 instance)
                       (unless success-p
                         (update-instance-for-slotds->values-map instance old-values)))))
         (updated-slots (compute-slot-diff-against-slotds->values-map instance old-values))
         prevalence-insert-p prevalence-remove-p success-p)
    (with-recursive-locks (prevalence-slot-locks (class-of instance) updated-slots)
      (unwind-protect
           (progn
             (prevalence-insert-instance instance :slots updated-slots)
             (setf prevalence-insert-p t)
             (prevalence-remove-instance instance
                                         :slots updated-slots
                                         :values old-values)
             (setf prevalence-remove-p t)
             :persist ;TEMPER
             (setf success-p t)
             instance)
        (unless success-p
          (when prevalence-insert-p
            ;; since we use the same order as they were inserted in,
            ;; if we encounter an error, it should be at the first non-inserted slot,
            ;; which is presumably where we abandoned out anyhow.
            (ignore-errors (prevalence-remove-instance instance :slots updated-slots)))
          (update-instance-for-slotds->values-map instance old-values)
          (when prevalence-remove-p ; as above
            (ignore-errors (prevalence-insert-instance instance :slots updated-slots))))))))

(defmethod change-class :around ((instance prevalence-object) (new-class prevalence-class)
                                 &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (let ((old-values (slotds->values-map instance))
        (old-class (class-of instance))
        prevalence-remove-p call-next-method-p prevalence-insert-p success-p)
    (with-recursive-locks (union (all-prevalence-slot-locks-for old-class)
                                 (all-prevalence-slot-locks-for new-class))
      (unwind-protect
           (progn
             (prevalence-remove-instance instance)
             (setf prevalence-remove-p t)
             (with-ignored-prevalence
                 (call-next-method))
             (setf call-next-method-p t)
             (prevalence-insert-instance instance)
             (setf prevalence-insert-p t)
             :persist ;; TEMPER
             (setf success-p t)
             instance)
        (unless success-p
          (when prevalence-insert-p
            (prevalence-remove-instance instance))
          (when call-next-method-p
            (with-ignored-prevalence
                (call-next-method instance old-class)
              ;; Simpler than constructing initargs for call-next-method,
              ;; particularly since initializations can run arbitrary code
              (update-instance-for-slotds->values-map instance old-values)))
          (when prevalence-remove-p
            (prevalence-insert-instance instance)))))))
