(in-package :spiclum)

(defclass prevalence-object ()
  ((uuid
    :initform (generate-uuid-for-object-store)
    :initarg :uuid
    :reader uuid
    :key :unique
    :equality #'eql)
   (reverse-class-lookup
    :accessor reverse-class-lookup
    :key :index
    :equality #'eq))
  (:metaclass prevalence-class)
  (:documentation "Superclass for all prevalence-objects.

Necessary since some of he intercessory methods don't specialize on metaclass.
Further, it defines some slots used by the object-store."))

(defmethod serialize-object ((instance prevalence-object))
  (if *saving-world-p*
      `(thunk (find-by-uuid ,(uuid instance)))
      `(find-by-uuid ,(uuid instance))))

(defmethod force ((instance prevalence-object))
  (do-bound-slots (slotd instance :name slot-name :value slot-value)
    (setf (slot-value instance slot-name)
          (force slot-value)))
  instance)

;;;; -1. Helpers

(defun compute-slot-diff-against-slot->value-map (instance map)
  "Returns list of slotds that differ between INSTANCE and MAP, by slot equality fns."
  (remove-if
   (lambda (slotd)
     (let ((slot-name (c2mop:slot-definition-name slotd)))
       (multiple-value-bind (value present-p)
           (gethash slot-name map)
         (multiple-value-bind (slot-value boundp)
             (guarded-slot-value instance slot-name)
           (or (not (or present-p boundp))
               (and present-p boundp
                    (funcall (equality slotd) value slot-value)))))))
   (c2mop:class-slots (class-of instance))))

;;;; 0. Prevalencing actions specializing on prevalence-object

(defmethod reinitialize-instance :around ((instance prevalence-object) &rest initargs &key &allow-other-keys)
  (let* ((old-values (slot->value-map instance))
         ;; doing this here to save on computation inside the locks
         (instance (handler-bind
                       ((error (lambda (e)
                                 (declare (ignore e))
                                 (update-instance-for-slot->value-map instance old-values))))
                     (with-ignored-prevalence (call-next-method))))
         (updated-slots (compute-slot-diff-against-slot->value-map instance old-values)))
    (with-recursive-locks (prevalence-slot-locks (class-of instance) updated-slots)
      (as-transaction
          ((:do (prevalence-remove-instance instance
                                            :slots updated-slots
                                            :values old-values)
            :undo (progn (update-instance-for-slot->value-map instance old-values)
                         (prevalence-insert-instance instance :slots updated-slots)))
           (:do (prevalence-insert-instance instance :slots updated-slots)
            :undo (prevalence-remove-instance instance :slots updated-slots)))
        (key-args (instance initargs) serialize :reinitialize-instance)
        instance))))

(defmethod change-class :around ((instance prevalence-object) (new-class prevalence-class)
                                 &rest initargs &key &allow-other-keys)
  (declare (ignorable initargs))
  (let ((old-values (slot->value-map instance))
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
                    (update-instance-for-slot->value-map instance old-values)))
           (:do (prevalence-insert-instance instance)
            :undo (prevalence-remove-instance instance)))
        (key-args (instance new-class initargs) serialize :change-class)
        instance))))

;;;; 1. Methods to ensure coherent REVERSE-CLASS-LOOKUP

(defmethod initialize-instance :after ((instance prevalence-object) &key &allow-other-keys)
  (setf (reverse-class-lookup instance) (class-of instance)))

(defmethod update-instance-for-different-class :after (previous (current prevalence-object) &rest initargs)
  (declare (ignore initargs))
  (setf (reverse-class-lookup current) (class-of current)))
