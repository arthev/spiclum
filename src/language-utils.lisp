(in-package :spiclum)

;;;; 0. :bordeaux-threads utils

(defun lock-name (lock)
  "Access the name of a lock. :bordeaux-threads doesn't support this.

Implemented for: SBCL"
  (sb-thread:mutex-name lock))

(defmacro with-recursive-locks (locks &body body)
  "Grab LOCKS (potentially recursively) in alphabetical order, execute BODY."
  `(call-with-recursive-locks
    (sort ,locks #'string< :key #'lock-name)
    (lambda () ,@body)))

(defun call-with-recursive-locks (locks body)
  "Function seam for WITH-RECURSIVE-LOCKS.

Usage of bt:with-recursive-lock-held probably inefficient
compared with bt:acquire-recursive-lock. But :bordeaux-threads
don't support that for SBCL."
  (labels ((internal (locks)
             (if (endp locks)
                 (funcall body)
                 (bt:with-recursive-lock-held ((car locks))
                   (internal (cdr locks))))))
    (internal locks)))

;;;; 1. CLOS/MOP utils

(defun metaclass-of (obj)
  "Returns two values: OBJ's metaclass, and whether (classp OBJ).

Probably assumes no metaclasses have metaclasses."
  (if (c2mop:classp obj)
      (values (class-of obj) t)
      (values (class-of (class-of obj)) nil)))

(defun slotds->values-map (instance)
  "Creates a map from INSTANCE's slotds to their values. Unbound slots aren't keyed."
  (let ((hash-table (make-hash-table)))
    (dolist (slotd (c2mop:class-slots (class-of instance)))
      (let ((slot-name (c2mop:slot-definition-name slotd)))
        (when (slot-boundp instance slot-name)
          (setf (gethash slotd hash-table)
                (slot-value instance slot-name)))))
    hash-table))

(defun update-instance-for-slotds->values-map (instance map)
  "Update all of INSTANCE's slot values to match MAP."
  (dolist (slotd (c2mop:class-slots (class-of instance)))
    (multiple-value-bind (value present-p)
        (gethash slotd map)
      (if present-p
          (setf (slot-value instance (c2mop:slot-definition-name slotd))
                value)
          (slot-makunbound instance (c2mop:slot-definition-name slotd))))))

(defun guarded-slot-value (instance slot-name)
  (if (slot-boundp instance slot-name)
      (values (slot-value instance slot-name) t)
      (values nil nil)))

(defun find-slot-defining-class (class slotd)
  "Finds the most specific slot-defining-class by
   searching through CLASS's precedence list until
   the first hit for a direct-slot-definition."
  (assert slotd)
  (find-if (lambda (candidate-class)
             (find-if (rfix #'direct-effective-slot-equivalence slotd)
                      (c2mop:class-direct-slots candidate-class)))
           (c2mop:class-precedence-list class)))

;; TODO: Something hereabouts looks funky.
(defun direct-effective-slot-equivalence (direct-slot effective-slot)
  (eq (c2mop:slot-definition-name direct-slot)
      (c2mop:slot-definition-name effective-slot)))

;;;; 2. Miscellaneous utils

(defmacro as-transaction (actions &body body)
  (let* ((gensyms (mapcar (ignore-args #'gensym) actions))
         (success-sym (gensym "success"))
         (dos-and-flags (loop for action in actions
                              for gensym in gensyms
                              collect (getf action :do)
                              collect `(setf ,gensym t)))
         (undos-in-whens (loop for action in (reverse actions)
                               for gensym in (reverse gensyms)
                               collect `(when ,gensym
                                          ,(getf action :undo)))))
    `(let ,`(,@gensyms ,success-sym)
       (unwind-protect
            (progn
              ,@dos-and-flags
              (setf ,success-sym t)
              ,@body)
         (unless ,success-sym
           ,@undos-in-whens)))))
