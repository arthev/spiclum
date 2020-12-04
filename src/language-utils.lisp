(in-package :spiclum)

;;;; 0. :bordeaux-threads utils

(defun lock-name (lock)
  "Access the name of a lock. :bordeaux-threads doesn't support this."
  #+sbcl
  (sb-thread:mutex-name lock)
  #-sbcl
  (error "Not implemented"))

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
  (if (endp locks)
      (funcall body)
      (bt:with-recursive-lock-held ((car locks))
        (call-with-recursive-locks (cdr locks) body))))

;;;; 1. CLOS/MOP utils

(defmacro do-bound-slots ((slot-var instance
                           &key
                             (slots nil slots-supplied-p)
                             (value (gensym) value-supplied-p)
                             (name (gensym)))
                          &body body)
  (let ((slots (if slots-supplied-p
                   slots
                   `(c2mop:class-slots (class-of ,instance)))))
    `(dolist (,slot-var ,slots)
       (let ((,name (c2mop:slot-definition-name ,slot-var)))
         (when (slot-boundp ,instance ,name)
           (let ((,value ,(if value-supplied-p
                              `(slot-value ,instance ,name)
                              nil)))
             ,@body))))))

(defun slot-by-name (class name)
  (unless (c2mop:class-finalized-p class)
    (c2mop:finalize-inheritance class))
  (find name (c2mop:class-slots class) :key #'c2mop:slot-definition-name))

(defun slot->value-map (instance)
  "Hash-table of slot-names->values. Unbound slots aren't keyed."
  (prog1-let (hash-table (make-hash-table))
    (do-bound-slots (slotd instance :value slot-value :name slot-name)
      (setf (gethash slot-name hash-table) slot-value))))

(defun update-instance-for-slot->value-map (instance map)
  "Update all of INSTANCE's slot values to match MAP."
  (assert (subsetp (hash-keys map) (slot-names instance)))
  (dolist (slotd (c2mop:class-slots (class-of instance)))
    (let ((slot-name (c2mop:slot-definition-name slotd)))
      (multiple-value-bind (value present-p)
          (gethash slot-name map)
        (if present-p
            (setf (slot-value instance slot-name) value)
            (slot-makunbound instance slot-name))))))

(defun guarded-slot-value (instance slot-name)
  "Like SLOT-VALUE: doesn't error; secondary value indicates whether the slot is bound."
  (if (slot-boundp instance slot-name)
      (values (slot-value instance slot-name) t)
      (values nil nil)))

(defun all-subclasses (class)
  (labels ((internal (class)
             (append (mklist class)
                     (lwhen (subclasses (c2mop:class-direct-subclasses class))
                       (mapappend #'all-subclasses subclasses)))))
    (remove-duplicates (internal class) :test #'eq)))


(defun find-slot-defining-class (class slotd)
  "Find the most specific slot-defining-class by
   searching through CLASS's precedence list until
   the first hit for a direct-slot-definition."
  (check-type slotd c2mop:standard-slot-definition)
  (find-if (rfix #'class-defines-slot-p slotd)
           (c2mop:class-precedence-list class)))

(defun class-defines-slot-p (class slotd)
  (find-if (rfix #'slot-name-equivalence slotd)
           (c2mop:class-direct-slots class)))

(defun slot-name-equivalence (&rest slots)
  (reduce #'eq slots :key #'c2mop:slot-definition-name))

;;;; 2. CLOS/MOP extensions

;; Let's hack the desired symbol for now
(eval-when (:compile-toplevel :load-toplevel :execute)
  (intern "ENSURE-CLASS-USING-METACLASS" 'c2mop)
  (export (find-symbol "ENSURE-CLASS-USING-METACLASS" 'c2mop) 'c2mop)
  (intern "*%ECUC-METHOD*" 'c2mop)
  (export (find-symbol "*%ECUC-METHOD*" 'c2mop) 'c2mop))

(defvar c2mop:*%ecuc-method* nil
  "This holds the next ensure-class-using-class method for use with the
ensure-class-using-metaclass modification of the MOP. Internal implementation detail.")

(defgeneric c2mop:ensure-class-using-metaclass
    (metaclass class name
     &key &allow-other-keys)
  (:documentation "Extend the MOP since ENSURE-CLASS-USING-CLASS doesn't allow specializing on the metaclass.

METACLASS is a prototype of the metaclass."))

(defmethod c2mop:ensure-class-using-class :around
    (class name &rest args &key (metaclass 'standard-class) &allow-other-keys)
  (assert (or (symbolp metaclass) (c2mop:classp metaclass)))
  (let ((metaclass (if (symbolp metaclass) (find-class metaclass) metaclass))
        (c2mop:*%ecuc-method*
          (lambda (class name &rest internal-args &key &allow-other-keys)
            (apply #'call-next-method
                   class
                   name
                   :metaclass metaclass
                   ;; And, so we'll capture implementation-specific extras:
                   (append internal-args args)))))
    (apply #'c2mop:ensure-class-using-metaclass (c2mop:class-prototype metaclass) class name args)))

(defmethod c2mop:ensure-class-using-metaclass
    (metaclass class name
     &rest args &key &allow-other-keys)
  (apply c2mop:*%ecuc-method*
         class
         name
         args))

;;;; 3. Miscellaneous utils

(defmacro as-transaction (actions &body body)
  "Treats ACTIONS like a transaction in attempting to rollback if something goes awry.

ACTIONS is a list of lists on form (:do [form] :undo [form]),
where the :do forms get evaluated in turn. If there's a non-local
exit from AS-TRANSACTION during the evaluation of the :do forms,
the :undo forms get evaluated - starting from the one matching
the last :do form that completed, and going 'up' through ACTIONS,
:undo-ing the most recent :do form to complete first, then the
second-most recent, and so forth.

BODY is whatever to do once the ACTIONS have been carried out.
Non-local exits from BODY do not trigger rollback as above."
  (let* ((gensyms (mapcar (ignore-args #'gensym) actions))
         (success-sym (gensym "success"))
         (dos-and-flags (mapappend (lambda (action gensym)
                                     `(,(getf action :do)
                                       (setf ,gensym t)))
                                   actions gensyms))
         (undos-in-whens (mapcar (lambda (action gensym)
                                   `(when ,gensym ,(getf action :undo)))
                                 (reverse actions) (reverse gensyms))))
    `(let ,`(,@gensyms ,success-sym)
       (unwind-protect
            (progn
              ,@dos-and-flags
              (setf ,success-sym t)
              (handler-bind ((error (lambda (e)
                                      (declare (ignore e))
                                      (setf ,success-sym nil))))
                ,@body))
         (unless ,success-sym
           ,@undos-in-whens)))))

(defun ANSI-time (&optional (time (get-universal-time)) (tz 0))
  "ANSI format for date/time without fractional second part.

Accepts a timezone, but doesn't (yet?) reflect the timezone in the output."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time tz)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

(defun list-directory-for-type (directory-pathname type)
  (remove-if (complement (lfix #'string= type))
             (cl-fad:list-directory directory-pathname)
             :key #'pathname-type))

(defmacro key-args ((&rest key-args) &rest call)
  "For cases where calls look like e.g. (fn a1 ... an :k1 k1 ... :kn kn).

Now you can just (key-args (k1 k2 ... kn) a1 ... an) instead!"
  (let ((canonicalized (mapappend #'list
                                  (mapcar #'keywordify key-args)
                                  key-args)))
    (append call canonicalized)))
