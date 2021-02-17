(in-package :spiclum)

(defmacro defpclass (class-name superclasses slot-specifiers &rest class-options)
  "Programmer macro for defining prevalence classes.

Handles the PREVALENCE-CLASS metaclass & the PREVALENCE-OBJECT superclass.
See PREVALENCE-CLASS for documentation on slot-specifier options."
  (when (find :metaclass class-options :key #'car)
    (error "DEFPCLASS uses implicit metaclass PREVALENCE-CLASS,~%~
            but an explicit metaclass was provided for ~S" class-name))
  ;; NOTE: It'd also be possible to inject PREVALENCE-OBJECT at the ensure-class-using-metaclass level
  `(defclass ,class-name (,@superclasses prevalence-object)
     ,slot-specifiers
     ,@class-options
     (:metaclass prevalence-class)))

(defmacro multi-setf (&rest pairs)
  (assert (evenp (length pairs)) (pairs) "setf (and multi-setf) operates on pairs, but PAIRS was odd length")
  ;; TODO: Extract locks
  (let* ((return-var (gensym "return"))
         (digested-pairs
           (loop for (place values) on pairs by #'cddr
                 collect (list
                          (gensym)
                          :setf-expansion (multiple-value-list
                                           (get-setf-expansion place))
                          :values-form values)))
         (actions
           (mapcar
            (lambda (digested-pair)
              (destructuring-bind (gensym
                                   &key setf-expansion values-form)
                  digested-pair
                (destructuring-bind (vars vals store-vars
                                     writer-form reader-form)
                    setf-expansion
                  `(:do
                    (let* ,(mapcar #'list vars vals)
                      (setf ,gensym (multiple-values-list ,reader-form))
                      (setf ,return-var (multiple-values-list ,values-form))
                      (multiple-value-bind ,store-vars
                          (values-list ,return-var)
                        ,writer-form))
                    :undo
                    (let* ,(mapcar #'list vars vals)
                      (multiple-value-bind ,store-vars (values-list ,gensym)
                        ,writer-form))))))
            digested-pairs)))
    `(let (;; create some gensym bindings
           ,@(mapcar #'car digested-pairs) ,return-var)
       (as-transaction
           (,@actions)
         ;; We serialize through letting the individual setfs serialize.
         ;; This is transactive modulo interrupts like the lisp dying.
         (values-list ,return-var)))))

(defun delete-object (obj)
  "Deletes OBJ by removing it from the object-store.

Note that a correct deletion requires the application
to ensure that no prevalence-object holds a reference
to the deleted object."
  (check-type obj prevalence-object)
  (with-recursive-locks (all-prevalence-slot-locks-for obj)
    (prevalence-remove-instance obj)))

(defun save-world (&key directory name)
  "Save a new world as per DIRECTORY and NAME.

If DIRECTORY or NAME (or both) are nil, then read the values
from *PREVALENCE-SYSTEM*'s STORAGE-PATH with time-of-call.

Writes the set of class definitions for prevalence-classes,
and the set of instances of prevalence-object."
  (when (or directory name)
    (with-accessors ((storage-path storage-path)) *prevalence-system*
      (setf storage-path
            (make-pathname :directory (or directory
                                          (pathname-directory storage-path))
                           :name (or name
                                     (pathname-name storage-path))))
      (ensure-directories-exist storage-path)))
  (update-prevalence-system-for-timestamp *prevalence-system*
                                          (ANSI-time))
  (initialize-prevalence-system-files *prevalence-system*)
  (let ((*saving-world-p* t))
    (maphash
     (lambda (name last-definition)
       (declare (ignore name))
       (destructuring-bind (name &rest args) last-definition
         (key-args (name args) serialize :ensure-class-using-metaclass)))
     (class-definition-store *prevalence-system*))
    (dolist (instance (find-all (find-class 'prevalence-object)))
      (serialize :make-instance :instance instance)))
  :ok)

(defun load-world (&key directory name)
  "Loads a world/transaction log (or creates empty ones).

Initializes the object-store/prevalence-system.
DIRECTORY and NAME are required."
  ;; e.g. directory "home/arthur/spiclum-test" name "spiclum-test"
  (assert (and directory name))
  (setf *prevalence-system*
        (make-instance 'prevalence-system
                       :storage-path (make-pathname :directory directory
                                                    :name name)))
  (let ((*package* (find-package :spiclum))
        (*persisting-p* nil))
    (load (world-file *prevalence-system*))
    (load (log-file *prevalence-system*))
    (force-all-thunks)
    (reset-uuid-seed-for-object-store)))
