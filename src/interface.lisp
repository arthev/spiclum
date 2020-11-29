(in-package :spiclum)

(defmacro defpclass (class-name superclasses slot-specifiers &rest class-options)
  "Programmer macro for defining prevalence classes."
  (when (find :metaclass class-options :key #'car)
    (error "DEFPCLASS uses implicit metaclass PREVALENCE-CLASS,~%~
            but an explicit metaclass was provided for ~S" class-name))
  ;; NOTE: It'd also be possible to inject PREVALENCE-OBJECT at the ensure-class-using-metaclass level
  `(defclass ,class-name (,@superclasses prevalence-object)
     ,slot-specifiers
     ,@class-options
     (:metaclass prevalence-class)))

(defun save-world ()
  "Save a new file as per *PREVALENCE-SYSTEM*'s STORAGE-PATH with time-of-call.

Writes the set of class definitions for prevalence-classes,
and the set of instances of prevalence-object."
  (update-prevalence-system-for-timestamp *prevalence-system*
                                          (ANSI-time))
  (let ((*saving-world-p* t))
    (maphash
     (lambda (name last-definition)
       (declare (ignore name))
       (apply #'serialize :ensure-class-using-metaclass last-definition))
     (class-definition-store *prevalence-system*))
    (dolist (object (find-all (find-class 'prevalence-object)))
      (serialize :make-instance :object object))))

(defun load-world (directory name)
  ;; e.g. directory "home/arthur/spiclum-test" name "spiclum-test"
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
