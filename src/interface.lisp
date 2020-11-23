(in-package :spiclum)

(defmacro defpclass (class-name superclasses slot-specifiers &rest class-options)
  "Programmer macro for defining prevalence classes."
  (when (find :metaclass class-options :key #'car)
    (error "DEFPCLASS uses implicit metaclass PREVALENCE-CLASS,~%~
            but an explicit metaclass was provided for ~S" class-name))
  `(defclass ,class-name (,@superclasses prevalence-object)
     ,slot-specifiers
     ,@class-options
     (:metaclass prevalence-class)))

(defun save-world ()
  'todo)

(defun load-world ()
  'todo)
