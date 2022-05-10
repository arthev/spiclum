(in-package :spiclum)

(define-condition prevalence-breach (error)
  ((breach-values
    :initarg :breach-values
    :accessor breach-values)
   (breach-slots
    :initarg :breach-slots
    :accessor breach-slots)
   (breach-class
    :initarg :breach-class
    :accessor breach-class))
  (:report
   (lambda (condition stream)
     (format stream "Breach for class ~S, slot(s) ~S, value(s) ~S"
             (breach-class condition)
             (breach-slots condition)
             (breach-values condition))))
  (:documentation
   "Ancestor error for different breaches of prevalence constraints."))

(define-condition non-unique-unique-keys (prevalence-breach)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Uniqueness broken: class ~S, slot(s) ~S, value(s) ~S"
             (breach-class condition)
             (breach-slots condition)
             (breach-values condition))))
  (:documentation
   "For breaches of the uniqueness constraints on keyable-slots."))

(define-condition removing-nonexistant-entry (prevalence-breach)
  ((breach-object
   :initarg :breach-object
   :accessor breach-object))
  (:report
   (lambda (condition stream)
     (format stream "Tried to remove object ~S, for class ~S, slot(s) ~S, value(s) ~S,~%
but couldn't find an entry"
             (breach-object condition) (breach-class condition)
             (breach-slots   condition) (breach-values condition))))
  (:documentation
   "For when trying to remove objects from mismatched entries."))
