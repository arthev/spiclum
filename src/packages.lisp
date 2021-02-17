(defpackage :spiclum
  (:use :cl)
  (:export ;; basics
           :defpclass
           :delete-object
           :prevalence-class
           :prevalence-object
           :multi-setf

           ;; queries
           :call-query
           :query

           ;; systems
           :load-world
           :save-world

           ;; conditions
           :non-unique-unique-keys
           :prevalence-breach
           :removing-nonexistant-entry

           ;; data extensions
           :acceptable-persistent-slot-value-type-p
           :force
           :serialize-object
           ))

;; Note that language-utils interns and exports
;; the following two symbols from c2mop:
;; ensure-class-using-metaclass
;; *%ecuc-method*
