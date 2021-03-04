(defpackage :spiclum
  (:use :cl)
  (:export ;; basics
           :defpclass
           :delete-object
           :prevalence-class
           :prevalence-object
           :multi-setf ;requires documentation in README
           :multi-psetf ;requires documentation in README - mention (for both) that they will probably not work "optimally" if the things that are being setfed interrelate (since then the undoing/setting to prev value will probably also interrelate etc)

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
