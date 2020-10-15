;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :spiclum
  :description "System Prevalence in Common Lisp using MOP"
  :version "-1.0"
  :author "arthev"
  :depends-on (:bordeaux-threads
               :fiveam
               :closer-mop
               :cl-arthur)
  :components ((:file "src/packages")
               (:file "src/prevalence-utils")
               (:file "src/prevalence-utils-tests")
               (:file "src/main")))
