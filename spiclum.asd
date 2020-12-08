;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :spiclum
  :description "System Prevalence in Common Lisp using MOP"
  :version "-1.0"
  :author "arthev"
  :depends-on (:bordeaux-threads
               :fiveam
               :closer-mop
               :cl-fad
               :cl-arthur)
  :components ((:file "src/packages")
               (:file "src/language-utils")
               (:file "src/conditions")
               (:file "src/keyable-slot")
               (:file "src/object-store")
               (:file "src/queries")
               (:file "src/serialization")
               (:file "src/prevalence-class")
               (:file "src/prevalence-object")
               (:file "src/interface")
               (:file "src/test/fixtures")
               (:file "src/test/mop-methods")
               (:file "src/test/serialization-tests")
               (:file "src/test/query-tests")
               (:file "src/test/integration")))
