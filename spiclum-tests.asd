;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem :spiclum-tests
  :description "Tests for SPICLUM"
  :version "0.0"
  :author "arthev"
  :depends-on (:spiclum
               :cl-mock
               :fiveam)
  :serial t
  :components ((:file "src/test/fixtures")
               (:file "src/test/mop-methods")
               (:file "src/test/serialization-tests")
               (:file "src/test/query-tests")
               (:file "src/test/atomics")
               (:file "src/test/integration")))
