(defsystem "supertrace"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Superior Common Lisp trace functionality"
  :depends-on ("supertrace/main")
  :pathname "src"
  :in-order-to ((test-op (test-op "supertrace/tests"))))

(defsystem "supertrace/tests"
  :depends-on ("supertrace"
               "rove"
               "cl-ppcre")
  :pathname "tests"
  :serial t
  :components
  ((:file "logger")
   (:file "main"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
