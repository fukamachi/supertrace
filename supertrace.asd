(defsystem "supertrace"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Superior Common Lisp trace functionality"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi")
  :pathname "src"
  :components
  ((:file "main" :depends-on ("logger"
                              "clock"))
   (:file "logger")
   (:module "clock"
    :serial t
    :components
    ((:file "package")
     (:cffi-grovel-file "grovel")
     (:file "cffi")
     (:file "main"))))
  :in-order-to ((test-op (test-op "supertrace/tests"))))

(defsystem "supertrace/tests"
  :depends-on ("supertrace"
               "rove"
               "cl-ppcre"
               "bordeaux-threads")
  :pathname "tests"
  :serial t
  :components
  ((:file "logger")
   (:file "main"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
