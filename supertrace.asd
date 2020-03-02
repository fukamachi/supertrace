(defsystem "supertrace"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :description "Superior Common Lisp trace functionality"
  :defsystem-depends-on ((:feature :unix "cffi-grovel"))
  :depends-on ((:feature :unix "cffi"))
  :pathname "src"
  :components
  ((:file "main" :depends-on ("logger"
                              (:feature :unix "clock")))
   (:file "logger")
   (:module "clock"
    :if-feature :unix
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
               "cl-ppcre")
  :pathname "tests"
  :serial t
  :components
  ((:file "logger")
   (:file "main"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
