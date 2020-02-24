(defpackage #:supertrace/tests/main
  (:use #:cl
        #:rove
        #:supertrace/main)
  (:import-from #:supertrace/main
                #:parse-supertrace-options)
  (:import-from #:cl-ppcre
                #:scan))

(defpackage #:supertrace/tests/main/test-package
  (:use #:cl)
  (:export #:say-hello
           #:hello-world))
(in-package #:supertrace/tests/main/test-package)
(defun say-hello (to)
  (format t "Hello, ~A!" to))
(defun hello-world ()
  (say-hello "World")
  t)

(in-package #:supertrace/tests/main)

(setup
  (untrace supertrace/tests/main/test-package:say-hello
           supertrace/tests/main/test-package:hello-world))

(deftest parse-supertrace-options
  (flet ((fut (args)
           (multiple-value-list (parse-supertrace-options args))))
    (ok (equal (fut '()) '(() ())))
    (ok (equal (fut '("PREPARE"))
               '(() ("PREPARE"))))
    (ok (equal (fut '("PREPARE" "EXECUTE"))
               '(() ("PREPARE" "EXECUTE"))))
    (ok (equal (fut '(:before #1=(lambda (&rest args)
                                   (warn "before ~S" args))
                      "PREPARE" "EXECUTE"))
               '((:before #1#) ("PREPARE" "EXECUTE"))))
    (ok (equal (fut '(:after #2=(lambda (&rest args)
                                  (warn "after ~S" args))
                      "PREPARE" "EXECUTE"))
               '((:after #2#) ("PREPARE" "EXECUTE"))))
    (ok (equal (fut '(:before #1# :after #2#
                      "PREPARE" "EXECUTE"))
               '((:before #1# :after #2#) ("PREPARE" "EXECUTE"))))
    (ok (equal (fut '("PREPARE" "EXECUTE"
                      :before #1# :after #2#))
               '((:before #1# :after #2#) ("PREPARE" "EXECUTE"))))))

(deftest supertrace
  (supertrace supertrace/tests/main/test-package:say-hello)
  (let* ((*standard-output* (make-broadcast-stream))
         (outputs (with-output-to-string (*trace-output*)
                    (supertrace/tests/main/test-package:say-hello "Eitaro"))))
    (ok (scan "^running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(say-hello \"Eitaro\"\\)
\\d+?\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(say-hello \"Eitaro\"\\) -> nil\\n$" outputs)))

  (supertrace supertrace/tests/main/test-package:hello-world)
  (let* ((*standard-output* (make-broadcast-stream))
         (outputs (with-output-to-string (*trace-output*)
                    (supertrace/tests/main/test-package:hello-world))))
    (ok (scan "^running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(hello-world\\)
running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(say-hello \"World\"\\)
\\d+?\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(say-hello \"World\"\\) -> nil
\\d+?\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(hello-world\\) -> t
$" outputs))))
