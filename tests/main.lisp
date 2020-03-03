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
           #:hello-world
           #:wait-a-while
           #:wait-a-while2))
(in-package #:supertrace/tests/main/test-package)
(defun say-hello (to)
  (format t "Hello, ~A!" to))
(defun hello-world ()
  (say-hello "World")
  t)
(defun wait-a-while ()
  (sleep 0.3))
(defun wait-a-while2 ()
  (sleep 0.1)
  (wait-a-while)
  (sleep 0.2))

(in-package #:supertrace/tests/main)

(setup
  (untrace supertrace/tests/main/test-package:say-hello
           supertrace/tests/main/test-package:hello-world
           supertrace/tests/main/test-package:wait-a-while
           supertrace/tests/main/test-package:wait-a-while2))

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
  (let ((outputs (let ((*standard-output* (make-broadcast-stream)))
                   (with-output-to-string (*trace-output*)
                     (supertrace/tests/main/test-package:hello-world)))))
    (ok (scan "^running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(hello-world\\)
running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(say-hello \"World\"\\)
\\d+?\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(say-hello \"World\"\\) -> nil
\\d+?\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(hello-world\\) -> t
$" outputs)))

  (supertrace supertrace/tests/main/test-package:wait-a-while)
  (let ((outputs (let ((*standard-output* (make-broadcast-stream)))
                   (with-output-to-string (*trace-output*)
                     (supertrace/tests/main/test-package:wait-a-while)))))
    (ok (scan "^running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\)
3\\d{2}\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\) -> nil
$" outputs)))

  (testing "Nested case"
    (supertrace supertrace/tests/main/test-package:wait-a-while2)
    (let ((outputs (let ((*standard-output* (make-broadcast-stream)))
                     (with-output-to-string (*trace-output*)
                       (supertrace/tests/main/test-package:wait-a-while2)))))
      (ok (scan "^running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while2\\)
running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\)
30\\d\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\) -> nil
60\\d\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while2\\) -> nil
$" outputs))))

  (testing "Multiple threads"
    (let* ((output-stream (make-string-output-stream))
           (bt:*default-special-bindings* `((*trace-output* . ,output-stream)
                                            (*standard-output* . ,(make-broadcast-stream))))
           threads)
      (push
        (bt:make-thread
          (lambda ()
            (supertrace/tests/main/test-package:wait-a-while))
          :name "wait-a-while 1")
        threads)
      (sleep 0.1)
      (push
        (bt:make-thread
          (lambda ()
            (supertrace/tests/main/test-package:wait-a-while))
          :name "wait-a-while 2")
        threads)

      (mapc #'bt:join-thread threads)
      (let ((outputs (get-output-stream-string output-stream)))
        (ok (scan "^running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\)
running <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\)
30\\d\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\) -> nil
30\\d\\.\\d{3}ms <SUPERTRACE/TESTS/MAIN/TEST-PACKAGE> \\(wait-a-while\\) -> nil
$" outputs))))))
