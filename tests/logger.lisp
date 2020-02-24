(defpackage #:supertrace/tests/logger
  (:use #:cl
        #:rove
        #:supertrace/logger))
(in-package #:supertrace/tests/logger)

(defpackage #:supertrace/tests/logger/test-package
  (:export #:say-hello))
(defun supertrace/tests/logger/test-package:say-hello (to)
  (format t "~&Hello, ~A!~%" to))

(deftest ensure-function-name
  (ok (equal (ensure-function-name 'supertrace/tests/logger/test-package:say-hello)
             "say-hello")
      "symbol")
  (ok (equal (ensure-function-name "SAY-HELLO") "say-hello")
      "string"))

(deftest ensure-function-package
  (ok (eq (ensure-function-package 'supertrace/tests/logger/test-package:say-hello)
          (find-package :supertrace/tests/logger/test-package))
      "symbol")
  (ok (eq (ensure-function-package "say-hello") *package*)
      "string"))

(deftest elapsed-logger
  (ok (outputs (funcall #'elapsed-logger
                        'supertrace/tests/logger/test-package:say-hello
                        '("World"))
               (format nil "running <SUPERTRACE/TESTS/LOGGER/TEST-PACKAGE> (say-hello \"World\")~%")
               *trace-output*)
      "for before logger")
  (ok (outputs (funcall #'elapsed-logger
                        'supertrace/tests/logger/test-package:say-hello
                        '("World")
                        nil
                        120)
               (format nil "0.120ms <SUPERTRACE/TESTS/LOGGER/TEST-PACKAGE> (say-hello \"World\") -> nil~%")
               *trace-output*)
      "for after logger"))
