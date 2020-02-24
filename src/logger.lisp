(defpackage #:supertrace/logger
  (:use #:cl)
  (:export #:elapsed-logger
           #:ensure-function-name
           #:ensure-function-package))
(in-package #:supertrace/logger)

(defun ensure-function-name (name)
  (etypecase name
    (string (string-downcase name))
    (symbol (let ((*print-case* :downcase))
              (princ-to-string name)))))

(defun ensure-function-package (name)
  (etypecase name
    (string *package*)
    (symbol (symbol-package name))))

(defun elapsed-logger (name args &optional (retval nil retval-supplied-p) elapsed)
  (let ((*print-case* :downcase))
    (format *trace-output*
            "~&~:[running~;~:*~,3Fms~] <~A> (~A~{ ~S~})~:[~*~; -> ~S~]~%"
            (and elapsed (/ elapsed 1000d0))
            (package-name (ensure-function-package name))
            (ensure-function-name name)
            args
            retval-supplied-p
            retval)))
