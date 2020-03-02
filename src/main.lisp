(defpackage #:supertrace
  (:nicknames #:supertrace/main)
  (:use #:cl)
  (:import-from #:supertrace/logger
                #:elapsed-logger)
  (:import-from #:alexandria
                #:once-only
                #:with-gensyms)
  (:export #:supertrace
           #:elapsed-logger))
(in-package #:supertrace)

(defparameter *before-unixtime* nil)
(defparameter *before-usec* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-supertrace-options (args)
    (loop while args
          for arg = (pop args)
          if (and args
                  (member arg '(:before :after :threshold)))
          append (list arg (pop args)) into options
          else collect arg into names
          finally (return (values options names)))))

(declaim (inline find-trace-call-frame))
(defun find-trace-call-frame ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (do ((frame (sb-di:top-frame) (sb-di:frame-down frame)))
      ((or (null frame)
           (eq 'sb-debug::trace-call (sb-debug::frame-call frame))) frame)
    (declare (type (or sb-di:frame null) frame))))

(defun ensure-printable (args)
  (if (consp args)
      (mapcar #'sb-debug::ensure-printable-object args)
      args))

(defun expand-function-names (names)
  (loop for name in names
        if (and (consp name)
                (eq (first name) 'package))
        append (let ((symbols '()))
                 (destructuring-bind (package-name &key internal)
                     (rest name)
                   (do-external-symbols (symb package-name)
                     (when (and (fboundp symb)
                                (not (macro-function symb)))
                       (push symb symbols)))
                   (when internal
                     (do-symbols (symb package-name)
                       (when (and (eq (symbol-package symb) (find-package package-name))
                                  (fboundp symb)
                                  (not (macro-function symb)))
                         (push symb symbols)))))
                 (nreverse symbols))
        collect name))

(defmacro supertrace (&rest names-and-options)
  (multiple-value-bind (options function-names)
      (parse-supertrace-options names-and-options)
    (let ((function-names (or function-names
                              ;; If no function/package names are supplied, trace all functions in the current package.
                              `((package ,(package-name *package*) :internal t)))))
      (destructuring-bind (&key (before ''elapsed-logger) (after ''elapsed-logger) threshold)
          options
        (with-gensyms (frame info form unixtime usec elapsed)
          `(trace :report ,(if (or before after)
                               nil
                               'trace)
                  :condition-all (progn
                                   ,(and before
                                         `(let ((,frame (find-trace-call-frame)))
                                            (when (null ,frame)
                                              (error "Failed to find sb-debug::trace-call in stacktraces"))
                                            (destructuring-bind (,info &rest ,form)
                                                (nth-value 1 (sb-debug::frame-call ,frame))
                                              (funcall ,before
                                                       (sb-debug::trace-info-what ,info)
                                                       (ensure-printable (rest ,form))))))
                                   ,(and after
                                         `(multiple-value-bind (,unixtime ,usec)
                                              (sb-ext:get-time-of-day)
                                            (push ,unixtime *before-unixtime*)
                                            (push ,usec *before-usec*)))
                                   t)
                  :break-after (progn
                                 ,(and after
                                       `(let ((,frame (find-trace-call-frame)))
                                          (when (null ,frame)
                                            (error "Failed to find sb-debug::trace-call in stacktraces"))
                                          (destructuring-bind (,info &rest ,form)
                                              (nth-value 1 (sb-debug::frame-call ,frame))
                                            (multiple-value-bind (,unixtime ,usec)
                                                (sb-ext:get-time-of-day)
                                              (let ((,elapsed (+ (* 1000000 (- ,unixtime (pop *before-unixtime*)))
                                                                 (- ,usec (pop *before-usec*)))))
                                                (when ,(if threshold
                                                           `(< ,threshold ,elapsed)
                                                           t)
                                                  (funcall ,after
                                                           (sb-debug::trace-info-what ,info)
                                                           (ensure-printable (rest ,form))
                                                           (sb-debug:arg 0)
                                                           ,elapsed)))))))
                                 nil)
                  ,@(expand-function-names function-names)))))))
