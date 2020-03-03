(defpackage #:supertrace
  (:nicknames #:supertrace/main)
  (:use #:cl)
  (:import-from #:supertrace/logger
                #:elapsed-logger)
  #+unix
  (:import-from #:supertrace/clock
                #:clock-gettime)
  (:export #:supertrace
           #:elapsed-logger))
(in-package #:supertrace)

(defparameter *before-timings-unixtime*
  (make-hash-table :test 'eq))
(defparameter *before-timings-usec*
  (make-hash-table :test 'eq))

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

(declaim (inline get-timings))
(defun get-timings ()
  #+unix (multiple-value-bind (sec nsec)
             (clock-gettime)
           (declare (fixnum nsec)
                    (optimize (speed 3) (safety 0) (debug 0)))
           (values sec (floor nsec 1000)))
  #-unix (sb-ext:get-time-of-day))

(defmacro supertrace (&rest names-and-options)
  (multiple-value-bind (options function-names)
      (parse-supertrace-options names-and-options)
    (let ((function-names (or function-names
                              ;; If no function/package names are supplied, trace all functions in the current package.
                              `((package ,(package-name *package*) :internal t))))
          (frame (gensym "FRAME"))
          (info (gensym "INFO"))
          (form (gensym "FORM"))
          (unixtime (gensym "UNIXTIME"))
          (usec (gensym "USEC"))
          (elapsed (gensym "ELAPSED")))
      (destructuring-bind (&key (before ''elapsed-logger) (after ''elapsed-logger) threshold)
          options
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
                                            (get-timings)
                                          (push ,unixtime (gethash sb-thread:*current-thread* *before-timings-unixtime*))
                                          (push ,usec (gethash sb-thread:*current-thread* *before-timings-usec*))))
                                 t)
                :break-after (progn
                               ,(and after
                                     `(let ((,frame (find-trace-call-frame)))
                                        (when (null ,frame)
                                          (error "Failed to find sb-debug::trace-call in stacktraces"))
                                        (destructuring-bind (,info &rest ,form)
                                            (nth-value 1 (sb-debug::frame-call ,frame))
                                          (multiple-value-bind (,unixtime ,usec)
                                              (get-timings)
                                            (declare (fixnum ,unixtime ,usec))
                                            (let ((,elapsed (locally (declare (optimize (speed 3) (safety 0) (debug 0)))
                                                              (+ (* 1000000 (- ,unixtime (the fixnum (pop (gethash sb-thread:*current-thread* *before-timings-unixtime*)))))
                                                                 (- ,usec (the fixnum (pop (gethash sb-thread:*current-thread* *before-timings-usec*))))))))
                                              (when ,(if threshold
                                                         `(< ,threshold ,elapsed)
                                                         t)
                                                (funcall ,after
                                                         (sb-debug::trace-info-what ,info)
                                                         (ensure-printable (rest ,form))
                                                         (sb-debug:arg 0)
                                                         ,elapsed)))))))
                               nil)
                ,@(expand-function-names function-names))))))
