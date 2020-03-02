(defpackage #:supertrace/clock
  (:use #:cl
        #:cffi-grovel)
  (:export #:clock-gettime
           #:+clock-monotonic+
           #:+clock-realtime+))
(in-package #:supertrace/clock)
