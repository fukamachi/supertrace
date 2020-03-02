(in-package #:supertrace/clock)

(cffi:defcstruct timespec
  (tv-sec time-t)
  (tv-nsec :long))

(cffi:defcfun ("clock_gettime" %clock-gettime) :int
  (clk-id clockid-t)
  (tp :pointer))
