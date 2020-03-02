(include "time.h")

(in-package #:supertrace/clock)

(ctype time-t "time_t")
(ctype clockid-t "clockid_t")

(constant (+clock-monotonic+ "CLOCK_MONOTONIC"))
(constant (+clock-realtime+ "CLOCK_REALTIME"))
