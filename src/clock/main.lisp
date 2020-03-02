(in-package #:supertrace/clock)

(defun clock-gettime (&optional (clock-id +clock-monotonic+))
  (cffi:with-foreign-object (timespec '(:pointer (:struct timespec)))
    (%clock-gettime clock-id timespec)
    (values (cffi:foreign-slot-value timespec '(:struct timespec) 'tv-sec)
            (cffi:foreign-slot-value timespec '(:struct timespec) 'tv-nsec))))
