(defpackage #:supertrace/tests/main
  (:use #:cl
        #:rove
        #:supertrace/main)
  (:import-from #:supertrace/main
                #:parse-supertrace-options))
(in-package #:supertrace/tests/main)

(deftest parse-supertrace-options
  (flet ((fut (args)
           (multiple-value-list (parse-supertrace-options args))))
    (ok (equal (fut '()) '(() ())))
    (ok (equal (fut '("DBI:PREPARE"))
               '(() ("DBI:PREPARE"))))
    (ok (equal (fut '("DBI:PREPARE" "DBI:EXECUTE"))
               '(() ("DBI:PREPARE" "DBI:EXECUTE"))))
    (ok (equal (fut '(:before #1=(lambda (&rest args)
                                   (warn "before ~S" args))
                      "DBI:PREPARE" "DBI:EXECUTE"))
               '((:before #1#) ("DBI:PREPARE" "DBI:EXECUTE"))))
    (ok (equal (fut '(:after #2=(lambda (&rest args)
                                  (warn "after ~S" args))
                      "DBI:PREPARE" "DBI:EXECUTE"))
               '((:after #2#) ("DBI:PREPARE" "DBI:EXECUTE"))))
    (ok (equal (fut '(:before #1# :after #2#
                      "DBI:PREPARE" "DBI:EXECUTE"))
               '((:before #1# :after #2#) ("DBI:PREPARE" "DBI:EXECUTE"))))
    (ok (equal (fut '("DBI:PREPARE" "DBI:EXECUTE"
                      :before #1# :after #2#))
               '((:before #1# :after #2#) ("DBI:PREPARE" "DBI:EXECUTE"))))))
