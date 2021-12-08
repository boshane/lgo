;;;; tests/package.lisp
;;;;

(defpackage #:lgo-tests
  (:use #:cl #:fiveam)
  (:export #:run!
           #:all-tests
           #:test-lgo))
