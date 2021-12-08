;;;; tests/main.lisp
;;;;

(in-package #:lgo-tests)

(defparameter *board-size* 10)
(defparameter *full-board* (lgo::make-board *board-size*))

;; Populate a test board in alternating fields as below
;; ^ * ^ * ^ * ^ * ^ *
;; * ^ * ^ * ^ * ^ * ^
;; ^ * ^ * ^ * ^ * ^ *
;; * ^ * ^ * ^ * ^ * ^
;; ^ * ^ * ^ * ^ * ^ *
;; * ^ * ^ * ^ * ^ * ^
;; ^ * ^ * ^ * ^ * ^ *
;; * ^ * ^ * ^ * ^ * ^
;; ^ * ^ * ^ * ^ * ^ *
;; * ^ * ^ * ^ * ^ * ^
;;
(defun populate-board ()
  (loop for i from 0 below *board-size* do
    (loop for j from 0 below *board-size* do
      (setf (lgo::stone (aref *full-board* i j))
            (if (zerop (mod (+ j i) 2))
                (make-instance 'lgo::stone :player :black)
                (make-instance 'lgo::stone :player :white))))))

(def-suite all-tests
  :description "All the lgo tests, to be broken up later...")

(in-suite all-tests)

(defun test-lgo ()
  (populate-board)
  (run! 'all-tests))

(test dummy-tests
  (is (eq (length (lgo::find-group (aref *full-board* 1 1) *full-board* :black)) 50))
  (is (eq (length (lgo::find-group (aref *full-board* 1 2) *full-board* :white)) 50)))
