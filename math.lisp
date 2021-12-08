;; math.lisp
;; Vector and matrix helper functions
;;

(defun mat-multiply (m1 m2)
  (let ((dimensions-a1 (array-dimensions m1))
        (dimensions-a2 (array-dimensions m2)))
   (list dimensions-a1 dimensions-a2)))
