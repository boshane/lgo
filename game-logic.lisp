;; game-logic.lisp
;;
;;

(in-package #:lgo)

(defstruct (vertices (:constructor v (x y &optional group)))
  x
  y
  group)

(defun print-array (arr)
  (let ((array-size (array-dimensions arr)))
    (destructuring-bind (m n) array-size
      (loop for i from 0 below m do
        (loop for j from 0 below n do
          (format *debug-io* "~A " (aref arr i j)))
        (terpri *debug-io*)))))

(defun occupied-adjacent (vert arr)
  (let ((x (vertices-x vert))
        (y (vertices-y vert)))
    (and (not (zerop (aref arr y x)))
         vert)))

(defun adjacent-takenp (vert arr)
  (let ((adj-verts (adjacent-vertices vert)))
    (mapcan #'(lambda (x) (when (occupied-adjacent x arr) (list x))) adj-verts)))

(defun adjacent-vertices (vert)
  (let ((x (vertices-x vert))
        (y (vertices-y vert)))
    (list (v (1- x) y)
          (v (1+ x) y)
          (v x (1- y))
          (v x (1+ y))
          (v (1- x) (1- y))
          (v (1+ x) (1+ y))
          (v (1- x) (1+ y))
          (v (1+ x) (1- y)))))

(defun single-or-group (vert arr)
  (let ((adjacent-list (adjacent-takenp vert arr)))
    (when adjacent-list
      (let ((group?
              (if (listp adjacent-list)
                  (mapcan #'(lambda (x) (list (vertices-group x))) adjacent-list)
                  (vertices-group adjacent-list))))
        group?))))
