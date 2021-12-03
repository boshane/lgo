;; game-logic.lisp
;;
;;

(in-package #:lgo)

(defun print-array (arr)
  (let ((array-size (array-dimensions arr)))
    (destructuring-bind (m n) array-size
      (loop for i from 0 below m do
        (loop for j from 0 below n do
          (format *debug-io* "~A " (aref arr i j)))
        (terpri *debug-io*)))))

(defun occupied? (vert arr &optional player)
  "Is the field at vertices VERT in board array ARR occupied? If so, occupied by PLAYER?"
    (and (stone (aref arr (vy vert) (vx vert)))
         (if player
             (and (equalp player (player (stone (aref arr (vy vert) (vx vert)))))
                vert)
             vert)))

(defun get-adjacent-to (field arr)
  "Find any adjacent stones surrounding field FIELD in array ARR"
  (let* ((field-vertices (vertices field))
         (field-player (player (stone field)))
         (adj-verts (adjacent-vertices field-vertices)))
    (mapcan #'(lambda (x)
                (when (occupied? (aref arr (vertices-y x) (vertices-x x)) arr field-player)
                  (list x)))
            adj-verts)))

(defun adjacent-vertices (vert)
  "Return a list of valid vertices surrounding vertices VERT"
  (let* ((x (vertices-x vert))
         (y (vertices-y vert))
         (verts (list (v (1- x) y)
                      (v (1+ x) y)
                      (v x (1- y))
                      (v x (1+ y))
                      (v (1- x) (1- y))
                      (v (1+ x) (1+ y))
                      (v (1- x) (1+ y))
                      (v (1+ x) (1- y)))))
    ;; Remove vertices below zero
    ;; TODO: Add MAX parameter and remove vertices above
    (mapcan #'(lambda (x) (unless (or (eq -1 (vertices-x x))
                                      (eq -1 (vertices-y x)))
                            (list x)))
            verts)))

(defun single-or-group (vert arr)
  (let ((adjacent-list (adjacent-takenp vert arr)))
    (when adjacent-list
      (let ((group?
              (if (listp adjacent-list)
                  (mapcan #'(lambda (x) (list (vertices-group x))) adjacent-list)
                  (vertices-group adjacent-list))))
        group?))))
