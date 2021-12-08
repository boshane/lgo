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

(defun place-stone (game-state dst-field &optional player)
  (let ((stone-at-field (stone dst-field))
        (current-player (if player
                            player
                            (current-player game-state))))
    (if (not stone-at-field)
        (let* ((new-stone (make-instance 'stone :player current-player :field dst-field))
              (dst-row (vy dst-field))
              (dst-col (vx dst-field)))
          (setf (stone dst-field) new-stone)
          (toggle-player game-state)))))

(defun occupied? (vert arr &optional player)
  "Is the field at vertices VERT in board array ARR occupied? If so, occupied by PLAYER?"
  (let ((field-at-vert (aref arr (vertices-x vert) (vertices-y vert))))
    (and (stone field-at-vert)
         (if player
             (and (equalp player (player (stone field-at-vert)))
                vert)
             vert))))

(defun adjacent-vertices (vert &optional (max 9))
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
    (mapcan #'(lambda (x) (unless (or (eq -1 (vertices-x x))
                                      (eq -1 (vertices-y x))
                                      (> (vertices-x x) max)
                                      (> (vertices-y x) max))
                            (list x)))
            verts)))

;; TODO: Find a more lisp-y way of doing this (recursive, LOOP, ?)
;;
(defun find-group (start-node arr player)
  "Find groups of nodes starting with node START-NODE in array ARR."
  (let ((nodes '())
        (start-vertices (vertices start-node)))
    (flet ((member-node (node)
             (member node nodes :test #'(lambda (x y)
                                          (and (eq (vertices-x x) (vertices-x y))
                                               (eq (vertices-y x) (vertices-y y)))))))
      (let ((pending-nodes '()))
        (labels ((do-next-node (next)
                   (and next
                        (let ((node-vertices (adjacent-vertices next)))
                          (dolist (n node-vertices)
                            (and (occupied? n arr player)
                                 (unless (member-node n)
                                   (push n nodes))
                                 (push n pending-nodes)))
                          (do-next-node (pop pending-nodes))))))
          (do-next-node start-vertices))))
    nodes))
