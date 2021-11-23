;;;; lgo - Go board game in common lisp
;;;; Dan Beauchesne
;;;; 2021-11-23

(in-package #:lgo)

(defparameter *window-size* 10)
(defparameter *default-size* 10)
(defparameter *square-size* 72)

(defun make-board (size)
  (let* ((x size)
         (y x)
         (fields (make-array (list x y))))
    (dotimes (row x)
      (dotimes (col y)
        (let ((field (make-instance 'field :row row :col col)))
          (setf (aref fields row col) field))))
    fields))

(defclass stone ()
  ((player :initarg :player :reader player)
   (field :initarg :field :accessor field)))

(defclass field ()
  ((col :initarg :col :reader col)
   (row :initarg :row :reader row)
   (stone :initform nil :accessor stone)))

(defun get-field (game-state row col)
  (let ((array (field-array (board game-state))))
    (when (array-in-bounds-p array row col)
      (aref array row col))))

(defclass game-state ()
  ((board :initarg :board :reader board)
   (player :initarg :player :accessor player)
   (locked :initarg :locked :accessor locked)
   (active-field :initarg :active-field :accessor active-field)
   (game-over :initarg :game-over :accessor :game-over))
  (:default-initargs :board (make-instance 'board)
                     :player :player-black
                     :locked nil
                     :game-over nil))

(defclass board ()
  ((size :initarg :size :initform *default-size* :accessor size)
   (field-array :initarg :field-array :accessor field-array
                :initform (make-board *default-size*))))

(defclass board-view (gadget-view) ())

(defun switch-player (game-state)
  (setf (player game-state)
        (ecase (player game-state)
          (:player-black :player-white)
          (:player-white :player-black))))

(defun player-color (player-name)
  (ecase player-name
  (:player-black +black+)
  (:player-white +white+)))

(defun new-game (game-state)
  (with-slots (board player locked game-over active-field)
      game-state
    (setf board (make-board (size (board (game-state))))
          player :player-black
          locked nil
          game-over nil
          active-field nil)))

(define-application-frame lgo (standard-application-frame game-state)
  ()
  (:pointer-documentation t)
  (:panes (output :application
                  :display-function 'display
                  :scroll-bars nil
                  :width (* *window-size* *square-size*)
                  :min-width (* *window-size* *square-size*)
                  :max-width (* *window-size* *square-size*)
                  :height (* *window-size* *square-size*)
                  :min-height (* *window-size* *square-size*)
                  :max-height (* *window-size* *square-size*)
                  :default-view (make-instance 'board-view))
          (input :application
                 :max-height 128))
  (:menu-bar nil))

(defun winnerp (game-state)
  nil)

(defun display (frame pane)
  (with-first-quadrant-coordinates (pane 0 (* *window-size* *square-size*))
    (with-scaling (pane *square-size* *square-size*)
              (present nil 'board :stream pane :single-box t))))

(define-lgo-command (command-quit :name t) ()
  (frame-exit *application-frame*))

(define-command (com-new-game :name t :command-table lgo) ()
  (new-game *application-frame*))

(define-presentation-method present
    ((field field) (type field) stream (view board-view) &key)
  (let* ((x (col field))
         (y (row field)))
    (draw-circle* stream x y .035 :ink +black+)
    (if (zerop y)
        (draw-line* stream x y x 9))
    (if (zerop x)
        (draw-line* stream x y 9 y))))

(define-presentation-method present (object (type board) stream (view board-view) &key)
  (let* ((ink +burlywood3+)
         (board-size (size (board *application-frame*)))
         (table-size (1- board-size)))
    (with-translation (stream 0.5 0.5)
      (draw-rectangle* stream 0 0 table-size table-size :ink ink)
      (dotimes (row board-size)
        (dotimes (col board-size)
          (present (get-field *application-frame* row col) 'field :view view))))))

(defparameter *frame* nil)
(defun lgo-ui ()
  (setf *frame* (make-application-frame 'lgo))
  (run-frame-top-level *frame*))
