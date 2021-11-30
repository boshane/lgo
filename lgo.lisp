;;;; lgo - Go board game in Common Lisp and McClim
;;;; Dan Beauchesne
;;;; 2021-11-23

(in-package #:lgo)

(defparameter *window-size* 10)
(defparameter *default-size* 10)
(defparameter *square-size* 72)
(defparameter *black-char* #\*)
(defparameter *white-char* #\^)

(defclass stone ()
  ((player :initarg :player :reader player)
   (field :initarg :field :accessor field)))

(defclass field ()
  ((col :initarg :col :reader col)
   (row :initarg :row :reader row)
   (stone :initform nil :accessor stone)))

(defun print-board-array (board-array)
  (let ((array-size (array-dimensions board-array)))
    (destructuring-bind (m n) array-size
      (loop for i from (1- m) downto 0 do ;; Clim field index rows from the bottom of the pane
                                          ;; so we have to flip it to print the grid
        (loop for j from 0 below n do
          (format *debug-io* "~A " (char-from-field (aref board-array i j))))
        (terpri *debug-io*)))))

(defun make-board (size)
  (let* ((x size)
         (y x)
         (fields (make-array (list x y))))
    (dotimes (row x)
      (dotimes (col y)
        (let ((field (make-instance 'field :row row :col col)))
          (setf (aref fields row col) field))))
    fields))

(defun char-from-field (field)
  (let ((field-stone (stone field)))
    (if field-stone
        (ecase (player field-stone)
          (:player-black *black-char*)
          (:player-white *white-char*))
        #\_)))

(defun toggle-player (game-state)
  (setf (player game-state)
        (ecase (player game-state)
          (:player-black :player-white)
          (:player-white :player-black))))

(defun place-stone (game-state dst-field)
  (if (not (stone dst-field))
      (let* ((current-player (player game-state))
             (new-stone (make-instance 'stone :player current-player :field dst-field)))
        (setf (stone dst-field) new-stone)
        (toggle-player game-state)
        (format *debug-io* "~A places stone at ~A ~A~%" current-player (col dst-field) (row dst-field))
        (print-board-array (field-array (board game-state))))
      (format *debug-io* "Stone already exists at ~A, ~A~%" (col dst-field) (row dst-field))))

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

(defmethod activep ((field field))
  (and (boundp '*application-frame*)
       (eql field (active-field *application-frame*))))

(defun player-color (player-name)
  (ecase player-name
  (:player-black +black+)
  (:player-white +white+)))

(defun switch-player (game-state)
  (setf (player game-state)
        (ecase (player game-state)
          (:player-black :player-white)
          (:player-white :player-black))))

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
                  :background +grey80+
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

(defun valid-move-p (game-state stone dst-field)
  t)

(defun display (frame pane)
  (with-first-quadrant-coordinates (pane 0 (* *window-size* *square-size*))
    (with-scaling (pane *square-size* *square-size*)
              (present nil 'board :stream pane :single-box t))))

(define-lgo-command (command-quit :name t) ()
  (frame-exit *application-frame*))

(defun test-field (dst-field &rest args)
  (declare (ignore args))
  t)

(define-lgo-command (command-select-field :name t)
    ((dst-field 'field :gesture (:select :tester test-field)))
  (let ((frame *application-frame*))
    (place-stone frame dst-field)))

;; Presentation for the stones
(define-presentation-method present
  ((stone stone) (type stone) stream (view board-view) &key)
  (let* ((stone-color (player-color (player stone))))
    (draw-circle* stream (col (field stone)) (row (field stone)) .45 :ink stone-color)))

;; Presentation for the fields
(define-presentation-method present
    ((field field) (type field) stream (view board-view) &key)
  (let* ((x (col field))
         (y (row field))
         (stone (stone field)))
    (if stone
        (present stone 'stone :view view)
        (draw-circle* stream x y .20 :ink +transparent-ink+))))

;; Presentation for the board itself
(define-presentation-method present
    (object (type board) stream (view board-view) &key)
  (let* ((ink +burlywood3+)
         (board-size (size (board *application-frame*)))
         (table-size (1- board-size)))
    (with-first-quadrant-coordinates (stream 0 (* (1+ board-size) *square-size*)))
    (with-translation (stream 0.5 0.5)
      (draw-rectangle* stream 0 0 table-size table-size :ink ink)
      (dotimes (row board-size)
        (dotimes (col board-size)
          ;; Draw the grid lines
          (cond ((zerop row) (draw-line* stream col row col (+ row table-size)))
                ((zerop col) (draw-line* stream col row (+ col table-size) row)))
          (present (get-field *application-frame* row col) 'field :view view))))))

(defparameter *frame* nil)
(defun lgo-ui ()
  (setf *frame* (make-application-frame 'lgo))
  (run-frame-top-level *frame*))
