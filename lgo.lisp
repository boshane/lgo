;;;; lgo - Go board game in Common Lisp and McClim
;;;; Dan Beauchesne
;;;; 2021-11-23
;;

(in-package #:lgo)

(defparameter *window-size* 20)
(defparameter *default-size* 10)
(defparameter *square-size* 36)
(defparameter *black-char* #\*)
(defparameter *white-char* #\^)
(defparameter *board-labels*
  (map 'list #'princ-to-string
       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defstruct (vertices (:constructor v (x y)))
  x
  y)

(defun vy (field)
  (vertices-y (vertices field)))

(defun vx (field)
  (vertices-x (vertices field)))

(defclass group ()
  ((members :initarg :members :reader members)
   (num :initarg :num :initform 1 :accessor num)))

(defclass stone ()
  ((player :initarg :player :reader player)
   (field :initarg :field :accessor field)))

(defclass field ()
  ((vertices :initarg :vertices :accessor vertices)
   (stone :initform nil :accessor stone)))

(defun print-board-array (board-array)
  (let ((array-size (array-dimensions board-array)))
    (destructuring-bind (m n) array-size
      (loop for i from (1- m) downto 0 do
        (loop for j from 0 below n do
          (format *debug-io* "~A " (char-from-field (aref board-array i j))))
        (terpri *debug-io*)))))

(defun make-board (size)
  "Generate a board of size SIZExSIZE and populate it with empty field objects."
  (let* ((x size)
         (y x)
         (fields (make-array (list x y))))
    (dotimes (row x)
      (dotimes (col y)
        (let ((field (make-instance 'field :vertices (v row col))))
          (setf (aref fields row col) field))))
    fields))

(defun char-from-field (field)
  (let ((field-stone (stone field)))
    (if field-stone
        (ecase (player field-stone)
          (:black *black-char*)
          (:white *white-char*))
        #\_)))

(defun toggle-player (game-state)
  (setf (current-player game-state)
        (ecase (current-player game-state)
          (:black :white)
          (:white :black))))

(defun update-board-array ())

(defun get-field (game-state row col)
  (let ((array (field-array (board game-state))))
    (when (array-in-bounds-p array row col)
      (aref array row col))))

(defclass game-state ()
  ((board :initarg :board :reader board)
   (locked :initarg :locked :accessor locked)
   (game-over :initarg :game-over :accessor :game-over)
   (current-player :initarg :current-player :accessor current-player)
   (black :initarg :black :accessor black)
   (white :initarg :white :accessor white))
  (:default-initargs :board (make-instance 'board)
                     :black (make-instance 'player)
                     :white (make-instance 'player)
                     :current-player :black
                     :locked nil
                     :game-over nil))

(defclass player ()
  ((groups :initarg :groups :initform nil :accessor groups)
   (color :initarg :color :accessor color)))

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
  (:black +black+)
  (:white +white+)))

(defun switch-player (game-state)
  (setf (player game-state)
        (ecase (player game-state)
          (:black :white)
          (:white :black))))

(defun new-game (game-state)
  (with-slots (board player locked game-over active-field)
      game-state
    (setf board (make-board (size (board (game-state))))
          player :player-black
          locked nil
          game-over nil
         active-field nil)))

(define-presentation-type special-page ())

(define-application-frame lgo (standard-application-frame game-state)
  ((selected-stone :initform nil))
  (:panes
   (vs-player :application
              :display-function 'display
              :scroll-bars nil
              :background +grey80+
              :default-view (make-instance 'board-view))
   (debug :application :display-function #'display-debug
          :display-time :command-loop)
   (pointer-doc :pointer-documentation))
  (:layouts
   (default
    (vertically (:width 800 :height 600)
      vs-player
      (with-tab-layout ('tab-page :name 'info-tabs)
        ("Debug info" debug))
      pointer-doc))))

(defun print-group-verts (verts)
    (mapcar #'(lambda (x) (format t "[~A,~A]" (vertices-x x) (vertices-y x))) verts))

(defun display-debug (frame pane)
  (and (slot-value frame 'selected-stone)
    (let* ((*standard-output* pane)
           (current-stone (slot-value frame 'selected-stone))
           (current-field (field current-stone))
           (current-player (player current-stone))
           (group (find-group current-field (field-array (board *application-frame*)) current-player)))
      (surrounding-output-with-border (pane)
        (formatting-table (pane :x-spacing 125)
          (formatting-row (pane)
            (formatting-cell (pane) (format pane "Location"))
            (formatting-cell (pane) (format pane "Group")))
          (formatting-row (pane)
            (formatting-cell (pane)
              (when current-stone
                (format pane "   ~A ~A" (vx (field current-stone)) (vy (field current-stone)))))
            (formatting-cell (pane)
              (and group
                   (print-group-verts group)))))))))

(defun winnerp (game-state)
  nil)

(defun valid-move-p (game-state stone dst-field)
  t)



(defun test-field (dst-field &rest args)
  (declare (ignore args))
  t)

(define-lgo-command (command-select-stone :name t)
    ((stone 'stone :gesture (:select :tester test-field)))
  (setf (slot-value *application-frame* 'selected-stone) stone))

(define-lgo-command (command-quit :name t) ()
  (frame-exit *application-frame*))

(define-lgo-command (command-select-field :name t)
    ((dst-field 'field :gesture (:select :tester test-field)))
  (let ((frame *application-frame*))
    (place-stone frame dst-field)))

;; Presentation for the stones
(define-presentation-method present
  ((stone stone) (type stone) stream (view board-view) &key)
  (let* ((stone-color (player-color (player stone)))
         (cur-field (field stone))
         (x (vx cur-field))
         (y (vy cur-field)))
    (draw-circle* stream x y .45 :ink stone-color)))

;; Presentation for the fields
(define-presentation-method present
    ((field field) (type field) stream (view board-view) &key)
  (let* ((x (vx field))
         (y (vy field))
         (stone (stone field)))
    (if stone
        (present stone 'stone :view view)
        (draw-circle* stream x y .20 :ink +transparent-ink+))))

(defun display (frame pane)
  (with-first-quadrant-coordinates (pane *square-size* (* (1+ *default-size*) *square-size*))
    (with-scaling (pane 32 32)
      (with-translation (pane 2 1)
        (present nil 'board :stream pane :single-box t)))))

;; Presentation for the board itself
(define-presentation-method present
    (object (type board) stream (view board-view) &key)
  (let* ((ink +burlywood3+)
         (board-size (size (board *application-frame*)))
         (table-size (1- board-size)))
    (draw-rectangle* stream -1 -1
                     (1+ table-size)
                     (1+ table-size) :ink ink)
    (dotimes (rows *default-size*)
      (dotimes (cols *default-size*)
        (and (zerop cols)
             (draw-line* stream rows 0 rows table-size))
        (and (zerop rows)
             (draw-line* stream 0 cols table-size cols))))
    ;; TODO: Do this more elegantly, maybe with proper text alignment at least
    (with-translation (stream -.08 -.9)
      (loop for x from 0 upto table-size
            do (draw-text* stream (nth x *board-labels*) x 0)))
    (with-translation (stream -.9 -.1)
      (loop for y from 0 upto table-size
            do (draw-text* stream (nth y *board-labels*) 0 y)))
    (dotimes (i board-size)
      (dotimes (j board-size)
        (present (get-field *application-frame* i j) 'field :view view)))))

(defparameter *frame* nil)
(defun lgo-ui ()
  (setf *frame* (make-application-frame 'lgo))
  (run-frame-top-level *frame*))
