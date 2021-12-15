;; SGF file lexer
;;
;;

(in-package :lgo)

(defconstant +sgf-version+ 4)
(setf +sgf-tokens+
  '(("[\]]" rbrack)
    ("[\[]" lbrack)
    ("[\(]" lparen)
    ("[\)]" rparen)
    ("[\;]" semicolon)
    ("[\:]" colon)
    ("[a-zA-Z]+" property-name)))

;; PROPERTY VALUE TYPE FUNCTION
(defvar *properties*
  '((b :sgf-move :move :nil)
    (ko :nil :move :nil)
    (mn :sgf-number :move :nil)
    (w :sgf-move :move :nil)
    (ab :sgf-stone-list :setup :nil)
    (aw :sgf-point-list :setup :nil)
    (pl :sgf-color :setup :nil)
    (c :sgf-text :nil :nil)
    (dm :sgf-double :nil :nil)
    (gb :sgf-double :nil :nil)
    (ho :sgf-double :nil :nil)
    (n :simple-text :nil :nil)
    (ff :number :root :nil)
    (gm :number :root :nil)
    (st :number :root :nil)))

(defvar *value-constraints*
  '((:ff (1 4))
    (:gm (1 16))
    (:st (0 3))))

(defmacro casepr (keyform &rest cases)
  "Wrapper for 'case' macro: test KEYFORM against CASES but print the car of a matching case."
    (let ((test (gensym)))
      `(let ((test ,keyform))
         (declare (ignorable test))
         (case test
         ,@(loop for (test prog) in cases
                 collect (cons test (list (progn '(prin1 test))
                                          prog)))))))

(defstruct sgf-move
  move)
(defstruct sgf-number
  number)
(defstruct sgf-stone-list
  stone-list)
(defstruct sgf-point-list
  point-list)
(defstruct sgf-color
  color)
(defstruct sgf-text
  text)
(defstruct sgf-simple-text
  simple-text)
(defstruct sgf-double
  double)

(defclass sgf-property ()
  ((property :initarg :property :reader property)
   (property-value :initarg :proerty-value :reader property-value)
   (property-type :initarg :proerty-type :reader property-type)
   (property-function :initarg :proerty-function :reader property-function)))

(defclass sgf-lexer ()
  ((source :initarg :source :reader source)
   (tree-depth :initform 0 :reader tree-depth)
   (pos    :initarg :pos :reader pos)
   (len    :initarg :len :reader len)
   (end    :initarg :end :reader end)
   (regex-table :initarg :regex-table :reader regex-table)))

(defclass sgf-node ()
  ((property :initarg :property :reader property)))

(defun make-sgf-lexer (string &key ((:pos pos) 0))
  (make-instance 'sgf-lexer :pos pos :source string))

(defmethod initialize-instance :after ((lexer sgf-lexer) &key &allow-other-keys)
  (with-slots (source len pos end regex-table)
      lexer
      (let ((len (length source))
            (rtable (loop for (regex . token) in (loop for (regex token) in +sgf-tokens+
                                                       collect (cons regex token))
                          for sexp = (cl-ppcre::parse-string regex)
                          for anchor = (cl-ppcre:create-scanner `(:sequence
                                                             :modeless-start-anchor
                                                             ,sexp))
                          collect (cons anchor token))))
        (unless pos (setf pos 0))
        (setf end len)
        (assert (<= 0 pos end len))
        (setf regex-table rtable))))

(defmethod advance-position ((lexer sgf-lexer))
  (setf (slot-value lexer 'pos) (1+ (slot-value lexer 'pos))))

(defmethod get-next-token ((lexer sgf-lexer))
  (let ((token (loop for (regex . sym) in (regex-table lexer)
        for next = (nth-value 1 (cl-ppcre:scan regex (source lexer) :start (pos lexer)))
        when next do (return (cons sym next)))))
    (if token
        token
        (signal 'unknown-token :unknown-token :unknown))))

(defun print-depth (depth)
  (terpri)
  (dotimes (n depth)
    (format t "-"))
  (format t "> "))

(defmethod read-property ((lexer sgf-lexer))
  (let ((tok (get-next-token lexer)))
    (and (assoc tok *properties*)
         (print tok))))

(define-condition unknown-token (error)
  ((%unknown-token :reader :unknown-token :initarg :unknown-token )))

(defun skip-token (condition)
  (declare (ignore condition))
  (format t "Token skipped~%")
  (throw :do-not-parse nil))

(defun parse-sgf-string (lexer &key ((:expect expect) nil))
  (let ((current-position (pos lexer))
        (depth (tree-depth lexer)))
    (unless (equal (end lexer) current-position)
      (let* ((tok-sym (handler-bind ((unknown-token #'skip-token))
                        (catch :do-not-parse
                          (get-next-token lexer))))
             (next-pos (cdr tok-sym)))
        (and tok-sym
             (case (car tok-sym)
               (lparen (setf (slot-value lexer 'tree-depth) (1+ depth)))
               (rparen (setf (slot-value lexer 'tree-depth) (1- depth)))
               (semicolon (print tok-sym))
               (property-name (print 'property))
               (colon (print tok-sym))
               (t t))
             (setf (slot-value lexer 'pos) next-pos)
             (parse-sgf-string lexer))))))
