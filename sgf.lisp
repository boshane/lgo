;; SGF file lexer
;;
;;

(in-package :lgo)

(defconstant +sgf-version+ 4)
(defparameter +sgf-tokens+ '(("[\(]" lparen)
                     ("[\)]" rparen)
                     ("[\;]" semicolon)
                     ("[\:]" colon)
                     ("[a-zA-Z]+" property-name)
                     ("\[[a-zA-Z0-9]+\]" parameters)
                     ("." unknown)))

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
                 collect (cons test (list (progn '(format t "~A~%" test))
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
   (active-token    :initarg :active-token :reader active-token)
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
  (handler-bind ((error #'no-next-token-match))
    (catch :cannot-parse
      (let ((token-pos (loop for (regex . sym) in (regex-table lexer)
                             for next = (nth-value 1 (cl-ppcre:scan regex (source lexer) :start (pos lexer)))
                             when next do (return (cons sym next)))))
        (if token-pos
            token-pos
            (signal 'no-match :no-next-token-match :unknown))))))

(define-condition lexer-error (error)
  ((%unknown-token :reader :unknown-token :initarg :unknown-token)
   (%no-match :reader :no-match :initarg :no-match)))

(defun no-next-token-match (condition)
  (declare (ignore condition))
  (format t "NO-REGEX-MATCH~%")
  (throw :cannot-parse nil))

(defun skip-token (condition)
  (declare (ignore condition))
  (format t "UNKNOWN-TOKEN~%")
  (throw :cannot-parse nil))

(defun print-depth (lexer)
  (terpri)
  (dotimes (n (tree-depth lexer))
    (princ "-"))
  (princ "> "))

(defun assign-token (token-string lexer)
  (let ((tok (intern (string-upcase token-string))))
    (if (assoc tok *properties*)
        (setf (slot-value lexer 'active-token) tok)
        (signal 'unknown-token :unknown-token :unknown))))

(defun execute-token (lexer)
  (if (not (active-token lexer))
      (format t "Parse error: active token is ~A~%" (active-token lexer))
      (format t "Executing token ~A~%" (active-token lexer))))

(defun lex-sgf-string (lexer)
  (let ((current-position (pos lexer))
        (depth (tree-depth lexer)))
    (unless (equal (end lexer) current-position)
      (let* ((tok-sym (get-next-token lexer))
             (next-pos (cdr tok-sym)))
        (and tok-sym
             (casepr (car tok-sym)
                     (lparen (setf (slot-value lexer 'tree-depth) (1+ depth)))
                     (rparen (setf (slot-value lexer 'tree-depth) (1- depth)))
                     (semicolon (setf (slot-value lexer 'active-token) nil))
                     (property-name (assign-token (subseq (source lexer) (pos lexer) next-pos) lexer))
                     (parameters (execute-token lexer))))
        (setf (slot-value lexer 'pos) next-pos)
        (lex-sgf-string lexer)))))
