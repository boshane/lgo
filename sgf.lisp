;; SGF file lexer
;;
;;

(in-package :lgo)

(defconstant +sgf-tokens+
  '(("[\]]" rbrack)
    ("[\[]" lbrack)
    ("[\(]" lparen)
    ("[\)]" rparen)
    ("[\;]" semicolon)
    ("[\:]" colon)))

(defclass sgf-lexer ()
  ((source :initarg :source :reader source)
   (tree-depth :initform 0 :reader tree-depth)
   (pos    :initarg :pos :reader pos)
   (len    :initarg :len :reader len)
   (end    :initarg :end :reader end)
   (regex-table :initarg :regex-table :reader regex-table)))

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

(defmethod get-next-token ((lexer sgf-lexer))
  (loop for (regex . sym) in (regex-table lexer)
        for next = (cl-ppcre:scan regex (source lexer) :start (pos lexer))
        when next do (progn (setf (slot-value lexer 'pos) (1+ (pos lexer)))
                            (return (cons sym next)))))


(defun print-depth (depth)
  (dotimes (n depth)
    (format t "-"))
  (format t ">"))

(defmacro casepr (keyform &rest cases)
  "Wrapper for 'case' macro: test KEYFORM against CASES but print the car of a matching case."
    (let ((test (gensym)))
      `(let ((test ,keyform))
         (case test
         ,@(loop for (test prog) in cases
                 collect (cons test (list (progn '(print test))
                                          prog)))))))

(defmethod parse-sgf-string ((lexer sgf-lexer))
  (let ((current-position (pos lexer))
        (depth (tree-depth lexer)))
    (unless (equal (end lexer) current-position)
      (print-depth (tree-depth lexer))
      (let* ((tok-sym (get-next-token lexer))
             (tok-pos (cdr tok-sym))
             (sym (car tok-sym)))
        (casepr (car tok-sym)
          (lparen (setf (slot-value lexer 'tree-depth) (1+ depth)))
          (rparen (setf (slot-value lexer 'tree-depth) (1- depth)))
          (semicolon (format t "~A~%" sym))
          (colon (format t "~A~%" sym))
          (t (format t "Unknown!~%")))
        (parse-sgf-string lexer)))))
