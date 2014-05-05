;parser for createyourproglang.com

;this takes a list of tokens from the lexer and returns an AST made from s-expressions


(defstruct state
  start
  pos 
  tokens
  tree
  exp
  exp-stack
  len)

(defun run-parser (tokens)
  (let ((s (make-lexer :start 0 :pos 0 :tokens tokens :exp '() exp-stack '() :tree '() :len (length tokens))))
    (loop for state-fn = #'state then (funcall state-fn s)
	 while state-fn)
    (reverse (state-tree s))))

(defun next (s)
  ;returns the next token, advances pos by one
  (incf (state-pos s))
  (if (>= (state-pos s) (state-len s))
      'eof
      (let ((res (elt (state-tokens s) (state-pos s))))
	res)))

(defun peek (s n)
  ;returns the token n characters ahead of pos, does not advance pos
  (let ((idx (+ (state-pos s) n)))
    (if (>= idx (state-len s))
	'eof
	(let ((res (elt (state-tokens s) idx)))
	  res))))

(defun accept (s lst)
  (let ((token (next s)))
    (if (find (car token) lst)
	token
	nil)))

(defun accept-run (s lst)
  ;accepts one or more tokens of a type in lst
  (loop for token = (next s)
       while (find (car token) lst)
       collecting token))

(defun is-next-number (s)
  (is-number (peek s 0)))

(defun is-next-literal (s)
  (is-literal (peek s 0)))

(defun is-next-operator (s)
  (is-operator (peek s 0)))

(defun is-next-dot (s)
  (is-dot (peek s 0)))

(defun is-next-identifier (s)
  (is-identifier (peek s 0)))

(defun expression-start (s)
  ;this one assumes that it's at the very start of an expression
  ;figure out which sort of expression we're in
  (setf (state-exp s) '())
  (let ((token (next s)))
    (push token (state-exp s))
    (cond ((is-next-literal s)
	   (cond ((is-next-operator s) #'operator-state)
		 ((is-next-dot s) #'call-state)
		 (t #'expression-mid)))
	  ((is-next-identifier s)
	   (cond ((is-next-l-paren s) #'arglist-state)
		 ((is-next-equal s) #'set-local-state)
		 (t #'expression-mid)))
	  ((is-next-constant s)
	   (cond ((is-next-equal s) #'set-constant-state)
		 (t #'expression-mid)))
	  ((is-next-def s)
	   (if (is-next-identifier s)
	       #'def-state
	       (progn (print "def error") nil)))
	  ((is-next-class s)
	   (if (is-next-constant s)
	       #'class-state
	       (progn (print "class-error") nil)))
	  ((is-next-if s)
	   #'if-state)
	  ((is-next-l-paren s)
	   (progn
	     (incf (state-pos s))
	     #'expression-start)))))

(defun operator-state (s)
  ;state for operators.  Operator precedence will be handled at a later step
  ;that is, a set of tokens like this:
  ;2 + 3 * 5
  ;will produce a parse tree like this:
  ;(+ 2 (* 3 5))
  ;and be cleaned up later
  (push (next s) (state-exp s)
	   





(defun is-literal (token)
  (find (car token) (list 'number 'string 'true 'false 'nil)))


(defstruct lexer
  start
  pos
  code
  tokens
  len)

(defun next (l)
  ;returns the next input character
  (incf (lexer-pos l))
  (if (>= (lexer-pos l) (lexer-len l))
      'eof
      (let ((res (elt (lexer-code l) (lexer-pos l))))
	res)))

(defun emit (l token)
  (if (car token)
      (push token (lexer-tokens l))))

(defun is-number (char)
  ;returns t if char is the start of a number, nil otherwise
  (if (find char "1234567890.")
      t
      nil))

(defun next-state (char)
  ;generic next state function, because the transitions after numbers, symbols, strings
  ;and close parens are all the same
  (cond ((equal char #\() #'lex-open-paren)
	((equal char #\)) #'lex-close-paren)
	((is-number char) #'lex-number)
	((equal char #\") #'lex-string)
	((or (equal char #\space) (equal char #\newline)) #'lex-whitespace)
	(t #'lex-symbol)))

(defun lex-open-paren (l)
  ;lexer state for an open paren
  ;emit an open paren token, transition to the next state
  (emit l (list 'l-paren (elt (lexer-code l) (lexer-start l))))
  (incf (lexer-start l))
  (next-state (next l)))

(defun lex-close-paren (l)
  ;lexer state for a close paren
  ;emit a close paren token, transition to the next state
  (emit l (list 'r-paren (elt (lexer-code l) (lexer-start l))))
  (incf (lexer-start l))
  (next-state (next l)))

(defmacro lex-state (name until-cond emit-symb)
  `(defun ,name (l)
     (loop for char = (next l)
	until ,until-cond
	until (equal char 'eof)
	finally (if (equal char 'eof)
		    (return nil)
		    (progn
		      (emit l (list ,emit-symb (subseq (lexer-code l) (lexer-start l) (lexer-pos l))))
		      (setf (lexer-start l) (lexer-pos l))
		      (return (next-state char)))))))

(lex-state lex-whitespace (not (or (equal char #\space) (equal char #\newline))) nil)
(lex-state lex-string (or (equal char #\() (equal char #\)) (equal char #\space)) 'string)
(lex-state lex-number (not (is-number char)) 'number)
(lex-state lex-symbol (or (equal char #\space) (equal char #\newline)) 'symbol)

(eval-when (:execute)
  (print (run-lexer "(+ 1 23.4 \"abcd\" (* 3 4))")))
