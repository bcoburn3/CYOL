;parser for createyourproglang.com

;this takes a list of tokens from the lexer and returns an AST made from s-expressions

(in-package :CYOL)

(defstruct state
  start
  pos 
  tokens
  exp
  exp-stack
  len
  ops)

(defun add-delims (tokens)
  ;returns a version of the token stream with delimitors for the AST added.
  ;instead of parsing the tokens directly, we're going to just turn it into a 
  ;lisp-like syntax using some (hopefully simple) rules
  (let  ((s (make-state :start 0 :pos -1 :tokens tokens :exp '() :exp-stack '() :len (length tokens) :ops 0)))
    (loop 
       for token = (next s)
       with res = (list 'open-delim)
       while (not (equal (peek s 1) 'eof))
       do (print "next")
	 (print token)
	 (print res)
	 (cond ((is-newline token)
		(if (not (is-next-newline s))
		    (push 'open-delim res)))
	       ((is-next-operator s)
		(setf res (append (operator-delims s) res)))
	       ((or (is-next-newline s) (is-next-indent s))
		(progn
		  (push token res)         ;if the current character is a newline we'd match
		  (push 'close-delim res))) ;the first condition
	       ((is-dedent token)
		(loop repeat (cadr token)
		   do (push 'close-delim res)))
	       (t (push token res)))
       finally (return res))))

(defun operator-delims (s)
  (loop for i = 0 then (+ i 1) ;starting at 2 to skip the first operator
     with ops-count = -1
     with res = '()
     for token = (peek s 0) then (next s)
     while (not (is-terminator token))
     do (if (is-next-operator s)
	    (progn (incf ops-count)
		   (push 'open-delim res)
		   (push token res))
	    (push token res))
     finally (progn ;(incf (state-pos s) i)
		    (return (append res (loop repeat ops-count collecting 'close-delim))))))


(defun run-parser (tokens)
  (let* ((n-tokens (append (list (list 'null 'token)) tokens))
	 (s (make-state :start 0 :pos 0 :tokens n-tokens :exp '() :exp-stack '() :tree '() :len (length tokens))))
    (loop for state-fn = #'expression-start then (funcall state-fn s)
	 repeat 1000
       while state-fn
       do (print "next")
	 (print (peek s 1))
	 (print "parser state")
	 (print (state-exp s))
	 (print (state-exp-stack s))
	 (print state-fn))
    (print (state-exp-stack s))
    (print (state-exp s))
    (reverse (state-exp-stack s))))

(defun next (s)
  ;returns the next token, advances pos by one
  (incf (state-pos s))
  (if (>= (state-pos s) (state-len s))
      'eof
      (let ((res (elt (state-tokens s) (state-pos s))))
	res)))

(defun skip (s)
  (incf (state-pos s)))

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

(defun append-token (s token)
  ;appends a token to the current expression
  (setf (state-exp s) (append (state-exp s) (list token))))

(defun push-cur-exp (s)
  (if (state-exp s) ;don't push nil states
      (push (list (state-exp s)) (state-exp-stack s)))
  (setf (state-exp s) '()))

(defun append-cur-exp (s)
  (if (state-exp-stack s)
      (if (state-exp s) ;don't push nil states
	  (setf (car (state-exp-stack s)) (append (list (state-exp s)) (car (state-exp-stack s)))))
       ;if the expression stack is empty we can just set it to the current expression
      (setf (state-exp-stack s) (list (state-exp s))))
  (setf (state-exp s) '()))

(defun pop-exp-stack (s)
  (setf (state-exp s) (append (list (state-exp s)) (pop (state-exp-stack s)))))

;notes:
;this works by building up each expression in reverse order, then calling reverse as they're
;pushed to the next step.
;all the (print "some error") calls will eventually be replaced by better reporting

(defun expression-start (s)
  ;this one assumes that it's at the very start of an expression
  ;push the current expression onto the indentation stack, then
  ;clear the current expression, and get the next token
  ;(push-cur-exp s)
  ;(let ((token (next s)))
    ;(setf (state-exp s) token)
    ;figure out which sort of expression we're in
  (cond ((equal (peek s 2) 'eof)
	 'nil)
	((is-next-literal s)
	 (progn (print "literal")
	 (cond ;((is-operator (peek s 2)) #'operator-state)
	       ((is-dot (peek s 2)) #'call-state)
	       (t (progn (push (next s) (state-exp s))
			 #'expression-start))
	       )))
	((is-next-identifier s)
	 (progn (print "identifier")
	 (cond ((is-l-paren (peek s 2)) #'self-call-state)
	       ((is-equal (peek s 2)) #'set-local-state)
	       (t (progn (push (next s) (state-exp s))
			 #'expression-start)))))
	((is-next-constant s)
	 (progn (print "constant")
	 (cond ((is-equal (peek s 2)) #'set-constant-state)
	       (t (progn (push (next s) (state-exp s))
			 #'expression-start)))))
	((is-next-operator s)
	 #'operator-state)
	((is-next-def s)
	 (progn (print "def")
	 (if (is-identifier (peek s 2))
	     #'def-state
	     (progn (print "def error") nil))))
	((is-next-class s)
	 (progn (print "class")
	 (if (is-constant (peek s 1))
	     #'class-state
	     (progn (print "class-error") nil))))
	((is-next-if s)
	 #'if-state)
	((is-next-delimiter s)
	 #'delim-state)
	((is-next-l-paren s)
	 (progn
	   (push-cur-exp s)
	   (skip s)
	   #'expression-start))
	(t (progn (print "missing state")
		  (print (peek s 1))))))

(defun operator-state (s)
  ;state for operators.  Operator precedence will be handled at a later step
  ;that is, a set of tokens like this:
  ;2 + 3 * 5
  ;will produce a parse tree like this:
  ;(+ 2 (* 3 5))
  ;and be cleaned up later
  (let ((prev-exp (pop (state-exp s))))
    ;(push-cur-exp s)
    (push (next s) (state-exp s))
    (push prev-exp (state-exp s))
    #'expression-start))

(defun call-state (s)
  ;state for expression.method type calls.
  (push-cur-exp s)
  (push 'call (state-exp s))
  (push (next s) (state-exp s))
  (skip s)
  (push (next s) (state-exp s))
  #'arglist-start-state)

(defun self-call-state (s)
  ;state for method(args) type calls.  add a 'nil-reciever token to the start of the 
  ;current expression, then continue to arglist
  (push-cur-exp s)
  (push 'call (state-exp s))
  (push 'nil-reciever (state-exp s))
  (push (next s) (state-exp s))
  #'arglist-start-state)
  
(defun arglist-start-state (s)
  ;state for argument lists
  ;skip the open paren, then check for closing parens
  (if (is-next-l-paren s)
      (progn (skip s)
	     (if (is-next-r-paren s)
		 (progn
		   (push 'nil-arguments (state-exp s))
		   (pop-exp-stack s)
		   #'expression-start)
		 #'expression-start))
      (print "arglist error, expected open paren")))

(defun set-local-state (s)
  ;insert a set token, then push just jump to a new expression
  (push 'set-local (state-exp s))
  (push (next s) (state-exp s))
  #'expression-start)

(defun set-constant-state (s)
  ;as above
  (push 'set-constant (state-exp s))
  (push (next s) (state-exp s))
  #'expression-start)

(defun def-state (s)
  (push-cur-exp s)
  (push (next s) (state-exp s))
  (push (next s) (state-exp s))
  (if (is-next-l-paren s)
      (progn
	(skip s)
	(push-cur-exp s)
	(push 'param-list (state-exp s))
	#'param-list-state)
     #'block-start))

(defun class-state (s)
  (push-cur-exp s)
  (push (next s) (state-exp s))
  (push (next s) (state-exp s))
  #'block-start)

(defun param-list-state (s)
  ;in the beginning or somewhere in the middle of a param list
  ;check for the closing paren, then push the identifier
  (if (is-next-comma s)
      (skip s))
1  (if (is-next-r-paren s)
      (progn 
	(pop-exp-stack s)
	#'block-start)
      (progn (push (next s) (state-exp s))
	     #'param-list-state)))

(defun block-start (s)
  (if (is-next-indent s)
      (progn (skip s)
	     (push-cur-exp s)
	     ;(push 'block (state-exp s))
	     #'expression-start)
      (print "block error, expected indent")))

(defun if-state (s)
  ;this just drops the potential error from not having a block after the
  ;if on the floor
  (append-cur-exp s)
  (push (next s) (state-exp s))
  #'expression-start)

(defun delim-state (s)
  ;state for having found a delimiter.  This includes commas, start/end of blocks,
  ;newlines, etc.
  (cond ((is-next-indent s) #'block-start)
	((is-next-comma s)
	 (progn (skip s)
		#'expression-start))
	((is-next-r-paren s)
	 (progn ;(pop-exp-stack s)
		(skip s)
		#'expression-start))
	((is-next-dedent s)
	 (progn (let ((offset (ceiling (cadr (next s)) 1)))
		  (loop repeat offset
		     do (pop-exp-stack s)))
		(skip s)
		#'expression-start))
	((is-next-newline s)
	 (progn (append-cur-exp s)
		(skip s)
		#'expression-start))))

;is-next-x type functions only below
(defun is-next-number (s)
  (is-number (peek s 1)))

(defun is-number (token)
  (equal (car token) 'number))

(defun is-next-literal (s)
  (is-literal (peek s 1)))

(defun is-literal (token)
  (or (equal (car token) 'string)
      (equal (car token) 'number)
      (and (equal (car token) 'keyword)
	   (or (equal (cadr token) "true")
	       (equal (cadr token) "false")
	       (equal (cadr token) "nil")))))

(defun is-next-operator (s)
  (is-operator (peek s 1)))

(defun is-operator (token)
  (equal (car token) 'operator))

(defun is-next-dot (s)
  (is-dot (peek s 1)))

(defun is-dot (token)
  (and (equal (car token) 'value)
       (equal (cadr token) ".")))

(defun is-next-identifier (s)
  (is-identifier (peek s 1)))

(defun is-identifier (token)
  (equal (car token) 'identifier))

(defun is-next-l-paren (s)
  (is-l-paren (peek s 1)))

(defun is-l-paren (token)
  (and (equal (car token) 'value)
       (equal (cadr token) #\()))

(defun is-next-r-paren (s)
  (is-r-paren (peek s 1)))

(defun is-r-paren (token)
  (and (equal (car token) 'value)
       (equal (cadr token) #\))))

(defun is-next-comma (s)
  (is-comma (peek s 1)))

(defun is-comma (token)
  (and (equal (car token) 'value)
       (equal (cadr token) #\,)))

(defun is-next-equal (s)
  (is-equal (peek s 1)))

(defun is-equal (token)
  (and (equal (car token) 'value)
       (equal (cadr token) #\=)))

(defun is-next-constant (s)
  (is-constant (peek s 1)))

(defun is-constant (token)
  (equal (car token) 'constant))

(defun is-next-def (s)
  (is-def (peek s 1)))

(defun is-def (token)
  (and (equal (car token) 'keyword)
       (equal (cadr token) "def")))

(defun is-next-class (s)
  (is-class (peek s 1)))

(defun is-class (token)
  (and (equal (car token) 'keyword)
       (equal (cadr token) "class")))

(defun is-next-if (s)
    (is-if (peek s 1)))
 
(defun is-if (token)
  (and (equal (car token) 'keyword)
       (equal (cadr token) "if")))

(defun is-next-indent (s)
  (is-indent (peek s 1)))

(defun is-indent (token)
  (equal (car token) 'indent))

(defun is-next-dedent (s)
  (is-dedent (peek s 1)))

(defun is-dedent (token)
  (equal (car token) 'dedent))

(defun is-next-newline (s)
  (is-newline (peek s 1)))

(defun is-newline (token)
  (or (equal (car token) 'newline)
      (and (equal (car token) 'value)
	   (equal (cadr token) ";"))))

(defun is-next-delimiter (s)
  (is-delimiter (peek s 1)))

(defun is-delimiter (token)
  (or (is-comma token)
      (is-r-paren token)
      (is-dedent token)
      (is-indent token)
      (is-newline token)))

(defun is-next-terminator (s)
  (is-terminator (peek s 1)))

(defun is-terminator (token)
  (or (is-newline token)
      (is-indent token)
      (is-dedent token)))


