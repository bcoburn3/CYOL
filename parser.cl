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

(defun append-token (s token)
  ;appends a token to the current expression
  (setf (state-exp s) (append (state-exp s) (list token))))

(defun push-cur-exp (s)
  (push (state-exp s) (state-exp-stack s))
  (setf (state-exp s) '()))

(defun pop-exp-stack (s)
  (setf (state-exp s) (append (pop (state-exp-stack s)) (list (state-exp s)))))

;notes:
;this works by building up each expression in reverse order, then calling reverse as they're
;pushed to the next step.
;all the (print "some error") calls will eventually be replaced by better reporting

(defun expression-start (s)
  ;this one assumes that it's at the very start of an expression
  ;push the current expression onto the indentation stack, then
  ;clear the current expression, and get the next token
  ;(push (state-exp s) (state-exp-stack s))
  (let ((token (next s)))
    (setf (state-exp s) token)
    ;figure out which sort of expression we're in
    (cond ((is-next-literal s)
	   (cond ((is-next-operator s) #'operator-state)
		 ((is-next-dot s) #'call-state)
		 (t #'expression-mid)))
	  ((is-next-identifier s)
	   (cond ((is-next-l-paren s) #'self-call-state)
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
	  ((is-next-delimiter s)
	   #'delim-state)
	  ((is-next-l-paren s)
	   (progn
	     (skip s)
	     #'expression-start)))))

(defun operator-state (s)
  ;state for operators.  Operator precedence will be handled at a later step
  ;that is, a set of tokens like this:
  ;2 + 3 * 5
  ;will produce a parse tree like this:
  ;(+ 2 (* 3 5))
  ;and be cleaned up later
  (push (next s) (state-exp s))
  #'expression-start)

(defun call-state (s)
  ;state for expression.method type calls.  skip the dot, push the identifier after
  ;onto the current expression.
  (skip s)
  (if (is-next-identifier s)
      (push (next s) (state-exp s))
      (print "call error, expected identifier"))
  #'arglist-start-state)

(defun self-call-state (s)
  ;state for method(args) type calls.  add a 'nil-reciever token to the start of the 
  ;current expression, then continue to arglist
  (append-token s 'nil-reciever)
  #'arglist-start-state)
  
(defun arglist-start-state (s)
  ;state for argument lists
  ;start by inserting a 'call' token at the start of the current expression
  (append-token s 'call)
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
  (append-token s 'set-local)
  #'expression-start)

(defun set-constant-state (s)
  ;as above
  (append-token s 'set-constant)
  #'expression-start)

(defun def-state (s)
  ;push the identifier onto the current expression, then check for paramlists
  (push (next s) (state-exp s))
  (if (is-next-l-paren s)
      (progn
	(skip s)
	(push-cur-exp s)
	(push 'param-list (state-exp s))
	#'param-list-state)
     #'block-start))

(defun class-state (s)
  (push (next s) (state-exp s))
  #'block-start)

(defun param-list-state (s)
  ;in the beginning or somewhere in the middle of a param list
  ;check for the closing paren, then push the identifier
  (if (is-next-comma s)
      (skip s))
  (if (is-next-r-paren s)
      (progn 
	(pop-exp-stack s)
	#'block-start)
      (progn (push (next s) (state-exp s))
	     #'param-list-state)))

(defun block-start (s)
  (if (is-next-indent s)
      (progn (skip s)
	     (push-cur-exp s)
	     (push 'block (state-exp s))
	     #'expression-start)
      (print "block error, expected indent")))

(defun if-state (s)
  ;this just drops the potential error from not having a block after the
  ;if on the floor
  (append-token s 'if)
  #'expression-start)

(defun delim-state (s)
  ;state for having found a delimiter.  This includes commas, start/end of blocks,
  ;newlines, etc.
  (cond ((is-next-indent s) #'block-start)
	((is-next-comma s)
	 (progn (skip s)
		(expression-start s)))
	((is-next-r-paren s)
	 (progn (pop-exp-stack s)
		#'expression-start))
	((is-next-dedent s)
	 (progn (pop-exp-stack s)
		#'expression-start))

	     
