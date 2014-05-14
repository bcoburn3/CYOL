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

(defun run-parser (tokens)
  (let ((s (make-state :start 0 :pos -1 :tokens tokens :exp '() :exp-stack '() :len (length tokens))))
    (loop while (< (state-pos s) (- (state-len s) 1))
       for exp = (expression-start s)
       while exp
       repeat 1000
       collecting exp)))

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

(defun expression-start (s)
  (cond ((equal 'eof s) 'nil)
	((equal (peek s 1) 'eof) 'nil)
	((literal-state s))
	((call-state s))
	((operator-state s))
	((set-symbol-state s))
	((get-symbol-state s))
	((def-state s))
	((class-state s))
	((if-state s))
	(t (next s))))

(defun literal-state (s)
  (if (is-next-literal s)
      (next s)
      'nil))

(defun call-state (s)
  (if (and (is-next-identifier s) (is-l-paren (peek s 2)))
      (list 'nil-receiver-call
	    (next s) 
	    (loop for exp = (expression-start s)
	       until (is-r-paren exp)
	       collecting exp))
      (if (and (is-next-dot s) (is-identifier (peek s 2)))
	  (let ((res '()))
	    ;this type of call will be 'merged' with the previous expression in a 
	    ;future pass
	    (push 'prev-receiver-call res)
	    (skip s)
	    (push (next s) res)
	    (if (is-next-l-paren s)
		(progn (skip s)
		       (loop for exp = (expression-start s)
			  until (is-r-paren exp)
			  do (push exp res))))
	    (reverse res)))))

(defun operator-state (s)
  ;these will be merged with the previous expression as with method calls
  (if (is-next-operator s)
      (list (next s) (expression-start s))))

(defun get-symbol-state (s)
  (if (and (is-next-symbol s) (not (or (is-equal (peek s 2))
				       (is-l-paren (peek s 2)))))
      (next s)))

(defun set-symbol-state (s)
  (if (and (is-next-symbol s) (is-equal (peek s 2)))
      (let ((symb (next s)))
	(skip s)
	(list 'set-symb symb (expression-start s)))))

(defun def-state (s)
  (if (and (is-next-def s) (is-identifier (peek s 2)))
      (let ((res (next s)))
	(push (next s) res)
	(if (is-next-l-paren s)
	    (progn (skip s)
		   (loop for token = (next s)
		      until (is-r-paren token)
		      do (if (is-identifier token)
			     (push token res)))))
	(skip s) ;ignoring the parse error when there isn't a block here
	(push (block-start s) res)
	(reverse res))))	

(defun class-state (s)
  (if (and (is-next-class s) (is-class (peek s 2)))
      (let ((res (next s)))
	(push (next s) res)
	(push (block-start s) res)
	(reverse res))))

(defun block-start (s)
  (loop for exp = (expression-start s)
     until (is-dedent exp)
     collecting exp))

(defun if-state (s)
  (if (is-next-if s)
      (let ((res (next s)))
	(push (loop for exp = (expression-start s)
		 until (is-indent exp)
		 collecting exp)
	      res)
	(push (block-start s) res)
	(reverse res))))

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

(defun is-symbol (token)
  (or (is-constant token)
      (is-identifier token)))

(defun is-next-symbol (s)
  (is-symbol (peek s 1)))

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


