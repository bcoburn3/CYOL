(defpackage :CYOL)

(in-package :CYOL)

(eval-when (:compile-toplevel :execute)
  (ql:quickload :cl-ppcre))

(eval-when (:execute)
  (let ((tokens (tokenize (file-to-string "c:\\ben\\code\\CYOL\\test2.txt"))))
    (print tokens)
    (print (length tokens))
    (let ((tree (run-parser tokens)))
      (print tree)
      (print (prune tree))
      (print (comp (prune tree))))))

(eval-when (:execute)
  (print (tokenize (file-to-string "c:\\ben\\code\\CYOL\\test2.txt"))))

;will produce something like actual lisp code, as a string.  Key insight:
;a compiler is just like an interpreter that produces machine code (or whatever) as output
;instead of actually doing the computations.  My comp below is based on the first evaluate
;function in Lisp In Small Pieces.

(defun comp (exp)
  (if (token? exp)
      (let ((type (car exp))
	    (val (nth 1 exp))) ;because nth returns nil for a one element list instead of erroring
	(cond ((equal type 'identifier) val)
	      ((equal type 'string) (concatenate 'string "\"" val "\""))
	      ((equal type 'number) val)
	      ((equal type 'operator) val)
	      ((equal type 'constant) val)
	      ((equal type 'keyword) val)
	      ((equal type 'set-symb) "setf ")
	      ((equal type 'nil-receiver-call) "function-call ")
	      ((equal type 'prev-receiver-call) "method-call ")))
      (let ((type (car exp)))
	(cond ((and (equal (car type) 'keyword) (equal (cadr type) "if "))
	       (concatenate 'string
			    "(if "
			    (comp (cadr exp)) " "
			    "(progn " (comp (cddr exp)) ") "))
	      ((equal (car type) 'set-symb)
	       (concatenate 'string
			    "(setf " 
			    (comp (cadr exp))
			    " "
			    (comp (caddr exp))))
	      (t (concatenate 'string 
			      "("
			      (comp (car exp))
			      (comp-list (cdr exp))))))))

(defun comp-list (lst)
  (if (listp lst)
      (if (cdr lst)
	  (concatenate 'string " "
		       (comp (car lst))
		       (comp-list (cdr lst)))
	  (concatenate 'string " "
		       (comp (car lst)) ") "))))

(defun token? (tok)
  (atom (car-or-nil tok)))
