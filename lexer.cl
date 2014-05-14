;Lexer for the language from http://createyourproglang.com/

;The book uses Ruby, but I don't know Ruby at all, and don't have it installed.  Also,
;the last time I tried doing something like this by simply copying from the book I didn't
;end up with much of an understanding of the language.

;so, I'm going to port it to Lisp instead, which will hopefully give a better understanding.

(in-package :CYOL)

(defun file-to-string (file-name)
  (with-open-file (in file-name)
    (with-output-to-string (s)
      (loop for line = (read-line in nil)
          while line
          do (princ line s)))))

(defun tokenize (code)
  (let ((keywords (list "def" "class" "if" "true" "false" "nil"))
	(tokens '())
	(current-indent 0)
	(indent-stack '(0))
	(chunk "temp"))
    (print keywords)
    (loop with pos = 0
       with len = (length code)
       while (< pos len)
       do ;(print chunk)
	 ;(print tokens)
	 (cond
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A([a-z]\\w*)" code :start pos))
	     (progn
	       (if (find chunk keywords :test #'equal)
		   (push (list 'keyword chunk) tokens)
		   (push (list 'identifier chunk) tokens))
	       (incf pos (length chunk))))
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A([A-Z]\\w*)" code :start pos))
	     (progn
	       (push (list 'constant chunk) tokens)
	       (incf pos (length chunk))))
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A([0-9]+)" code :start pos))
	     (progn
	       (push (list 'number chunk) tokens)
	       (incf pos (length chunk))))
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A\"([^\"]*)\"" code :start pos))
	     (progn 
	       (push (list 'string (remove #\" chunk)) tokens)
	       (incf pos (length chunk))))
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A\\:\\r( +)" code :start pos))
	     (progn
	       (if (<= (- (length chunk) 2) current-indent)
		   (print "error"))
	       (setf current-indent (- (length chunk) 2))
	       (push current-indent indent-stack)
	       (push (list 'indent current-indent) tokens)
	       (incf pos (+ 2 current-indent))))
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A\\r( *)" code :start pos))
	     (progn
	       (if (= (- (length chunk) 1) current-indent) 
		   (push (list 'newline "\\n") tokens) ;indentation level hasn't changed
		   (progn
		     #|(let ((prev-indent (pop indent-stack)))
		       (if (car indent-stack)
			   (setf current-indent (car indent-stack))
			   (setf current-indent 0))
		       (push (list 'dedent (- prev-indent (- (length chunk) 1))) tokens))|#
		     (loop for indent-size = (- (length chunk) 1)
			while (< indent-size current-indent)
			do (let ((prev-indent (pop indent-stack)))
			  (setf current-indent (car indent-stack))
			  ;(print "dedent test")
			  ;(print prev-indent)
			  ;(print current-indent)
			  ;(print (length chunk))
			  (push (list 'dedent (- prev-indent (- (length chunk) 1))) tokens)))
		     (push (list 'newline "\\n") tokens)))
	       (incf pos (length chunk))))
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A(\\|\\||&&|==|!=|<=|>=)" code :start pos))
	     (progn
	       (push (list 'operator chunk) tokens)
	       (incf pos (length chunk))))
	    ((equal (elt code pos) #\space) (incf pos))
	    (t (progn
		 (push (list 'value (elt code pos)) tokens)
		 (incf pos)))))
	 (reverse tokens)))
  
(eval-when (:execute)
  (print (tokenize (file-to-string "c:\\ben\\code\\CYOL\\test.txt"))))











    
    
