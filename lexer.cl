;Lexer for the language from http://createyourproglang.com/

;The book uses Ruby, but I don't know Ruby at all, and don't have it installed.  Also,
;the last time I tried doing something like this by simply copying from the book I didn't
;end up with much of an understanding of the language.

;so, I'm going to port it to Lisp instead, which will hopefully give a better understanding.

(eval-when (:compile-toplevel :execute)
  (ql:quickload :cl-ppcre))

(defun file-to-string (file-name)
  (with-open-file (in file-name)
    (with-output-to-string (s)
      (loop for line = (read-line in nil)
          while line
          do (princ line s)))))

(cl-ppcre:scan-to-strings "\\A\\:\\r( +)" ":
    ")

(print (cl-ppcre:scan-to-strings '(:sequence :modeless-start-anchor #\: #\return (:register (:greedy-repetition 1 nil #\space))) 
				 (with-output-to-string (stream)
				   (dolist (char '(#\: #\return #\space))
				     (princ char stream)))))

(print (cl-ppcre:parse-string  "\\A\\:\\r( +)"))

(loop for c across (file-to-string "c:\\ben\\code\\CYOL\\test.txt")
     do (print c))

(loop for c across ":
    "
     do (print c))

(defun tokenize (code)
  (let ((keywords '("def" "class" "if" "true" "false" "nil"))
	(tokens '())
	(current-indent 0)
	(indent-stack '(0))
	(chunk "temp"))
    (loop with pos = 0
       with len = (length code)
       while (< pos len)
       do (cond
	    ((setf chunk (cl-ppcre:scan-to-strings "\\A([a-z]\\w*)" code :start pos))
	     (progn
	       (if (find chunk keywords)
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
		     (loop for indent-size = (- (length chunk) 1)
			while (< indent-size current-indent)
			do (pop indent-stack)
			  (setf current-indent (car indent-stack))
			  (push (list 'dedent (- (length chunk) 1)) tokens))
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











    
    
