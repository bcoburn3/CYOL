(defpackage :CYOL)

(in-package :CYOL)

(eval-when (:compile-toplevel :execute)
  (ql:quickload :cl-ppcre))

(eval-when (:execute)
  (print (tokenize (file-to-string "c:\\ben\\code\\CYOL\\test.txt")))
  (print (run-parser (tokenize (file-to-string  "c:\\ben\\code\\CYOL\\test.txt")))))

#|
((((NUMBER "2") (KEYWORD "if") ((CONSTANT "True") (IDENTIFIER "tasty") (OPERATOR "==") ((STRING "Delicious!") (IDENTIFIER "print") BLOCK) (((STRING "done!") (IDENTIFIER "print") NIL-RECIEVER CALL (NUMBER "1") (KEYWORD "if") (BLOCK ((NUMBER "2") (KEYWORD "if") ((STRING "...") (IDENTIFIER "print") NIL-RECIEVER CALL BLOCK)) (((KEYWORD "false") (KEYWORD "if") ((IDENTIFIER "pass") BLOCK)))))))))) 

|#
