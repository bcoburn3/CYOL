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
      (print (prune tree)))))

(eval-when (:execute)
  (print (tokenize (file-to-string "c:\\ben\\code\\CYOL\\test2.txt"))))
