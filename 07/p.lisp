(ql:quickload "str")

(defun input (file tfun)
  (let* ((lines (uiop:read-file-lines file)))
    (funcall tfun lines)))

(defun run (lines)
  (print lines))

(print "Results")
(print (list "P1" (input "_in" #'run)))
(print (list "P2" (input "in" #'run)))
