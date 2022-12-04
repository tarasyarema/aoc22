(ql:quickload :str)

(defun input (file tfun)
  (let* ((lines (uiop:read-file-lines file)))
    (funcall tfun 
        (loop for line in lines 
            collect (mapcar (lambda (part) (mapcar #'parse-integer (str:split "-" part :omit-nulls t)))
                      (str:split "," line :omit-nulls t))))))

(defun range (a b)
   (loop for n from a below (+ b 1) by 1
      collect n))

(defun run (lines)
  (reduce '+
    (loop for ((a b) (c d)) in lines
        collect (if (and (set-difference (range c d) (range a b)) (set-difference (range a b) (range c d))) 0 1))))

(defun run2 (lines)
  (reduce '+
    (loop for ((a b) (c d)) in lines
        collect (if (not (intersection (range a b) (range c d))) 0 1))))

(print "Results")
(print (list "P1" (input "in" #'run)))
(print (list "P2" (input "in" #'run2)))
