(ql:quickload "str")

(defun input (file tfun l)
  (let* ((lines (uiop:read-file-lines file)))
    (funcall tfun (map 'list #'char-code (car lines)) l)))

(defun sublist (l a b)
  (let* ((s (nthcdr a l)))
    (loop for i from 0 to (- (min (length s) (- b a)) 1)
          collect (nth i s))))

(defun is-marker (l n)
  (equal n (length (remove-duplicates l))))

; This one returns the marker, I though you had
; to return this instead of the number XD
(defun run-wtf (line l)
  (str:concat
    (mapcar 'code-char
      (loop for i from 0 to (length line)
        when (<= (+ i l) (length line))
        do (let* ((curr (sublist line i (+ i l))))
             (if (is-marker curr l) (return curr)))))))

(defun run (line l)
  (loop for i from 0 to (length line)
    when (<= (+ i l) (length line))
    do (let* ((curr (sublist line i (+ i l))))
         (if (is-marker curr l) (return (+ i l))))))

(print "Results")
(print (list "P1" (input "in" #'run 4)))
(print (list "P2" (input "in" #'run 14)))
