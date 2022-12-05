(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defun input (file tfun n reversed)
  (let* ((lines (uiop:read-file-lines file)))
    (funcall tfun lines n reversed)))

(defun popn (l n reversed)
  (let* ((ll (loop for i from 0 to n collect (nth i l))))
    (if reversed (reverse ll) ll)))

(popn '("D" "N" "Z") 1 T)

(defun parse-cranes (lines n)
  (mapcar (lambda (x) (remove-if #'null x))
    (apply #'mapcar #'list
      (loop for i from 0 to n by 1
        collect (let* ((parsed (ppcre:all-matches-as-strings "(([(A-Z)]|\\s{3})\\s{0,1})" (nth i lines))))
                  (loop for part in parsed 
                    collect (if (equal part "    ") nil part)))))))

(input "in" #'parse-cranes 7)

(defun parse-moves (lines n)
    (loop for i from (+ n 3) to (- (length lines) 1)
        collect (mapcar 'parse-integer 
                    (ppcre:all-matches-as-strings "([0-9]+)" (nth i lines)))))

(input "_in" #'parse-moves 2)

(defun set-nth (list n val)
  (if (> n 0)
      (cons (car list)
            (set-nth (cdr list) (- n 1) val))
      (cons (append val (car list)) (cdr list))))

(set-nth '(("N" "Z") ("D" "C" "M") ("P")) 0 '("D"))

(defun pop-nth (list n m)
  (if (> n 0)
      (cons (car list) (pop-nth (cdr list) (- n 1) m))
    (cons (nthcdr m (car list)) (cdr list))))

(pop-nth '(("N" "Z") ("D" "C" "M") ("P")) 1 1)

(defun move-n-to-m (list n from to reversed)
  (set-nth (pop-nth list from n) to (popn (nth from list) (- n 1) reversed)))

(move-n-to-m '(("D" "N" "Z") ("C" "M") ("P")) 3 0 2 T)

(defun run (lines n reversed)
  (str:join ""
    (loop for stack in 
      (let* ((cranes (parse-cranes lines n)))
        (loop for (n from to) in (parse-moves lines n)
            do (setq cranes (move-n-to-m cranes n (- from 1) (- to 1) reversed))
            finally (return (values cranes))))
      collect (car stack))))

; _in 2
; in 7

(print "Results")
(print (list "P1" (input "in" #'run 7 T)))
(print (list "P2" (input "in" #'run 7 NIL)))
