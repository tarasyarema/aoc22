(defun input (file tfun)
  (let* ((i (uiop:read-file-lines file)))
    (funcall tfun i)))

(defun parse-str (x)
  (mod (mod (mod (char-code (char x 0)) 88) 65) 23))

(loop for i in '("A" "B" "C" "X" "Y" "Z")
      do (print (list i (parse-str i))))


(defun win-better (x y)
  (if (equal x y) 3
      (if (equal (mod (+ x 1) 3) (mod y 3)) 6 0)))

(loop for i in '((1 1) (1 2) (1 0) (2 1) (2 2) (2 0) (0 1) (0 2) (0 0))
    do (print (list i (apply 'win-better i))))

(defun to-win (x y)
  (if (equal y 1) x
      (if (equal y 0) (mod (- x 1) 3)
          (if (equal y 2) (mod (+ x 1) 3)))))

(loop for i in '((1 1) (1 2) (1 0))
    do (print (list i (apply 'to-win i))))

(defun run (lines)
  (let* ((s 0))
    (loop for line in lines
        do (let* ((c (mapcar 'parse-str (str:words line))))
              (incf s (+ (+ (nth 1 c) 1) (apply 'win-better c)))))
    s))

(defun run2 (lines)
  (let* ((s 0))
    (loop for line in lines
        do (let* ((c (mapcar 'parse-str (str:words line))))
              (let* ((tmp (list (* (nth 1 c) 3) (apply 'to-win c) 1)))
                (incf s (apply '+ tmp)))))
    s))

(print "Results")
(print (list "P1" (input "in" #'run)))
(print (list "P2" (input "in" #'run2)))
