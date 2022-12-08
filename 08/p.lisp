(ql:quickload "str")

(defun input (file tfun)
  (let* ((lines (uiop:read-file-lines file)))
    (funcall tfun lines)))

(defun peaks-n (line)
  (let* ((m (- (car line) 1))
         (p 0))
    (loop for l in line
        do (if (> l m) 
               (progn
                 (setq m l)
                 (setq p (+ p 1)))))
    p))

(defun peaks (line)
  (let* ((m (- (car line) 1))
         (lm 0)
         (p 0))
    (loop for l in line
          for i from 0
        when (> l m)
        collect (progn
                 (setq lm m)
                 (setq m l)
                 (setq p (+ p 1))
                 (list i (- l lm))))))

(peaks (reverse '(6 5 3 3 2)))

(defun parse-lines (lines)
  (loop for line in lines
        collect (loop for c across line 
                 collect (parse-integer (string c)))))

(defun rotate (list-of-lists)
  (apply #'mapcar #'list list-of-lists))

(rotate '((1 2) (3 4)))

(defun without-last(l)
    (reverse (cdr (reverse l))))

(without-last '(1 2 3))

(defun flatten (l)
  (apply 'concatenate 'list l))

(defun vertical (parsed)
  (flatten
    (loop for l in (rotate parsed)
          for i from 0
          collect (loop for (x _) in (peaks l) collect (list x i))
          collect (loop for (x _) in (peaks (reverse l))
                        collect (list (- (length l) (+ x 1)) i)))))

(remove-duplicates
  (vertical (parse-lines (uiop:read-file-lines "_in")))
  :test #'equal)

(defun horizontal (parsed)
  (flatten
    (loop for l in parsed
          for i from 0
          collect (loop for (x _) in (peaks l) collect (list i x))
          collect (loop for (x _) in (peaks (reverse l))
                        collect (list i (- (length l) (+ x 1)))))))

(remove-duplicates
  (horizontal (parse-lines (uiop:read-file-lines "_in")))
  :test #'equal)

(defun parsed-peaks (parsed)
  (remove-duplicates 
    (append 
      (horizontal parsed) 
      (vertical parsed))
    :test #'equal))

(defun run-one (lines)
  (let* ((parsed (parse-lines lines)))
    (length (parsed-peaks parsed))))

(defun run-two (lines)
  (let* ((parsed (parse-lines lines)))
    (loop for (x y) in (parsed-peaks parsed)
      collect (list
                (list x y
               ; (reduce #'*
               ;   (remove-if-not #'identity
                   (list
                     (nth 1 (find-if (lambda (e) (equal (car e) y)) (peaks (nth x parsed))))
                     (nth 1 (find-if (lambda (e) (equal (car e) y)) (peaks (reverse (nth x parsed)))))
                     (nth 1 (find-if (lambda (e) (equal (car e) y)) (peaks (nth x (rotate parsed)))))
                     (nth 1 (find-if (lambda (e) (equal (car e) y)) (peaks (reverse (nth x (rotate parsed))))))))))))

(print "Results")
; (print (list "P1" (input "in" #'run-one)))
(print (list "P2" (input "_in" #'run-two)))
