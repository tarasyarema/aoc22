(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defun input (file tfun)
  (let* ((lines (uiop:read-file-lines file)))
    (funcall tfun lines)))

(defun without-last(l)
    (reverse (cdr (reverse l))))

(defun parse-tree (lines)
  (let* ((fs '())
         (curr '())
         (curr_fs '()))
    (loop for line in lines
        when (str:containsp "$ cd" line)
        do (let* ((path (str:substring 5 t line)))
             (progn
               (if (and (not (null curr)) (not (null curr_fs))) 
                   (setq fs (append fs (list (list curr curr_fs)))))
               (if (equal path "..")
                  (progn
                    (setq curr (without-last curr))
                    (setq curr_fs '()))
                (progn
                  (setq curr (append curr (list path)))
                  (setq curr_fs '())))))
        when (> (cl-ppcre:count-matches "([0-9]+)" line) 0)
        do (setq curr_fs (append curr_fs (list (parse-integer (cl-ppcre:scan-to-strings "([0-9]+)" line)))))
        when (> (cl-ppcre:count-matches "dir ([a-z]+)" line) 0)
        do (setq curr_fs (append curr_fs (list (nth 1 (str:split " " line))))))
    ; Last set
    (setq fs (append fs (list (list curr curr_fs))))
    fs))

(defun compute-size (tree path)
  (loop for (root nodes) in tree
      when (equal root path)
      summing (loop for node in nodes
                summing (if (integerp node) node
                            (compute-size tree (append path (list node)))))))

(defun run-first (lines)
  (let* ((tr (parse-tree lines)))
    (loop for (path _nodes) in tr
      sum (let* ((curr (compute-size tr path)))
            (if (<= curr 100000) curr 0)))))

(defun run-second (lines)
  (let* ((all (let* ((tr (parse-tree lines)))
                (loop for (path _nodes) in tr 
                      collect (compute-size tr path))))
         (s (car all))
         (d (- 30000000 (- 70000000 s))))
    (loop for p in all
          when (> p d)
          minimize p)))

(print "Results")
(print (list "P1" (input "in" #'run-first)))
(print (list "P2" (input "in" #'run-second)))
