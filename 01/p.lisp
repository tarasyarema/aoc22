(defun run (x l)
  (if (null l) x
      (if (first l)
          (run (+ x (first l)) (rest l))
          (max x (run 0 (rest l))))))

(defun run2 (x l)
  (if (null l) (cons x nil)
      (if (first l)
          (run2 (+ x (first l)) (rest l))
          (cons x (run2 0 (rest l))))))

(print (subseq (sort (run2 0 (loop for line in (uiop:read-file-lines "in")
    collect (parse-integer line :junk-allowed t))) '>) 0 3))

(reduce '+ (subseq (sort (run2 0 (loop for line in (uiop:read-file-lines "in")
    collect (parse-integer line :junk-allowed t))) '>) 0 3))
