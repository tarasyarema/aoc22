(defun input (file tfun)
  (let* ((i (uiop:read-file-lines file)))
    (funcall tfun i)))

(defun get-parts (line)
  (list 
    (subseq line 0 (/ (length line) 2))
    (subseq line (/ (length line) 2))))

(get-parts "sdfs")

(defun transform-char-code (code)
  (if (< code 97) (+ 27 (mod code 65)) (mod code 96)))

(transform-char-code 112)
(transform-char-code 76)

(defun n-intersection (lists)
  (if (null (rest lists))
      (first lists)
      (intersection (first lists) (n-intersection (rest lists)))))

(n-intersection (list (list 1 2 3) (list 1) (list 4 5 6 1) (list 1 2) (list 1)))

(defun common-letters (parts)
  (remove-duplicates 
    (n-intersection
           (loop for part in parts collect (loop for c across part collect (transform-char-code (char-code c)))))))

(common-letters (list "tarasy" "aremag"))

; (defparameter *my-hash* (make-hash-table :test 'equal))
; (setf (gethash 'one-entry *my-hash*) "one")
; (if (gethash 'one-entry *my-hash*) "Key exists" "Key does not exist")
; (if (gethash 'wtf *my-hash*) "Key exists" "Key does not exist")
; (defun print-hash-entry (key value)
;   (format t "The value associated with the key ~S is ~S~%" key value))
; (maphash #'print-hash-entry *my-hash*)

; (defun run-d (lines)
;     (loop for line in lines
;       collect (common-letters (get-parts line))))
; (input "_in" #'run-d)

(defun run (lines)
  (reduce '+ 
    (loop for line in lines
      collect (reduce '+ (common-letters (get-parts line))))))

(common-letters (list "vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg"))

(defun partition (input-list n)
  (loop with list = input-list
      while list collect (loop repeat n while list collect (pop list))))

(partition '(1 2 3 4 5 6 7 8 9 10) 6) 

(defun run2 (lines)
  (reduce '+ 
    (loop for parts in (partition lines 3)
      collect (reduce '+ (common-letters parts)))))

(print "Results")
(print (list "P1" (input "in" #'run)))
(print (list "P2" (input "in" #'run2)))
