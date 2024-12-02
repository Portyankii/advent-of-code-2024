(defun total-distance-between-lists (list1 list2)
  "Calculate the total distance between two lists of numbers after sorting them."
  (let* ((sorted1 (sort (copy-seq list1) #'<))
         (sorted2 (sort (copy-seq list2) #'<)))
    (reduce #'+
            (mapcar (lambda (x y) (abs (- x y))) sorted1 sorted2))))

(defun read-lists-from-file (file-path)
  "Read two lists from a file where each line contains two numbers separated by whitespace."
  (let ((list1 nil)
        (list2 nil))
    (with-open-file (stream file-path)
      (loop for line = (read-line stream nil)
            while line do
            (let* ((columns (split-sequence:split-sequence #\space line :remove-empty-subseqs t))
                   (left (parse-integer (first columns)))
                   (right (parse-integer (second columns))))
              (push left list1)
              (push right list2))))
    (values (reverse list1) (reverse list2))))

(defun main (file-path)
  (multiple-value-bind (list1 list2) (read-lists-from-file file-path)
    (format t "The total distance between the lists is: ~a~%"
            (total-distance-between-lists list1 list2))))

(main "input.dat")
