(defparameter *file-path* "/config/workspace/advent-of-code-2024/day1/part2.dat")

(defun safe-report-p (report)
  (let ((increasing t)
        (decreasing t)
        (valid-diff t))
    (dotimes (i (1- (length report)))
      (let ((diff (abs (- (elt report (1+ i)) (elt report i)))))
        ;; Check if difference is between 1 and 3
        (unless (and (>= diff 1) (<= diff 3))
          (setq valid-diff nil))
        ;; Check if the sequence is increasing or decreasing
        (when (>= (elt report (1+ i)) (elt report i))
          (setq decreasing nil))
        (when (<= (elt report (1+ i)) (elt report i))
          (setq increasing nil))))
    (and valid-diff (or increasing decreasing))))

(defun count-safe-reports (reports)
  (let ((safe-count 0))
    (dolist (report reports)
      (when (safe-report-p report)
        (incf safe-count)))
    safe-count))

(defun read-reports-from-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (map 'list #'parse-integer (split-sequence:split-sequence #\Space line)))))

(let ((reports (read-reports-from-file *file-path*)))
  (print (count-safe-reports reports)))
