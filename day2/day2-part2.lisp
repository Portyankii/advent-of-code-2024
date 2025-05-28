(defparameter *file-path* "/home/rye/development/lisp/advent-of-code-2024/day2/test.dat")

(defun safe-report-p (report)
  (let ((increasing t)
        (decreasing t)
        (valid-diff t)
        (bad-diff-count 0))
    (dotimes (i (1- (length report)))
      (let ((diff (abs (- (elt report (1+ i)) (elt report i)))))
        ;; Check if difference is between 1 and 3
        (unless (and (>= diff 1) (<= diff 3))
          (incf bad-diff-count))
        ;; Stop checking if more than one bad difference is found
        (when (> bad-diff-count 1)
          (setq valid-diff nil))
        ;; Update increasing and decreasing flags
        (when (> (elt report (1+ i)) (elt report i))
          (setq decreasing nil))
        (when (< (elt report (1+ i)) (elt report i))
          (setq increasing nil))))
    ;; Ensure mixed trends with valid differences are allowed
    (and valid-diff (or increasing decreasing (<= bad-diff-count 1)))))



(defun count-safe-reports (reports)
  (let ((safe-count 0))
    (dolist (report reports)
      (when (safe-report-p report)
        (incf safe-count)))
    safe-count))

(defun read-reports-from-file (filename)
  (with-open-file (stream filename)
    (loop with line-count = 0
          for line = (read-line stream nil)
          while line
          do (incf line-count)
          collect (map 'list #'parse-integer (split-sequence:split-sequence #\Space line))
          finally (format t "Total lines read: ~a~%" line-count))))

(let ((reports (read-reports-from-file *file-path*)))
  (print (count-safe-reports reports)))
