(defparameter *file-path* "advent-of-code-2024/day2/day2.dat")

(defun strictly-safe-report-p (report)
  "Check if a report is safe without any tolerance (Part 1 logic)"
  (when (< (length report) 2)
    (return-from strictly-safe-report-p t))

  (let ((increasing t)
        (decreasing t))
    (dotimes (i (1- (length report)))
      (let* ((curr (elt report i))
             (next (elt report (1+ i)))
             (diff (abs (- next curr))))
        ;; Check if difference is between 1 and 3
        (unless (and (>= diff 1) (<= diff 3))
          (return-from strictly-safe-report-p nil))
        ;; Update trend flags
        (when (> next curr)
          (setq decreasing nil))
        (when (< next curr)
          (setq increasing nil))
        ;; If neither increasing nor decreasing, it's not safe
        (unless (or increasing decreasing)
          (return-from strictly-safe-report-p nil))))
    t))

(defun safe-report-with-dampener-p (report)
  "Check if a report is safe with Problem Dampener (Part 2 logic)"
  ;; First check if it's already safe without removing anything
  (when (strictly-safe-report-p report)
    (return-from safe-report-with-dampener-p t))

  ;; Try removing each element one at a time
  (dotimes (i (length report))
    (let ((modified-report (append (subseq report 0 i)
                                   (subseq report (1+ i)))))
      (when (strictly-safe-report-p modified-report)
        (return-from safe-report-with-dampener-p t))))

  ;; If no single removal makes it safe, return nil
  nil)

(defun count-safe-reports (reports &optional (use-dampener nil))
  "Count safe reports. If use-dampener is true, uses Part 2 logic."
  (let ((safe-count 0))
    (dolist (report reports)
      (when (if use-dampener
                (safe-report-with-dampener-p report)
                (strictly-safe-report-p report))
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

;; Test with the example data
(defun test-with-example ()
  "Test with the provided example data"
  (let ((example-reports '((7 6 4 2 1)
                           (1 2 7 8 9)
                           (9 7 6 2 1)
                           (1 3 2 4 5)
                           (8 6 4 4 1)
                           (1 3 6 7 9))))
    (format t "Example data:~%")
    (format t "Part 1 (no dampener): ~a safe reports~%"
            (count-safe-reports example-reports nil))
    (format t "Part 2 (with dampener): ~a safe reports~%"
            (count-safe-reports example-reports t))
    (format t "~%Individual report analysis:~%")
    (dolist (report example-reports)
      (format t "~a: Part1=~a, Part2=~a~%"
              report
              (strictly-safe-report-p report)
              (safe-report-with-dampener-p report)))))

;; Run the actual solution
(let ((reports (read-reports-from-file *file-path*)))
  (format t "~%Results from file:~%")
  (format t "Part 1 (no dampener): ~a safe reports~%"
          (count-safe-reports reports nil))
  (format t "Part 2 (with dampener): ~a safe reports~%"
          (count-safe-reports reports t)))

;; Uncomment the line below to test with example data
;; (test-with-example)
