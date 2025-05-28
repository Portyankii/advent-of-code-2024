(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)

(defparameter *do* t)

(defun MUL (x y)
  "if do is true, return x*y, if do is nil, return 0"
  (if *do*
      (* x y)
      0))

(defun SET-DO ()
  (setf *do* t) 0)

(defun SET-DONT ()
  (setf *do* nil) 0)


(defun filter-slop (str)
  (cl-ppcre:all-matches-as-strings "mul\\([0-9]+,[0-9]+\\)|don't\\(\\)|do\\(\\)" str))

(defun fix-do (lst)
  (mapcar (lambda (s)
            (if (string= s "do()")
                "set-do()"
                s))
          lst))

(defun fix-dont (lst)
  (mapcar (lambda (s)
            (if (string= s "don't()")
                "set-dont()"
                s))
          lst))

(defun parse-c-call (s)
  "Parse a C-style call string like \"foo(1,2)\" to a Lisp list like (foo 1 2)."
  (let* ((open-paren (position #\( s))
         (close-paren (position #\) s :from-end t))
         (func-name (subseq s 0 open-paren))
         (args-str (subseq s (1+ open-paren) close-paren))
         (args (if (string= args-str "")
                   '()
                   (mapcar #'read-from-string
                           (split-sequence:split-sequence #\, args-str)))))
    (list* (intern (string-upcase func-name) :cl-user) args)))


(defun parse-list-to-forms (lst)
  "Convert a list of C-style strings to a list of Lisp forms (do not evaluate)."
  (mapcar #'parse-c-call lst))

(defun read-file-string (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun write-list-as-sexp (lst filename)
  (with-open-file (stream filename :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (pprint lst stream)))

(defun execute-instructions-sequentially (forms)
  "Execute a list of forms sequentially, summing the numeric results."
  (let ((sum 0))
    (dolist (form forms sum)
      (let ((result (eval form)))
        (when (numberp result)
          (incf sum result))))))
(setf *do* t)

(let ((result (execute-instructions-sequentially
               (parse-list-to-forms
                (fix-dont
                 (fix-do
                  (filter-slop
                   (read-file-string "/home/rye/development/lisp/advent-of-code-2024/day3/day3.dat"))))))))
  (format t "Result: ~a~%" result)
  result)
