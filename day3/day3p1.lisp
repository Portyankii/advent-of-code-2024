(ql:quickload :split-sequence)
(ql:quickload :cl-ppcre)


(defun mul (x y)
  "im to lazy to convert mul to *. that is the only reason this exists"
  (* x y))

(defun filter-slop (str)
  (cl-ppcre:all-matches-as-strings "mul\\([0-9]+,[0-9]+\\)" str))


(defun parse-c-call (s)
  "Parse a C-style call string like \"foo(1,2)\" to a Lisp list like (foo 1 2)."
  (let* ((open-paren (position #\( s))
         (close-paren (position #\) s :from-end t))
         (func-name (subseq s 0 open-paren))
         (args-str (subseq s (1+ open-paren) close-paren))
         (args (mapcar #'read-from-string
                       (split-sequence:split-sequence #\, args-str))))
    `(,(intern (string-upcase func-name) :cl-user) ,@args)))

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

(write-list-as-sexp
 (parse-list-to-forms (filter-slop (read-file-string "/home/rye/development/lisp/advent-of-code-2024/day2/day3/day3.dat")))
 "output.txt")
(parse-list-to-forms (filter-slop (read-file-string "/home/rye/development/lisp/advent-of-code-2024/day2/day3/day3.dat")))
