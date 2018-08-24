(in-package :cl-hdl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defun n-characters (n character)
  (let (arr)
    (dotimes (i n)
      (push character arr))
    (coerce arr 'string)))

(defun iformat (indentation destination control-string &rest format-arguments)
  "indent + FORMAT"
  (when (plusp indentation)
    (format t (n-characters indentation #\Tab)))
  (apply #'format destination control-string format-arguments))
