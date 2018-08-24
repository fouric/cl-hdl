(in-package :cl-hdl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defun n-spaces (n)
  (let (arr)
    (dotimes (i n)
      (push #\Space arr))
    (coerce arr 'string)))
