(in-package :cl-hdl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defun *example-source*
    `((timescale (1 ns) (1 ps))
      ))

(defun generate-verilog ()
  (format t "hello world!~%"))
