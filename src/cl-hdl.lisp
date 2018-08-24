(in-package :cl-hdl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defparameter *example-source*
  `(
    (timescale (1 ns) (1 ps))
    ))

(defparameter *timescale-units* '(s ms us ns ps fs))

(defun check-timescale-syntax (timescale-form)
  (let ((scale (nth 1 expression))
        (precision (nth 2 expression)))
    (let ((scale-value (first scale))
          (scale-units (second value))
          (precision-value (first precision))
          (precision-units (second precision)))
      (assert (and (integerp scale-value)
                   (not (minusp scale-value))
                   (member (scale-units) *timescale-units*))
              (scale-value scale-units) "Invalid timescale scale: (~S ~S)" scale-value scale-units)
      (assert (and (integerp precision-value)
                   (not (minusp precision-value))
                   (member (precision-units *timescale-units*)))
              (precision-value precision-units) "Invalid timescale precision: (~S ~S)" precision-value precision-units)
      )))

(defun generate-verilog (source)
  (dolist (expression source)
    (fresh-line)
    (case (first expression)
      (timescale
       ;; like (timescale (1 ns) (1 ps))
       (let ((scale (nth 1 expression))
             (precision (nth 2 expression)))
         (format t "`timescale ~a ~a / ~a ~a~%" (first scale) (string-downcase (string (second scale))) (first precision) (string-downcase (string (second precision)))))))))

(defun test ()
  (generate-verilog *example-source*))
