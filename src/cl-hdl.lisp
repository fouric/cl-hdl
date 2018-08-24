(in-package :cl-hdl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

;;; NOTE: currently, little to no name translation is being done on symbols/identifiers - if you name your module my-module, IT WILL BREAK

(defparameter *example-source*
  `(
    (timescale (1 ns) (1 ps))
    (define "dly" \#1)
    (// "this comment will be embedded into the output Verilog code")
    (module "tx" ((define ten 10))
            (timescale (1 ns) (100 ps)))
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
              (precision-value precision-units) "Invalid timescale precision: (~S ~S)" precision-value precision-units))))

;; http://verilog.renerta.com/mobile/source/vrg00008.htm
;; this could probably be improved by having a case-bind expression:
;; (case-bind form
;;   (timescale (scale precision)
;;     ...)
;;   (define (name text)
;;     ...)
;;   (module (name (port-list))
;;     ...)
(defun generate-verilog-form (form &optional (indentation 0))
  (flet ((emit (control-string &rest format-arguments)
           (apply #'iformat indentation t (concatenate 'string control-string "~%") format-arguments))
         (indent-emit (control-string &rest format-arguments)
           (apply #'iformat (1+ indentation) t (concatenate 'string control-string "~%") format-arguments)))
    (ccase (first form)
      (timescale
       ;; like (timescale (1 ns) (1 ps))
       (let ((scale (nth 1 form))
             (precision (nth 2 form)))
         (emit "`timescale ~a ~a / ~a ~a" (first scale) (string-downcase (string (second scale))) (first precision) (string-downcase (string (second precision))))))
      (define
       ;; does NOT support macro arguments at the moment, mainly because I'm unsure as how to convey the optional nature of the arguments
       ;; like (define "dly" \#1)
       (let ((name (nth 1 form))
             (text (nth 2 form)))
         (emit "`define ~a ~a" name text)))
      (//
       (emit "// ~a" (second form)))
      (/*
       (emit "/* ~a */" (second form)))
      (module
       ;; TODO: parameters, "endmodule : name"
       ;; http://verilog.renerta.com/mobile/source/vrg00026.htm
       (let ((name (nth 1 form))
             (port-list (nth 2 form)))
         (emit "module ~a" name)
         (indent-emit "(")
         (dolist (port port-list)
           (generate-verilog-form port (1+ indentation)))
         (indent-emit ");")
         (dolist (form (nthcdr 3 form))
           (generate-verilog-form form (1+ indentation)))
         (emit "endmodule"))))))

(defun generate-verilog (source)
  (dolist (form source)
    (fresh-line)
    (generate-verilog-form form)))

(defun test ()
  (generate-verilog *example-source*))
