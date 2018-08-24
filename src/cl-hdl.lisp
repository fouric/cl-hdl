(in-package :cl-hdl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

;;; NOTE: currently, little to no name translation is being done on symbols/identifiers - if you name your module my-module, IT WILL BREAK

(defparameter *example-source*
  `(
    (timescale (1 ns) (1 ps))
    (define "dly" \#1)
    (// "this comment will be embedded into the output Verilog code")
    (module "tx" ((input reg "clk_master")
                  (input "trigger")
                  (output "out")
                  (output "vdd"))
            (// "send consecutive integers")
            (reg (7 0) "next_byte")
            (assign "vdd" 1))
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
  (labels ((emit (control-string &rest format-arguments)
             (apply #'iformat indentation t (concatenate 'string control-string "~%") format-arguments))
           (indent-emit (control-string &rest format-arguments)
             (apply #'iformat (1+ indentation) t (concatenate 'string control-string "~%") format-arguments))
           (sameline-emit (control-string &rest format-arguments)
             (apply #'iformat (1+ indentation) t (concatenate 'string control-string "") format-arguments))
           (generate-signal ()
             (let ((range (if (listp (second form))
                              (format nil "[~a:~a] " (first (second form)) (second (second form)))
                              ""))
                   ;; rest will be like (foo 255) or (foo (7 0) 255) or (foo (7 0))
                   (rest (if (listp (second form))
                             (nthcdr 2 form)
                             (nthcdr 1 form))))
               (let ((name (first rest))
                     (words (if (and (listp (second rest)) (second rest))
                                (format nil " [~a:~a]" (first (second rest)) (second (second rest)))
                                ""))
                     (init (if (listp (second rest))
                               (third rest)
                               (second rest))))
                 (format t "~a~a~a~a" range name words (if init (format nil " = ~a" init) ""))))))
    (case (first form)
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
       ;; TODO: parameters (turns out that #(...) creates a simple-vector in CL, we could use this...), "endmodule : name"
       ;; http://verilog.renerta.com/mobile/source/vrg00026.htm
       (let ((name (nth 1 form))
             (port-list (nth 2 form)))
         (emit "module ~a" name)
         (indent-emit "(")
         (dolist (port (butlast port-list))
           (generate-verilog-form port (1+ indentation))
           ;; wait, this breaks on comments
           (emit ","))
         (alexandria:when-let ((port (first (last port-list))))
           (generate-verilog-form port (1+ indentation)))
         (fresh-line)
         (indent-emit ");")
         (dolist (form (nthcdr 3 form))
           (generate-verilog-form form (1+ indentation)))
         (emit "endmodule")))
      (reg
       ;; WAIT - WHAT IF WE NEED TO PARSE SUBEXPRESSIONS TO SUBSTITUTE IN PARAMETERS OR MACROS
       ;; TODO: do that
       ;; like (reg foo), (reg (7 0) foo), (reg (7 0) foo 255), (reg (7 0) rom (15 0) 0), (reg rom (15 0) 0)
       ;; ok this is going to be tricky
       (format t "reg ")
       (generate-signal))
      (wire
       (format t "wire ")
       (generate-signal))
      (otherwise
       (let ((first (first form)))
         (cond
           ((member first '(input output inout))
            (sameline-emit (concatenate 'string (string-downcase (string first)) " "))
            (generate-signal))
           (t
            (emit "unimplemented form: ~s" form))))))))

(defun generate-verilog (source)
  (dolist (form source)
    (fresh-line)
    (generate-verilog-form form)))

(defun test ()
  (generate-verilog *example-source*))
