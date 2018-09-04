(in-package :z80-asm)

(defparameter 8-bit-operand-lower
  '((A . #b111)
    (B . #b000)
    (C . #b001)
    (D . #b010)
    (E . #b011)
    (H . #b100)
    (L . #b101)
    (HL . #b110)))

(defparameter 8-bit-operand-higher
  '((A . #b111000)
    (B . #b000000)
    (C . #b001000)
    (D . #b010000)
    (E . #b011000)
    (H . #b100000)
    (L . #b101000)
    (HL . #b110000)))

(defparameter 8-bit-opcodes
  '((LD . #x40)))

(defun assembly->machine (assembly-operation)
  (destructuring-bind (operation operand-x operand-y) assembly-operation
    (logior
     (assoc-value 8-bit-opcodes operation)
     (assoc-value 8-bit-operand-higher operand-x)
     (assoc-value 8-bit-operand-lower operand-y))))

(defun generate-all-8-bit-loads ()
  (let ((registers '(A B C D E H L HL)))
    (mapcar (lambda (pair) (cons 'LD pair))
            (remove '(HL HL)
                    (apply #'alexandria:map-product #'list (list registers registers))
                     :test #'equalp))))
