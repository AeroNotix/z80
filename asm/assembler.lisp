(in-package :z80-asm)

(defparameter 8-bit-operand
  '((A . #b111)
    (B . #b000)
    (C . #b001)
    (D . #b010)
    (E . #b011)
    (H . #b100)
    (L . #b101)
    (HL . #b110)))

(defparameter 8-bit-opcodes
  '((LD . #x40)))

(defun assembly->machine (assembly-operation)
  (destructuring-bind (operation operand-x operand-y) assembly-operation
    (logior
     (assoc-value 8-bit-opcodes operation)
     (ash (assoc-value 8-bit-operand operand-x) 3)
     (assoc-value 8-bit-operand operand-y))))

(defun generate-all-8-bit-loads ()
  (let ((registers '(A B C D E H L HL)))
    (mapcar (lambda (pair) (cons 'LD pair))
            (remove '(HL HL)
                    (apply #'alexandria:map-product #'list (list registers registers))
                     :test #'equalp))))
