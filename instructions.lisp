(in-package :z80)


(defclass instruction ()
  ((name :initarg :name :accessor name)
   (size :initarg :size :accessor size)
   ;; TODO: Implement cycle counting, will be for timing/R register
   ;(cycles :initarg :cycles)
   (microcode :initarg :microcode :accessor microcode)))

(defmethod print-object ((int instruction) out)
  (format out "<INSTRUCTION {~A}>" (name int)))

(defmacro define-instruction (name size args &body body)
  (let ((microcode-fn-name
         (intern (concatenate 'string (symbol-name name) "-MICROCODE"))))
    `(flet ((,microcode-fn-name ,args
              (progn (when logging-enabled
                       (format t "Executing ~A~%" ,name))
                     ,@body)))
       (let* ((inst (make-instance 'instruction
                                   :name ',name
                                   :size ,size
                                   :microcode #',microcode-fn-name)))
         (defparameter ,name inst)))))

;; From: http://www.z80.info/decoding.htm

;; Essentially:

;; 7   6   5   4   3   2   1   0
;; |-x-|   |---y---|   |---z---|
;;         |-p-|   q

(defun opcode-y (opcode)
  (logand (rshift opcode 3) #x07))

(defun opcode-z (opcode)
  (logand opcode #x07))

(defun opcode-p (opcode)
  (logand (rshift opcode 4) #x03))

(defun opcode-q (opcode)
  (logand (rshift opcode 3) #x03))

(defun push% (cpu &rest values)
  (loop for value in values
     do
       (decf (reg-sp cpu))
       (setf (mem-sp cpu) value)))

(defun push-from (cpu place)
  (let ((upper-reg (car (16-bit-register->8-bit-registers place)))
        (lower-reg (cdr (16-bit-register->8-bit-registers place))))
    (push% cpu (funcall upper-reg cpu) (funcall lower-reg cpu))))

(defun pop-to (cpu place)
  (let ((upper-reg (car (16-bit-register->8-bit-registers place)))
        (lower-reg (cdr (16-bit-register->8-bit-registers place))))
    (setf (mem-sp cpu) (funcall upper-reg cpu))
    (incf (reg-sp cpu))
    (setf (mem-sp cpu) (funcall lower-reg cpu))
    (incf (reg-sp cpu))))

(defun ret (cpu)
  (let ((lower (mem-sp cpu)))
    (incf (reg-sp cpu))
    (let ((upper (mem-sp cpu)))
      (incf (reg-sp cpu))
      (setf (pc cpu) (logior (ash upper 8) lower)))))

(defun call (cpu address)
  (incf (pc cpu) 3)
  (push% cpu (reg-pc-p cpu) (reg-pc-c cpu))
  (setf (pc cpu) address))

(defun ld (c y z)
  (funcall (setf-of y) (funcall z c) c))

(defun calculate-flags (value)
  (let ((c-flag (if (> value 255) c-mask 0))
        (z-flag (if (eq value 0) z-mask 0))
        (p-flag (if (eq (8-bit-parity value) 1) p-mask 0))
        (s-flag (if (< 127 value 256) s-mask 0))
        (n-flag 0)
        (h-flag h-mask)) ;; not-correct
    (logior c-flag z-flag p-flag s-flag n-flag h-flag)))

(defun inc (c y &key (amount 1))
  (let* ((result (+ amount (funcall y c)))
         (next-flags (calculate-flags result)))
    (setf (reg-f c) next-flags)
    (funcall (setf-of y) result c)))

(defun dec (c y &key (amount 1))
  (let* ((result (- (funcall y c) amount))
         (next-flags (calculate-flags result)))
    (setf (reg-f c) next-flags)
    (funcall (setf-of y) result c)))

(defun find-8-bit-register (i)
  (elt (list 'reg-b 'reg-c 'reg-d 'reg-e 'reg-h 'reg-l 'mem-hl 'reg-a) i))

(defun find-16-bit-register (i)
  (elt (list 'reg-bc 'reg-de 'reg-hl 'reg-sp) i))

(defun find-16-bit-register% (i)
  (elt (list 'reg-bc 'reg-de 'reg-hl 'reg-af) i))

(define-instruction nop #x1 (cpu opcode))

(define-instruction halt #x1 (cpu opcode)
  (setf (slot-value cpu 'halted?) t))

;; 8-bit register loading LD, r[y], r[z]
(define-instruction ld-r-r #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode)))
        (z (find-8-bit-register (opcode-z opcode))))
    (ld cpu y z)))

;; 8-bit load immediate LD, r[y], n
(define-instruction ld-r-n #x2 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (ld cpu y #'fetch-byte-from-ram)))

;; 16-bit load immediate LD rp[p], nn
(define-instruction ld-rr-nn #x3 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (ld cpu p #'fetch-word-from-ram)))

(define-instruction ld-r-indirect-hl #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (ld cpu y #'mem-hl)))

(define-instruction ld-indirect-hl-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (ld cpu 'mem-hl z)))

(define-instruction ld-indirect-bc-a #x1 (cpu opcode)
  (setf (mem-bc cpu) (reg-a cpu)))

(define-instruction ld-indirect-de-a #x1 (cpu opcode)
  (setf (mem-de cpu) (reg-a cpu)))

(define-instruction ld-a-indirect-de #x1 (cpu opcode)
  (setf (reg-a cpu) (mem-de cpu)))

(define-instruction ld-a-indirect-bc #x1 (cpu opcode)
  (setf (reg-a cpu) (mem-bc cpu)))

(define-instruction ld-a-indirect-nn #x3 (cpu opcode)
  (setf (reg-a cpu) (elt (ram cpu) (fetch-word-from-ram cpu))))

(define-instruction ld-indirect-nn-hl #x3 (cpu opcode)
  (setf (elt (ram cpu) (fetch-word-from-ram cpu)) (reg-hl cpu)))

(define-instruction ld-indirect-nn-a #x3 (cpu opcode)
  (setf (elt (ram cpu) (fetch-word-from-ram cpu)) (reg-a cpu)))

(define-instruction ld-hl-indirect-nn #x3 (cpu opcode)
  (setf (reg-hl cpu) (elt (ram cpu) (fetch-word-from-ram cpu))))

(define-instruction ld-indirect-hl-n #x2 (cpu opcode)
  (setf (mem-hl cpu) (fetch-byte-from-ram cpu)))

(define-instruction ld-sp-hl #x1 (cpu opcode)
  (setf (reg-sp cpu) (reg-hl cpu)))

(define-instruction inc-rr #x1 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (inc cpu p)))

(define-instruction inc-r #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (inc cpu y)))

(define-instruction inc-indirect-hl #x1 (cpu opcode)
  (incf (elt (ram cpu) (reg-hl cpu))))

(define-instruction dec-indirect-hl #x1 (cpu opcode)
  (decf (elt (ram cpu) (reg-hl cpu))))

(define-instruction dec-r #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (dec cpu y)))

(define-instruction dec-rr #x1 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (dec cpu p)))

(define-instruction scf #x1 (cpu opcode)
  (funcallf #'logior (reg-f cpu) c-mask))

(define-instruction ccf #x1 (cpu opcode)
  (error "Not implemented (ccf): invert carry flag"))

(define-instruction cpl #x1 (cpu opcode)
  (error "Not implemented (cpl): flip bits in reg-a"))

(define-instruction cp-r #x1 (cpu opcode)
  (let* ((z (find-8-bit-register (opcode-z opcode)))
         (result (- (reg-a cpu) (funcall z cpu)))
         (next-flags (calculate-flags result)))
    (setf (reg-f cpu) next-flags)))

(define-instruction cp-n #x2 (cpu opcode)
  (let* ((value (fetch-byte-from-ram cpu))
         (result (- (reg-a cpu) value))
         (next-flags (calculate-flags result)))
    (setf (reg-f cpu) next-flags)))

(define-instruction daa #x1 (cpu opcode)
  (let ((upper-tetrade (logand (reg-a cpu) #xF0))
        (lower-tetrade (logand (reg-a cpu) #x0F))
        (next-a (reg-a cpu)))
    (when (or (> upper-tetrade #x90) (flag-c cpu))
      (incf next-a #x60))
    (when (or (> lower-tetrade #x09) (flag-h cpu))
      (incf next-a #x06))))

(define-instruction rra #x1 (cpu opcode)
  (error "Not implemented (rra): rotate a right, copy flags accordingly"))

(define-instruction rla #x1 (cpu opcode)
  (error "Not implemented (rla): rotate a left, copy flags accordingly"))

(define-instruction rlca #x1 (cpu opcode)
  (let ((bit-7 (rshift (reg-a cpu) 7)))
    (setf (flag-c cpu) bit-7)
    (setf (reg-a cpu) (logior (ash (reg-a cpu) 1) bit-7))))

(define-instruction rrca #x1 (cpu opcode)
  (error "Not implemented (rrca): rotate a left, copy flags accordingly"))

(define-instruction ex-af-af% #x1 (cpu opcode)
  (rotatef (reg-af cpu) (reg-af% cpu)))

(define-instruction exx #x1 (cpu opcode)
  (rotatef (reg-bc cpu) (reg-bc% cpu))
  (rotatef (reg-de cpu) (reg-de% cpu))
  (rotatef (reg-hl cpu) (reg-hl% cpu)))

(define-instruction ex-indirect-sp-hl #x1 (cpu opcode)
  (rotatef (mem-sp cpu) (reg-hl cpu)))

(define-instruction ex-hl-hl% #x1 (cpu opcode)
  (rotatef (reg-hl cpu) (reg-hl% cpu)))

(define-instruction ex-de-hl #x1 (cpu opcode)
  (rotatef (reg-de cpu) (reg-hl cpu)))

(define-instruction add-hl-rr #x1 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (inc cpu #'reg-hl :amount (funcall p cpu))))

(define-instruction add-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (inc cpu #'reg-a :amount (funcall z cpu))))

(define-instruction add-n #x1 (cpu opcode)
  (let ((value (fetch-byte-from-ram cpu)))
    (inc cpu #'reg-a :amount value)))

(define-instruction adc-n #x1 (cpu opcode)
  (let ((value (fetch-byte-from-ram cpu)))
    (inc cpu #'reg-a :amount (+ value (flag-c cpu)))))

(define-instruction adc-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (inc cpu #'reg-a :amount (+ (funcall z cpu) (flag-c cpu)))))

(define-instruction sub-r #x1 (cpu opcode)
  (error "Not implemented: sub r from reg-a"))

(define-instruction sub-n #x1 (cpu opcode)
  (error "Not implemented: sub n from reg-a"))

(define-instruction sbc-r #x1 (cpu opcode)
  (error "Not implemented: sub r and carry flag from reg-a"))

(define-instruction sbc-n #x1 (cpu opcode)
  (error "Not implemented: sub n and carry flag from reg-a"))

(define-instruction and-r #x1 (cpu opcode)
  (warn "and-r doesn't set flags, yet")
  (funcallf #'logand (reg-a cpu) (funcall (find-8-bit-register (opcode-z opcode)) cpu)))

(define-instruction and-n #x2 (cpu opcode)
  (warn "and-n doesn't set flags, yet")
  (funcallf #'logand (reg-a cpu) (fetch-byte-from-ram cpu)))

(define-instruction xor-r #x1 (cpu opcode)
  (warn "xor-r doesn't set flags, yet")
  (funcallf #'logxor (reg-a cpu) (funcall (find-8-bit-register (opcode-z opcode)) cpu)))

(define-instruction xor-n #x2 (cpu opcode)
  (warn "xor-n doesn't set flags, yet")
  (funcallf #'logxor (reg-a cpu) (fetch-byte-from-ram cpu)))

(define-instruction or-r #x1 (cpu opcode)
  (warn "or-r doesn't set flags, yet")
  (funcallf #'logior (reg-a cpu)
          (funcall (find-8-bit-register (opcode-z opcode)) cpu)))

(define-instruction or-n #x2 (cpu opcode)
  (warn "or-n doesn't set flags, yet")
  (setf (reg-a cpu) (logior (reg-a cpu) (fetch-byte-from-ram cpu))))

(define-instruction jr-cc-d #x2 (cpu opcode)
  ;; +2 to the jump offset as the jr-cc-d instruction is two bytes itself
  (let ((jump-offset (+ 2 (unsigned->signed (fetch-byte-from-ram cpu) 1)))
        (jump-condition (find-condition (opcode-q opcode))))
    (when (funcall jump-condition cpu)
      (incf (pc cpu) jump-offset))))

(define-instruction jp-cc-nn #x3 (cpu opcode)
  (let ((jump-address (fetch-byte-from-ram cpu))
        (jump-condition (find-condition (opcode-q opcode))))
    (when (funcall jump-condition cpu)
      (setf (pc cpu) jump-address))))

(define-instruction jp-nn #x3 (cpu opcode)
  (let ((jump-address (fetch-word-from-ram cpu)))
    (setf (pc cpu) jump-address)))

(define-instruction jp-hl #x3 (cpu opcode)
  (let ((jump-address (reg-hl cpu)))
    (setf (pc cpu) jump-address)))

(define-instruction jr-d #x2 (cpu opcode)
  (let ((jump-offset (unsigned->signed (fetch-byte-from-ram cpu) 1)))
    (incf (pc cpu) jump-offset)))

(define-instruction djnz-d #x2 (cpu opcode)
  (let ((jump-offset (unsigned->signed (fetch-byte-from-ram cpu) 1)))
    (when (flag-nz cpu)
      (incf (pc cpu) jump-offset))))

(define-instruction push-register #x1 (cpu opcode)
  (let ((register (find-16-bit-register% (opcode-p opcode))))
    (push-from cpu register)))

(define-instruction pop-register #x1 (cpu opcode)
  (let ((register (find-16-bit-register% (opcode-p opcode))))
    (pop-to cpu register)))

(define-instruction ret-cc #x1 (cpu opcode)
  (let ((condition (find-condition (opcode-y opcode))))
    (when (funcall condition cpu)
      (ret cpu))))

(define-instruction pop-cc #x1 (cpu opcode)
  (let ((condition (find-condition (opcode-y) cpu)))
    (when (funcall condition cpu)
      (pop cpu))))

(define-instruction call-cc-nn #x3 (cpu opcode)
  (let ((condition (find-condition (opcode-y cpu))))
    (when (funcall condition cpu)
      (call cpu (fetch-word-from-ram cpu)))))

(define-instruction call-nn #x3 (cpu opcode)
  (call cpu (fetch-word-from-ram cpu)))

(define-instruction rst-p #x1 (cpu opcode)
  (let ((y (opcode-y opcode))
        (upper-pc (reg-pc-p cpu))
        (lower-pc (reg-pc-c cpu)))
    (push% cpu upper-pc lower-pc)
    (setf (pc cpu) (find-rst-address y))))

(define-instruction ret #x1 (cpu opcode)
  (ret cpu))

(define-instruction out-n-a #x2 (cpu opcode)
  (let ((port-id (fetch-byte-from-ram cpu)))
    (write-port cpu port-id (reg-a cpu))))

(define-instruction in-a-n #x2 (cpu opcode)
  (let ((port-id (fetch-byte-from-ram cpu)))
    (setf (reg-a cpu) (read-port cpu port-id) )))

(define-instruction di #x1 (cpu opcode)
  (setf (interrupts-enabled? cpu) nil))

(define-instruction ei #x1 (cpu opcode)
  (setf (interrupts-enabled? cpu) t))

;; ED Prefixed opcodes

(define-instruction ed-prefix #x0 (cpu opcode)
  (incf (reg-pc cpu))
  (execute-next-instruction cpu ed-prefix-table))

(define-instruction ed-undefined #x1 (cpu opcode))

(define-instruction in-r-c #x2 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y cpu))))
    (ld cpu y 'port-c)))

(define-instruction out-c-r #x2 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (ld cpu 'port-c y)))

(define-instruction sbc-hl-rr #x2 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (dec cpu #'reg-hl (+ (funcall p cpu) (flag-c cpu)))))

(define-instruction ld-indirect-nn-rr #x4 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode)))
        (address-to-write-to (fetch-word-from-ram cpu)))
    (write-word-to-ram cpu address-to-write-to (funcall p cpu))))

(define-instruction neg #x2 (cpu opcode)
  (funcallf #'twos-complement (reg-a cpu)))

(define-instruction reti-retn #x2 (cpu opcode)
  (warn "reti-retn is supposed to also set IFF1 to IFF2, we don't implement either of those yet")
  (pop-to cpu #'reg-pc))

(define-instruction im-n #x2 (cpu opcode)
  (setf (interrupts-enabled? cpu) (= 1 (fetch-byte-from-ram cpu))))

(define-instruction ld-i-a-ld-r-a #x2 (cpu opcode)
  (warn "ld-i-a-ld-r-a may not be implemented correctly")
  (case (opcode-y opcode)
    (0 (ld cpu #'reg-i #'reg-a))
    (1 (ld cpu #'reg-r #'reg-a))
    (2 (ld cpu #'reg-a #'reg-i))
    (3 (ld cpu #'reg-a #'reg-r))))

(define-instruction ld-a-i-ld-a-r #x2 (cpu opcode)
  (warn "ld-a-i-ld-a-r may not be implemented correctly")
  (case (opcode-y opcode)
    (0 (ld cpu #'reg-i #'reg-a))
    (1 (ld cpu #'reg-r #'reg-a))
    (2 (ld cpu #'reg-a #'reg-i))
    (3 (ld cpu #'reg-a #'reg-r))))

(define-instruction adc-hl-rr #x2 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (inc cpu #'reg-hl (+ (funcall p cpu) (flag-c cpu)))))

(define-instruction ld-rr-indirect-nn #x4 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode)))
        ;; two bytes for the actual opcode, then the address
        (address (read-word-from-ram cpu :address (+ 2 (pc cpu)))))
    (warn "ld-rr-indirect-nn will need some optimization, loading from a closure")
    (ld cpu p (lambda (cpu) (read-byte-from-ram cpu :address address)))))

(define-instruction rld-rrd #x2 (cpu opcode)
  (let ((a-upper-tetrade (logand #xFF00 (reg-a cpu)))
        (a-lower-tetrade (logand #x00FF (reg-a cpu)))
        (mem-hl-upper-tetrade (logand #xFF00 (mem-hl cpu)))
        (mem-hl-lower-tetrade (logand #x00FF (mem-hl cpu))))
    ;; define this
    (when (= opcode #x6F)
      (setf (reg-a cpu) (logand mem-hl-upper-tetrade a-upper-tetrade))
      (setf (mem-hl cpu) (logand (ash mem-hl-lower-tetrade 8) a-lower-tetrade)))
    (when (= opcode #x67)
      (setf (reg-a cpu) (logand a-upper-tetrade mem-hl-lower-tetrade))
      (setf (mem-hl cpu) (logand (ash a-lower-tetrade 8) (rshift mem-hl-upper-tetrade 8))))))

#|

Block instructions are all supposed to decrement PC by two if their
conditions are not met. We don't implement this feature because, in
reality it's a bit of a hack in the Z80 hardware.

The opcode is two bytes long and by decrementing the PC by two
effectively these instructions trigger a jr-2 instruction.

For us, it's more efficient to just loop through until the counters
reach zero.

|#

(defmethod block-move-instruction ((cpu cpu) op)
  (setf (mem-de cpu) (mem-hl cpu))
  (funcallf op (reg-hl cpu))
  (funcallf op (reg-de cpu))
  (decf (reg-bc cpu)))

(define-instruction ldi #x2 (cpu opcode)
  (block-move-instruction cpu #'1+))

(define-instruction ldd #x2 (cpu opcode)
  (block-move-instruction cpu #'1-))

(define-instruction ldir #x2 (cpu opcode)
  (while (plusp (reg-bc cpu))
    (block-move-instruction cpu #'1+)))

(define-instruction lddr #x2 (cpu opcode)
  (while (plusp (reg-bc cpu))
    (block-move-instruction cpu #'1-)))

(defmethod block-compare-instruction ((cpu cpu) op)
  (let* ((result (- (reg-a cpu) (mem-hl cpu))))
    (setf (reg-f cpu) (calculate-flags result))
    (funcallf op (reg-hl cpu))
    (funcallf op (reg-bc cpu))))

(define-instruction cpi #x2 (cpu opcode)
  (block-compare-instruction cpu #'1+))

(define-instruction cpir #x2 (cpu opcode)
  (while (and (/= (reg-a cpu) (mem-hl cpu))
              (plusp (reg-bc cpu)))
    (block-compare-instruction cpu #'1+)))

(define-instruction cpd #x2 (cpu opcode)
  (block-compare-instruction cpu #'1-))

(define-instruction cpdr #x2 (cpu opcode)
  (while (and (/= (reg-a cpu) (mem-hl cpu))
              (plusp (reg-bc cpu)))
    (block-compare-instruction cpu #'1-)))

(defun block-input-instruction (cpu op)
  (setf (mem-hl cpu) (read-port cpu (reg-c cpu)))
  (decf (reg-b cpu))
  (funcallf op (reg-hl cpu)))

(define-instruction ini #x2 (cpu opcode)
  (block-input-instruction cpu #'1+))

(define-instruction ind #x2 (cpu opcode)
  (block-input-instruction cpu #'1-))

(define-instruction inir #x2 (cpu opcode)
  (while (plusp (reg-b cpu))
    (block-input-instruction cpu #'1+)))

(define-instruction indr #x2 (cpu opcode)
  (while (plusp (reg-b cpu))
    (block-input-instruction cpu #'1-)))

(defun block-output-instruction (cpu op)
  (setf (port-c cpu) (mem-hl cpu))
  (decf (reg-b cpu))
  (funcallf op (reg-hl cpu)))

(define-instruction outi #x2 (cpu opcode)
  (block-output-instruction cpu #'1+))

(define-instruction outd #x2 (cpu opcode)
  (block-output-instruction cpu #'1-))

(define-instruction otir #x2 (cpu opcode)
  (while (plusp (reg-b cpu))
    (block-output-instruction cpu #'1+)))

(define-instruction otdr #x2 (cpu opcode)
  (while (plusp (reg-b cpu))
    (block-output-instruction cpu #'1-)))

;; CB Prefixed opcodes

(define-instruction cb-prefix #x2 (cpu opcode)
  (incf (reg-pc cpu))
  (execute-next-instruction cpu cb-prefix-table))

#|

All rotation instructions likely can be refactored to a single
function which knows which direction to go in.

|#

(define-instruction rlc-r #x2 (cpu opcode)
  (let* ((z (find-8-bit-register (opcode-z opcode)))
         (z-value (funcall z cpu))
         (next-flags (calculate-flags z-value))
         ;; put #x80 in a define
         (z-msb (rshift (logand z-value #x80) 7)))
    (funcall (setf-of z) (logior (ash z-value 1) z-msb) cpu)
    ;; logior next-flags z-msb might be wrong. We might need to set
    ;; the c-flag to the z-msb no matter what
    (setf (reg-f cpu) (logior next-flags z-msb))))

(define-instruction rlc-indirect-hl #x2 (cpu opcode)
  (let* ((z-value (mem-hl cpu))
         (next-flags (calculate-flags z-value))
         ;; put #x80 in a define
         (z-msb (rshift (logand z-value #x80) 7)))
    (setf (mem-hl cpu) (logior (ash z-value 1) z-msb))
    ;; logior next-flags z-msb might be wrong. We might need to set
    ;; the c-flag to the z-msb no matter what
    (funcallf #'logior (reg-f cpu) z-msb)))

(define-instruction rrc-r #x2 (cpu opcode)
  (let* ((z (find-8-bit-register (opcode-z opcode)))
         (z-value (funcall z cpu))
         (next-flags (calculate-flags z-value))
         (z-lsb (logand z-value #x1)))
    (funcall (setf-of z) (logior (rshift z-value 1) z-lsb) cpu)
    (setf (reg-f cpu) (logior next-flags z-lsb))))

(define-instruction rrc-indirect-hl #x2 (cpu opcode)
  (let* ((z (mem-hl cpu))
         (z-value (funcall z cpu))
         (next-flags (calculate-flags z-value))
         (z-lsb (ash (logand z-value #x1) 7)))
    (setf (mem-hl cpu) (logior (rshift z-value 1) z-lsb))
    (funcallf #'logior (reg-f cpu) z-lsb)))

(define-instruction rl-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu))
        (z-msb (rshift (logand z-value #x80) 7))
        (c (flag-c cpu)))
    (funcall (setf-of z) (logand (ash z-value 1) c))
    (funcallf #'logand (reg-f cpu) z-msb)))

(define-instruction rl-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu))
        (z-msb (rshift (logand z-value #x80) 7))
        (c (flag-c cpu)))
    (setf (mem-hl cpu) (logand (ash z-value 1) c))
    (funcallf #'logand (reg-f cpu) z-msb)))

(define-instruction rr-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu))
        (z-lsb (logand z-value #x1))
        (c (flag-c cpu)))
    (funcall (setf-of z) (logior (rshift z-value 1) (ash c-flag 7)) cpu)
    (funcallf #'logior (reg-f cpu) z-lsb)))

(define-instruction rr-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu))
        (z-lsb (logand z-value #x1))
        (c (flag-c cpu)))
    (setf (mem-hl cpu) (logior (rshift z-value 1) (ash c-flag 7)))
    (setf (reg-f cpu) (logior (reg-f cpu) z-lsb))))

(define-instruction sla-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu))
        (z-msb (rshift (logand z-value #x80) 7)))
    (funcall (setf-of z) (ash z-value 1) cpu)
    (funcallf #'logior (reg-f cpu) z-msb)))

(define-instruction sla-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu))
        (z-msb (rshift (logand z-value #x80) 7)))
    (setf (mem-hl cpu) (ash z-value 1))
    (funcallf #'logior (reg-f cpu) z-msb)))

(define-instruction sra-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu)))
    (funcall (setf-of z) (rshift (funcall z cpu) 1) cpu)
    (setf (reg-f cpu) (logior (reg-f cpu) (logand #x1 z-value)))))

(define-instruction sra-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu)))
    (setf (mem-hl cpu) (rshift z-value 1))
    (setf (reg-f cpu) (logior (reg-f cpu) (logand #x1 z-value)))))

(define-instruction sll-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu))
        (z-msb (rshift (logand z-value #x80) 7)))
    (funcall (setf-of z) (logior (ash z-value 1) #x1))
    (funcallf #'logior (reg-f cpu) #x1)))

(define-instruction sll-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu))
        (z-msb (rshift (logand z-value #x80) 7)))
    (setf (mem-hl cpu) (logior (ash z-value 1) #x1))
    (funcallf #'logior (reg-f cpu) #x1)))

(define-instruction srl-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu))
        (z-lsb (logand z-value #x1)))
    (funcall (setf-of z) (logior (rshift z-value 1) (ash #x1 7)))
    (funcallf #'logior (reg-f cpu) z-lsb)))

(define-instruction srl-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu))
        (z-lsb (logand z-value #x1)))
    (setf (mem-hl cpu) (logior (rshift z-value 1) (ash #x1 7)))
    (funcallf #'logior (reg-f cpu) z-lsb)))

(defun bit-test-flags (value position)
  (logior h-mask (if (logbitp position value) z-mask 0)))

(define-instruction bit-b-r #x2 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode)))
        (z-value (funcall z cpu))
        (y (opcode-y opcode)))
    (setf (reg-f cpu) (bit-test-flags z-value y))))

(define-instruction bit-b-indirect-hl #x2 (cpu opcode)
  (let ((z-value (mem-hl cpu))
        (y (opcode-y opcode)))
    (setf (reg-f cpu) (bit-test-flags z-value y))))

(defun reset/set-bits (cpu opcode res/set-fn)
  (let* ((z (find-8-bit-register (opcode-z opcode)))
         (z-value (funcall z cpu))
         (y (opcode-y opcode)))
    (funcall (setf-of z) (funcall res/set-fn z-value y) cpu)))

(define-instruction res-b-r #x2 (cpu opcode)
  (reset/set-bits cpu opcode #'reset-bit-at))

(define-instruction set-b-r #x2 (cpu opcode)
  (reset/set-bits cpu opcode #'set-bit-at))

(define-instruction dd-prefix #x2 (cpu opcode)
  (error "Not implemented: dd-prefixes, switch the instruction table to dd-prefixed"))

(define-instruction fd-prefix #x2 (cpu opcode)
  (error "Not implemented: fd-prefixes, switch the instruction table to fd-prefixed"))
