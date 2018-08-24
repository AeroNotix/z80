(in-package :z80)

(defclass instruction ()
  ((name :initarg :name :accessor name)
   (size :initarg :size :accessor size)
   (cycles :initarg :cycles)
   (microcode :initarg :microcode :accessor microcode)))

(defmacro define-instruction (name size args &body body)

  `(let* ((inst (make-instance 'instruction
                               :name ',name
                               :size ,size
                               :microcode (lambda ,args ,@body))))
     (defparameter ,name inst)))

(defmacro setf-of (place)
  `(fdefinition `(setf ,,place)))

(defmacro incf-of (place)
  `(fdefinition `(incf ,,place)))

(defmacro decf-of (place)
  `(fdefinition `(decf ,,place)))

;; From: http://www.z80.info/decoding.htm

;; Essentially:

;; 7   6   5   4   3   2   1   0
;; |-x-|   |---y---|   |---z---|
;;         |-p-|   q

(defun opcode-y (opcode)
  (logand (ash opcode -3) #x07))

(defun opcode-z (opcode)
  (logand opcode #x07))

(defun opcode-p (opcode)
  (logand (ash opcode -4) #x03))

(defun opcode-q (opcode)
  (logand (ash opcode -3) #x03))

(defun push% (cpu &rest values)
  (loop for value in values
     do
       (setf (mem-sp cpu) value)
       (decf (reg-sp cpu))))

(defun push-from (cpu place)
  (let ((upper-reg (first (16-bit-register->8-bit-registers place)))
        (lower-reg (second (16-bit-register->8-bit-registers place))))
    (push% cpu (funcall upper-reg cpu) (funcall lower-reg cpu))))

(defun pop-to (cpu place)
  (let ((upper-reg (first (16-bit-register->8-bit-registers place)))
        (lower-reg (second (16-bit-register->8-bit-registers place))))
    (setf (mem-sp cpu) (funcall upper-reg cpu))
    (incf (reg-sp cpu))
    (setf (mem-sp cpu) (funcall lower-reg cpu))
    (incf (reg-sp cpu))))

(defun ret (cpu)
  (let ((lower (mem-sp cpu)))
    (incf (reg-sp cpu))
    (let ((upper (mem-sp cpu)))
      (setf (pc cpu) (logand (ash upper 8) lower)))))

(defun call (cpu address)
  (push% cpu (+ 3 (pc cpu)))
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
    (ld cpu p #'fetch-word)))

(define-instruction ld-r-indirect-hl #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (ld y cpu #'mem-hl)))

(define-instruction ld-indirect-hl-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (ld #'mem-hl cpu z)))

(define-instruction ld-indirect-bc-a #x1 (cpu opcode)
  (setf (mem-bc cpu) (reg-a cpu)))

(define-instruction ld-indirect-de-a #x1 (cpu opcode)
  (setf (mem-de cpu) (reg-a cpu)))

(define-instruction ld-a-indirect-de #x1 (cpu opcode)
  (setf (reg-a cpu) (mem-de cpu)))

(define-instruction ld-a-indirect-bc #x1 (cpu opcode)
  (setf (reg-a cpu) (mem-bc cpu)))

(define-instruction ld-a-indirect-nn #x3 (cpu opcode)
  (setf (reg-a cpu) (elt (ram cpu) (fetch-word cpu))))

(define-instruction ld-indirect-nn-hl #x3 (cpu opcode)
  (setf (elt (ram cpu) (fetch-word cpu)) (reg-hl cpu)))

(define-instruction ld-indirect-nn-a #x3 (cpu opcode)
  (setf (elt (ram cpu) (fetch-word cpu)) (reg-a cpu)))

(define-instruction ld-hl-indirect-nn #x3 (cpu opcode)
  (setf (reg-hl cpu) (elt (ram cpu) (fetch-word cpu))))

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
  (setf (reg-f cpu) (logior c-mask (reg-f cpu))))

(define-instruction ccf #x1 (cpu opcode)
  (error "Not implemented: invert carry flag"))

(define-instruction cpl #x1 (cpu opcode)
  (error "Not implemented: flip bits in reg-a"))

(define-instruction cp-r #x1 (cpu opcode)
  (error "Not implemented: subtract r from reg-a, affect flag, reg-a unaffected"))

(define-instruction cp-n #x2 (cpu opcode)
  (error "Not implemented: subtract n from reg-a, affect flag, reg-a unaffected"))

(define-instruction daa #x1 (cpu opcode)
  (error "Not implemented: adjust register a for BCD"))

(define-instruction rra #x1 (cpu opcode)
  (error "fuck-knows-what-to-do-with-this-opcode"))

(define-instruction rla #x1 (cpu opcode)
  (error "fuck-knows-what-to-do-with-this-opcode"))

(define-instruction rlca #x1 (cpu opcode)
  (error "fuck-knows-what-to-do-with-this-opcode"))

(define-instruction rrca #x1 (cpu opcode)
  (error "fuck-knows-what-to-do-with-this-opcode"))

(define-instruction ex-af-af% #x1 (cpu opcode)
  (rotatef (reg-af cpu)
           (reg-af% cpu)))

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
    (inc cpu 'reg-hl :amount (funcall p cpu))))

(define-instruction add-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (inc cpu 'reg-a :amount (funcall z cpu))))

(define-instruction add-n #x1 (cpu opcode)
  (let ((value (fetch-byte-from-ram cpu)))
    (inc cpu 'reg-a :amount value)))

(define-instruction adc-n #x1 (cpu opcode)
  (let ((value (fetch-byte-from-ram cpu)))
    (inc cpu 'reg-a :amount (+ value (flag-c cpu)))))

(define-instruction adc-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (inc cpu 'reg-a :amount (+ (funcall z cpu) (flag-c cpu)))))

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
  (setf (reg-a cpu) (logand (reg-a cpu) (funcall (find-8-bit-register (opcode-z opcode)) cpu))))

(define-instruction and-n #x2 (cpu opcode)
  (warn "and-n doesn't set flags, yet")
  (setf (reg-a cpu) (logand (reg-a cpu) (fetch-byte-from-ram cpu))))

(define-instruction xor-r #x1 (cpu opcode)
  (warn "xor-r doesn't set flags, yet")
  (setf (reg-a cpu) (logxor (reg-a cpu) (funcall (find-8-bit-register (opcode-z opcode)) cpu))))

(define-instruction xor-n #x2 (cpu opcode)
  (warn "xor-n doesn't set flags, yet")
  (setf (reg-a cpu) (logxor (reg-a cpu) (fetch-byte-from-ram cpu))))

(define-instruction or-r #x1 (cpu opcode)
  (warn "or-r doesn't set flags, yet")
  (setf (reg-a cpu)
        (logior (reg-a cpu)
                (funcall (find-8-bit-register (opcode-z opcode)) cpu))))

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
  (let ((jump-address (fetch-byte-from-ram cpu)))
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
    (push-from register)))

(define-instruction pop-register #x1 (cpu opcode)
  (let ((register (find-16-bit-register% (opcode-p opcode))))
    (pop-to register)))

(define-instruction ret-cc #x1 (cpu opcode)
  (let ((condition (find-condition (opcode-y))))
    (when (funcall condition cpu)
      (ret cpu))))

(define-instruction pop-cc #x1 (cpu opcode)
  (let ((condition (find-condition (opcode-y) cpu)))
    (when (funcall condition cpu)
      (pop cpu))))

(define-instruction call-cc-nn #x3 (cpu opcode)
  (let ((condition (find-condition (opcode-y cpu))))
    (when (funcall condition cpu)
      (call cpu (fetch-word cpu)))))

(define-instruction call-nn #x3 (cpu opcode)
  (call cpu (fetch-word cpu)))

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

(define-instruction ed-prefix #x2 (cpu opcode)
  )

(define-instruction cb-prefix #x2 (cpu opcode)
  )

(define-instruction dd-prefix #x2 (cpu opcode)
  )

(define-instruction fd-prefix #x2 (cpu opcode)
  )
