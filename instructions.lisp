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

(defun ld (c y z)
  (funcall (setf-of y) (funcall z c) c))

(defun inc (c y &key (amount 1))
  (let* ((result (+ amount (funcall y c)))
         (c-flag (if (> result 255) c-flag 0))
         (z-flag (if (eq result 0) z-flag 0))
         (p-flag p-flag) ;; not correct
         (s-flag (if (< 127 result 256) s-flag 0))
         (n-flag 0)
         (h-flag 0) ;; not-correct
         (next-flags (logior c-flag z-flag p-flag s-flag n-flag h-flag)))
    (format t "~B~%" next-flags)
    (setf (reg-f c) next-flags)
    (funcall (setf-of y) result c)))

(defun dec (c y)
  (let ((next-flags (logior z-flag))
        (res (funcall (decf-of y) c)))
    (setf (reg-f next-flags) c)))

(defun find-8-bit-register (i)
  (elt (list 'reg-b 'reg-c 'reg-d 'reg-e 'reg-h 'reg-l 'mem-hl 'reg-a) i))

(defun find-16-bit-register (i)
  (elt (list 'reg-bc 'reg-de 'reg-hl 'reg-sp) i))

(define-instruction nop #x1 (cpu opcode))

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

(define-instruction ld-indirect-bc-a #x1 (cpu opcode)
  (setf (mem-bc cpu) (reg-a cpu)))

(define-instruction ld-a-indirect-bc #x1 (cpu opcode)
  (setf (reg-a cpu) (mem-bc cpu)))

(define-instruction inc-rr #x1 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (inc cpu p)))

(define-instruction inc-r #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (inc cpu y)))

(define-instruction dec-r #x1 (cpu opcode)
  (let ((y (find-8-bit-register (opcode-y opcode))))
    (dec cpu y)))

(define-instruction dec-rr #x1 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (dec cpu p)))

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

(define-instruction add-hl-rr #x1 (cpu opcode)
  (let ((p (find-16-bit-register (opcode-p opcode))))
    (inc cpu 'reg-hl :amount (funcall p cpu))))

(define-instruction add-r #x1 (cpu opcode)
  (let ((z (find-8-bit-register (opcode-z opcode))))
    (inc cpu 'reg-a :amount (funcall z cpu))))

(define-instruction add-n #x1 (cpu opcode)
  (let ((value (fetch-byte-from-ram cpu)))
    (inc cpu 'reg-a :amount value)))

(define-instruction djnz-n #x2 (cpu opcode)
  (let ((jump-offset (unsigned->signed (fetch-byte-from-ram cpu))))
    (

;; (define-instruction ld-b-n #x06 #x2 (cpu) (setf (reg-b cpu) (fetch-byte-from-ram cpu)))
;; (define-instruction ld-d-n #x16 #x2 (cpu) (setf (reg-d cpu) (fetch-byte-from-ram cpu)))
;; (define-instruction ld-h-n #x26 #x2 (cpu) (setf (reg-h cpu) (fetch-byte-from-ram cpu)))
;; (define-instruction ld-hl-n #x36 #x2 (cpu) (setf (reg-h cpu) (setf (mem-hl cpu)
;;                                                                    (fetch-byte-from-ram cpu))))


;; (define-instruction ld-a-bc #x0A #x2 (cpu) (setf (reg-a cpu) (mem-bc cpu)))
;; (define-instruction ld-a-de #x1A #x2 (cpu) (setf (reg-a cpu) (mem-de cpu)))
;; (define-instruction ld-hl-nn #x2A #x3 (cpu)
;;   (setf (reg-hl cpu) (elt (ram cpu) (fetch-word cpu))))
;; (define-instruction ld-a-nn #x3A #x2 (cpu)
;;   (setf (reg-a cpu) (elt (ram cpu) (fetch-word cpu))))

;; (define-instruction ld-bc-nn #x01 #x3 (cpu) (setf (reg-bc cpu) (fetch-word cpu)))
;; (define-instruction ld-de-nn #x11 #x3 (cpu) (setf (reg-de cpu) (fetch-word cpu)))
;; (define-instruction ld-hl-nn #x21 #x3 (cpu) (setf (reg-hl cpu) (fetch-word cpu)))
;; ;; implement (reg-sp cpu)
;; (define-instruction ld-sp-nn #x31 #x3 (cpu) (setf (reg-sp cpu) (fetch-word cpu)))

;; (define-instruction ld-c-n #x0E #x2 (cpu) (setf (reg-c cpu) (fetch-byte-from-ram cpu)))
;; (define-instruction ld-e-n #x1E #x2 (cpu) (setf (reg-e cpu) (fetch-byte-from-ram cpu)))
;; (define-instruction ld-l-n #x2E #x2 (cpu) (setf (reg-l cpu) (fetch-byte-from-ram cpu)))
;; (define-instruction ld-a-n #x3E #x2 (cpu) (setf (reg-a cpu) (fetch-byte-from-ram cpu)))

;; ;; LD-x{,y} instructions
;; (define-instruction ld-b-b #x40 #x1 (cpu) (setf (reg-b cpu) (reg-b cpu)))
;; (define-instruction ld-b-c #x41 #x1 (cpu) (setf (reg-b cpu) (reg-c cpu)))
;; (define-instruction ld-b-d #x42 #x1 (cpu) (setf (reg-b cpu) (reg-d cpu)))
;; (define-instruction ld-b-e #x43 #x1 (cpu) (setf (reg-b cpu) (reg-e cpu)))
;; (define-instruction ld-b-h #x44 #x1 (cpu) (setf (reg-b cpu) (reg-h cpu)))
;; (define-instruction ld-b-l #x45 #x1 (cpu) (setf (reg-b cpu) (reg-l cpu)))
;; (define-instruction ld-b-hl #x46 #x1 (cpu) (setf (reg-b cpu) (mem-hl cpu)))
;; (define-instruction ld-b-a #x47 #x1 (cpu) (setf (reg-b cpu) (reg-a cpu)))
;; (define-instruction ld-c-b #x48 #x1 (cpu) (setf (reg-c cpu) (reg-b cpu)))
;; (define-instruction ld-c-c #x49 #x1 (cpu) (setf (reg-c cpu) (reg-c cpu)))
;; (define-instruction ld-c-d #x4A #x1 (cpu) (setf (reg-c cpu) (reg-d cpu)))
;; (define-instruction ld-c-e #x4B #x1 (cpu) (setf (reg-c cpu) (reg-e cpu)))
;; (define-instruction ld-c-h #x4C #x1 (cpu) (setf (reg-c cpu) (reg-h cpu)))
;; (define-instruction ld-c-l #x4D #x1 (cpu) (setf (reg-c cpu) (reg-l cpu)))
;; (define-instruction ld-c-hl #x4E #x1 (cpu) (setf (reg-c cpu) (mem-hl cpu)))
;; (define-instruction ld-c-a #x4F #x1 (cpu) (setf (reg-c cpu) (reg-a cpu)))

;; (define-instruction ld-d-b #x50 #x1 (cpu) (setf (reg-d cpu) (reg-b cpu)))
;; (define-instruction ld-d-c #x51 #x1 (cpu) (setf (reg-d cpu) (reg-c cpu)))
;; (define-instruction ld-d-d #x52 #x1 (cpu) (setf (reg-d cpu) (reg-d cpu)))
;; (define-instruction ld-d-e #x53 #x1 (cpu) (setf (reg-d cpu) (reg-e cpu)))
;; (define-instruction ld-d-h #x54 #x1 (cpu) (setf (reg-d cpu) (reg-h cpu)))
;; (define-instruction ld-d-l #x55 #x1 (cpu) (setf (reg-d cpu) (reg-l cpu)))
;; (define-instruction ld-d-hl #x56 #x1 (cpu) (setf (reg-d cpu) (mem-hl cpu)))
;; (define-instruction ld-d-a #x57 #x1 (cpu) (setf (reg-d cpu) (reg-a cpu)))
;; (define-instruction ld-e-b #x58 #x1 (cpu) (setf (reg-e cpu) (reg-b cpu)))
;; (define-instruction ld-e-c #x59 #x1 (cpu) (setf (reg-e cpu) (reg-c cpu)))
;; (define-instruction ld-e-d #x5A #x1 (cpu) (setf (reg-e cpu) (reg-d cpu)))
;; (define-instruction ld-e-e #x5B #x1 (cpu) (setf (reg-e cpu) (reg-e cpu)))
;; (define-instruction ld-e-h #x5C #x1 (cpu) (setf (reg-e cpu) (reg-h cpu)))
;; (define-instruction ld-e-l #x5D #x1 (cpu) (setf (reg-e cpu) (reg-l cpu)))
;; (define-instruction ld-e-hl #x5E #x1 (cpu) (setf (reg-e cpu) (mem-hl cpu)))
;; (define-instruction ld-e-a #x5F #x1 (cpu) (setf (reg-e cpu) (reg-a cpu)))

;; (define-instruction ld-h-b #x60 #x1 (cpu) (setf (reg-h cpu) (reg-b cpu)))
;; (define-instruction ld-h-c #x61 #x1 (cpu) (setf (reg-h cpu) (reg-c cpu)))
;; (define-instruction ld-h-d #x62 #x1 (cpu) (setf (reg-h cpu) (reg-d cpu)))
;; (define-instruction ld-h-e #x63 #x1 (cpu) (setf (reg-h cpu) (reg-e cpu)))
;; (define-instruction ld-h-h #x64 #x1 (cpu) (setf (reg-h cpu) (reg-h cpu)))
;; (define-instruction ld-h-l #x65 #x1 (cpu) (setf (reg-h cpu) (reg-l cpu)))
;; (define-instruction ld-h-hl #x66 #x1 (cpu) (setf (reg-h cpu) (mem-hl cpu)))
;; (define-instruction ld-h-a #x67 #x1 (cpu) (setf (reg-h cpu) (reg-a cpu)))
;; (define-instruction ld-l-b #x68 #x1 (cpu) (setf (reg-l cpu) (reg-b cpu)))
;; (define-instruction ld-l-c #x69 #x1 (cpu) (setf (reg-l cpu) (reg-c cpu)))
;; (define-instruction ld-l-d #x6A #x1 (cpu) (setf (reg-l cpu) (reg-d cpu)))
;; (define-instruction ld-l-e #x6B #x1 (cpu) (setf (reg-l cpu) (reg-e cpu)))
;; (define-instruction ld-l-h #x6C #x1 (cpu) (setf (reg-l cpu) (reg-h cpu)))
;; (define-instruction ld-l-l #x6D #x1 (cpu) (setf (reg-l cpu) (reg-l cpu)))
;; (define-instruction ld-l-hl #x6E #x1 (cpu) (setf (reg-l cpu) (mem-hl cpu)))
;; (define-instruction ld-l-a #x6F #x1 (cpu) (setf (reg-l cpu) (reg-a cpu)))

;; (define-instruction ld-hl-b #x70 #x1 (cpu) (setf (mem-hl cpu) (reg-b cpu)))
;; (define-instruction ld-hl-c #x71 #x1 (cpu) (setf (mem-hl cpu) (reg-c cpu)))
;; (define-instruction ld-hl-d #x72 #x1 (cpu) (setf (mem-hl cpu) (reg-d cpu)))
;; (define-instruction ld-hl-e #x73 #x1 (cpu) (setf (mem-hl cpu) (reg-e cpu)))
;; (define-instruction ld-hl-h #x74 #x1 (cpu) (setf (mem-hl cpu) (reg-h cpu)))
;; (define-instruction ld-hl-l #x75 #x1 (cpu) (setf (mem-hl cpu) (reg-l cpu)))
;; (define-instruction halt   #x76 #x1 (cpu) (halt cpu))
;; (define-instruction ld-hl-a #x77 #x1 (cpu) (setf (mem-hl cpu) (reg-a cpu)))
;; (define-instruction ld-a-b #x78 #x1 (cpu) (setf (reg-a cpu) (reg-b cpu)))
;; (define-instruction ld-a-c #x79 #x1 (cpu) (setf (reg-a cpu) (reg-c cpu)))
;; (define-instruction ld-a-d #x7A #x1 (cpu) (setf (reg-a cpu) (reg-d cpu)))
;; (define-instruction ld-a-e #x7B #x1 (cpu) (setf (reg-a cpu) (reg-e cpu)))
;; (define-instruction ld-a-h #x7C #x1 (cpu) (setf (reg-a cpu) (reg-h cpu)))
;; (define-instruction ld-a-l #x7D #x1 (cpu) (setf (reg-a cpu) (reg-l cpu)))
;; (define-instruction ld-a-hl #x7E #x1 (cpu) (setf (reg-a cpu) (mem-hl cpu)))
;; (define-instruction ld-a-a #x7F #x1 (cpu) (setf (reg-a cpu) (reg-a cpu)))

;; ;; 16-bit inc-XX's -- probably doesn't work
;; (define-instruction inc-bc #x03 #x1 (cpu) (setf (reg-bc cpu) (1+ reg-bc cpu)))

;; ;; 8-bit inc-X's
;; (define-instruction inc-b  #x04 #x1 (cpu) (setf (reg-b cpu) (1+ (reg-b cpu))))
;; (define-instruction inc-d  #x14 #x1 (cpu) (setf (reg-d cpu) (1+ (reg-d cpu))))
;; (define-instruction inc-h  #x24 #x1 (cpu) (setf (reg-h cpu) (1+ (reg-h cpu))))
;; (define-instruction inc-c  #x0C #x1 (cpu) (setf (reg-c cpu) (1+ (reg-c cpu))))
;; (define-instruction inc-e  #x1C #x1 (cpu) (setf (reg-e cpu) (1+ (reg-e cpu))))
;; (define-instruction inc-l  #x2C #x1 (cpu) (setf (reg-l cpu) (1+ (reg-l cpu))))
;; (define-instruction inc-a  #x3C #x1 (cpu) (setf (reg-a cpu) (1+ (reg-a cpu))))

;; ;; 16-bit dec-X's
;; (define-instruction dec-bc #x0B #x1 (cpu) (setf (reg-bc cpu) (1- (reg-bc cpu))))
;; (define-instruction dec-de #x1B #x1 (cpu) (setf (reg-de cpu) (1- (reg-de cpu))))
;; (define-instruction dec-hl #x2B #x1 (cpu) (setf (reg-hl cpu) (1- (reg-hl cpu))))
;; (define-instruction dec-sp #x3B #x1 (cpu) (setf (reg-sp cpu) (1- (reg-sp cpu))))

;; ;; 8-bit dec-X's
;; (define-instruction dec-b #x05 #x1 (cpu) (setf (reg-b cpu) (1- (reg-b cpu))))
;; (define-instruction dec-d #x15 #x1 (cpu) (setf (reg-d cpu) (1- (reg-d cpu))))
;; (define-instruction dec-h #x21 #x1 (cpu) (setf (reg-h cpu) (1- (reg-h cpu))))
;; (define-instruction dec-hl #x35 #x1 (cpu) (setf (mem-hl cpu) (1- (mem-hl cpu))))
;; (define-instruction dec-c #x0D #x1 (cpu) (setf (reg-c cpu) (1- (reg-c cpu))))
;; (define-instruction dec-e #x1D #x1 (cpu) (setf (reg-e cpu) (1- (reg-e cpu))))
;; (define-instruction dec-l #x2D #x1 (cpu) (setf (reg-l cpu) (1- (reg-l cpu))))
;; (define-instruction dec-a #x3D #x1 (cpu) (setf (reg-a cpu) (1- (reg-a cpu))))

;; ;; inc-hl needs ram access
;; (define-instruction inc-hl #x34 #x1 (cpu) (setf (reg-hl cpu) (1+ (reg-hl cpu))))

;; ;; or instructions
;; (define-instruction or-c #xB1 #x1 (cpu)
;;   (let ((result (logior (reg-a cpu)
;;                         (reg-c cpu))))
;;     (setf (reg-a cpu) result
;;           (reg-f cpu) (if (eq result 0)
;;                           64
;;                           0))))
;; ;; xor instructions
;; (define-instruction xor-b #xA8 #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-b cpu))))
;; (define-instruction xor-c #xA9 #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-c cpu))))
;; (define-instruction xor-d #xAA #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-d cpu))))
;; (define-instruction xor-e #xAB #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-e cpu))))
;; (define-instruction xor-h #xAC #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-h cpu))))
;; (define-instruction xor-l #xAD #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-l cpu))))
;; (define-instruction xor-z #xAE #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (mem-hl cpu))))
;; (define-instruction xor-a #xAF #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-a cpu))))
;; (define-instruction xor-n #xEE #x2 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (fetch-byte-from-ram cpu))))

;; ;; Relative jumps
;; (define-instruction jr-nz-n #x20 #x2 (cpu)
;;   (when (zero-flag-set? cpu)
;;     (incf (pc cpu) (unsigned->signed (fetch-byte-from-ram cpu) 1))))

;; ;; Absolute jumps
;; (define-instruction jp-nn #xC3 #x3 (cpu)
;;   (setf (pc cpu) (fetch-word cpu)))

;; (define-instruction jp-hl #xE9 #x1 (cpu)
;;   (setf (pc cpu) (reg-hl cpu)))

;; (define-instruction jp-nc-nn #xD2 #x3 (cpu)
;;   (when (not (carry-flag-set? cpu))
;;     (funcall (microcode jp-nn) cpu)))

;; ;; I/O instructions
;; (define-instruction in-a-nn #xDB #x2 (cpu)
;;   (setf (reg-a cpu) (read-port cpu (fetch-byte-from-ram cpu))))

;; (define-instruction out-n-a #xD3 #x2 (cpu)
;;   (write-port cpu (fetch-byte-from-ram cpu) (reg-a cpu)))

;; (define-instruction di #xF3 #x1 (cpu)
;;   (setf (interrupts cpu) :disabled))
