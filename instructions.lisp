(in-package :z80)


(defun signed->unsigned/bits (n bits)
  (let ((negative-offset (expt 2 bits)))
    (if (< n 0)
	    (the integer (+ n negative-offset))
	    n)))

(defun signed->unsigned (n bytes)
  (signed->unsigned/bits n (* 8 bytes)))

(defun unsigned->signed/bits (n bits)
  (let* ((negative-offset (expt 2 bits))
	     (max (- (/ negative-offset 2) 1)))
    (if (> n max)
	    (- n negative-offset)
	    n)))

(defun unsigned->signed (n bytes)
  (unsigned->signed/bits n (* 8 bytes)))

(define-instruction nop     #x00 #x1 (cpu))

;; Load immediate instructions
(define-instruction ld-b-n #x06 #x2 (cpu) (setf (reg-b cpu) (fetch-byte-from-ram cpu)))
(define-instruction ld-d-n #x16 #x2 (cpu) (setf (reg-d cpu) (fetch-byte-from-ram cpu)))
(define-instruction ld-h-n #x26 #x2 (cpu) (setf (reg-h cpu) (fetch-byte-from-ram cpu)))
(define-instruction ld-hl-n #x36 #x2 (cpu) (setf (reg-h cpu) (setf (mem-hl cpu)
                                                                     (fetch-byte-from-ram cpu))))
(define-instruction ld-a-bc #x0A #x2 (cpu) (setf (reg-a cpu) (mem-bc cpu)))
(define-instruction ld-a-de #x1A #x2 (cpu) (setf (reg-a cpu) (mem-de cpu)))
(define-instruction ld-hl-nn #x2A #x3 (cpu)
  (setf (reg-hl cpu) (elt (ram cpu) (fetch-word cpu))))
(define-instruction ld-a-nn #x3A #x2 (cpu)
  (setf (reg-a cpu) (elt (ram cpu) (fetch-word cpu))))
(define-instruction ld-bc-nn #x01 #x3 (cpu) (setf (reg-bc cpu) (fetch-word cpu)))

(define-instruction ld-c-n #x0E #x2 (cpu) (setf (reg-c cpu) (fetch-byte-from-ram cpu)))
(define-instruction ld-e-n #x1E #x2 (cpu) (setf (reg-e cpu) (fetch-byte-from-ram cpu)))
(define-instruction ld-l-n #x2E #x2 (cpu) (setf (reg-l cpu) (fetch-byte-from-ram cpu)))
(define-instruction ld-a-n #x3E #x2 (cpu) (setf (reg-a cpu) (fetch-byte-from-ram cpu)))

;; LD-x{,y} instructions
(define-instruction ld-b-b  #x40 #x1 (cpu) (setf (reg-b cpu) (reg-b cpu)))
(define-instruction ld-b-c  #x41 #x1 (cpu) (setf (reg-b cpu) (reg-c cpu)))
(define-instruction ld-b-d  #x42 #x1 (cpu) (setf (reg-b cpu) (reg-d cpu)))
(define-instruction ld-b-e  #x43 #x1 (cpu) (setf (reg-b cpu) (reg-e cpu)))
(define-instruction ld-b-h  #x44 #x1 (cpu) (setf (reg-b cpu) (reg-h cpu)))
(define-instruction ld-b-l  #x45 #x1 (cpu) (setf (reg-b cpu) (reg-l cpu)))
(define-instruction ld-b-hl #x46 #x1 (cpu) (setf (reg-b cpu) (mem-hl cpu)))
(define-instruction ld-b-a  #x47 #x1 (cpu) (setf (reg-b cpu) (reg-a cpu)))
(define-instruction ld-c-b  #x48 #x1 (cpu) (setf (reg-c cpu) (reg-b cpu)))
(define-instruction ld-c-c  #x49 #x1 (cpu) (setf (reg-c cpu) (reg-c cpu)))
(define-instruction ld-c-d  #x4A #x1 (cpu) (setf (reg-c cpu) (reg-d cpu)))
(define-instruction ld-c-e  #x4B #x1 (cpu) (setf (reg-c cpu) (reg-e cpu)))
(define-instruction ld-c-h  #x4C #x1 (cpu) (setf (reg-c cpu) (reg-h cpu)))
(define-instruction ld-c-l  #x4D #x1 (cpu) (setf (reg-c cpu) (reg-l cpu)))
(define-instruction ld-c-hl #x4E #x1 (cpu) (setf (reg-c cpu) (mem-hl cpu)))
(define-instruction ld-c-a  #x4F #x1 (cpu) (setf (reg-c cpu) (reg-a cpu)))

(define-instruction ld-d-b  #x50 #x1 (cpu) (setf (reg-d cpu) (reg-b cpu)))
(define-instruction ld-d-c  #x51 #x1 (cpu) (setf (reg-d cpu) (reg-c cpu)))
(define-instruction ld-d-d  #x52 #x1 (cpu) (setf (reg-d cpu) (reg-d cpu)))
(define-instruction ld-d-e  #x53 #x1 (cpu) (setf (reg-d cpu) (reg-e cpu)))
(define-instruction ld-d-h  #x54 #x1 (cpu) (setf (reg-d cpu) (reg-h cpu)))
(define-instruction ld-d-l  #x55 #x1 (cpu) (setf (reg-d cpu) (reg-l cpu)))
(define-instruction ld-d-hl #x56 #x1 (cpu) (setf (reg-d cpu) (mem-hl cpu)))
(define-instruction ld-d-a  #x57 #x1 (cpu) (setf (reg-d cpu) (reg-a cpu)))
(define-instruction ld-e-b  #x58 #x1 (cpu) (setf (reg-e cpu) (reg-b cpu)))
(define-instruction ld-e-c  #x59 #x1 (cpu) (setf (reg-e cpu) (reg-c cpu)))
(define-instruction ld-e-d  #x5A #x1 (cpu) (setf (reg-e cpu) (reg-d cpu)))
(define-instruction ld-e-e  #x5B #x1 (cpu) (setf (reg-e cpu) (reg-e cpu)))
(define-instruction ld-e-h  #x5C #x1 (cpu) (setf (reg-e cpu) (reg-h cpu)))
(define-instruction ld-e-l  #x5D #x1 (cpu) (setf (reg-e cpu) (reg-l cpu)))
(define-instruction ld-e-hl #x5E #x1 (cpu) (setf (reg-e cpu) (mem-hl cpu)))
(define-instruction ld-e-a  #x5F #x1 (cpu) (setf (reg-e cpu) (reg-a cpu)))

(define-instruction ld-h-b  #x60 #x1 (cpu) (setf (reg-h cpu) (reg-b cpu)))
(define-instruction ld-h-c  #x61 #x1 (cpu) (setf (reg-h cpu) (reg-c cpu)))
(define-instruction ld-h-d  #x62 #x1 (cpu) (setf (reg-h cpu) (reg-d cpu)))
(define-instruction ld-h-e  #x63 #x1 (cpu) (setf (reg-h cpu) (reg-e cpu)))
(define-instruction ld-h-h  #x64 #x1 (cpu) (setf (reg-h cpu) (reg-h cpu)))
(define-instruction ld-h-l  #x65 #x1 (cpu) (setf (reg-h cpu) (reg-l cpu)))
(define-instruction ld-h-hl #x66 #x1 (cpu) (setf (reg-h cpu) (mem-hl cpu)))
(define-instruction ld-h-a  #x67 #x1 (cpu) (setf (reg-h cpu) (reg-a cpu)))
(define-instruction ld-l-b  #x68 #x1 (cpu) (setf (reg-l cpu) (reg-b cpu)))
(define-instruction ld-l-c  #x69 #x1 (cpu) (setf (reg-l cpu) (reg-c cpu)))
(define-instruction ld-l-d  #x6A #x1 (cpu) (setf (reg-l cpu) (reg-d cpu)))
(define-instruction ld-l-e  #x6B #x1 (cpu) (setf (reg-l cpu) (reg-e cpu)))
(define-instruction ld-l-h  #x6C #x1 (cpu) (setf (reg-l cpu) (reg-h cpu)))
(define-instruction ld-l-l  #x6D #x1 (cpu) (setf (reg-l cpu) (reg-l cpu)))
(define-instruction ld-l-hl #x6E #x1 (cpu) (setf (reg-l cpu) (mem-hl cpu)))
(define-instruction ld-l-a  #x6F #x1 (cpu) (setf (reg-l cpu) (reg-a cpu)))

(define-instruction ld-hl-b #x70 #x1 (cpu) (setf (mem-hl cpu) (reg-b cpu)))
(define-instruction ld-hl-c #x71 #x1 (cpu) (setf (mem-hl cpu) (reg-c cpu)))
(define-instruction ld-hl-d #x72 #x1 (cpu) (setf (mem-hl cpu) (reg-d cpu)))
(define-instruction ld-hl-e #x73 #x1 (cpu) (setf (mem-hl cpu) (reg-e cpu)))
(define-instruction ld-hl-h #x74 #x1 (cpu) (setf (mem-hl cpu) (reg-h cpu)))
(define-instruction ld-hl-l #x75 #x1 (cpu) (setf (mem-hl cpu) (reg-l cpu)))
(define-instruction halt    #x76 #x1 (cpu) (halt cpu))
(define-instruction ld-hl-a #x77 #x1 (cpu) (setf (mem-hl cpu) (reg-a cpu)))
(define-instruction ld-a-b  #x78 #x1 (cpu) (setf (reg-a cpu) (reg-b cpu)))
(define-instruction ld-a-c  #x79 #x1 (cpu) (setf (reg-a cpu) (reg-c cpu)))
(define-instruction ld-a-d  #x7A #x1 (cpu) (setf (reg-a cpu) (reg-d cpu)))
(define-instruction ld-a-e  #x7B #x1 (cpu) (setf (reg-a cpu) (reg-e cpu)))
(define-instruction ld-a-h  #x7C #x1 (cpu) (setf (reg-a cpu) (reg-h cpu)))
(define-instruction ld-a-l  #x7D #x1 (cpu) (setf (reg-a cpu) (reg-l cpu)))
(define-instruction ld-a-hl #x7E #x1 (cpu) (setf (reg-a cpu) (mem-hl cpu)))
(define-instruction ld-a-a  #x7F #x1 (cpu) (setf (reg-a cpu) (reg-a cpu)))

;; 16-bit inc-XX's -- probably doesn't work
(define-instruction inc-bc  #x03 #x1 (cpu) (setf (reg-bc cpu) (1+ reg-bc cpu)))

;; 8-bit inc-X's
(define-instruction inc-b   #x04 #x1 (cpu) (setf (reg-b cpu) (1+ (reg-b cpu))))
(define-instruction inc-d   #x14 #x1 (cpu) (setf (reg-d cpu) (1+ (reg-d cpu))))
(define-instruction inc-h   #x24 #x1 (cpu) (setf (reg-h cpu) (1+ (reg-h cpu))))
(define-instruction inc-c   #x0C #x1 (cpu) (setf (reg-c cpu) (1+ (reg-c cpu))))
(define-instruction inc-e   #x1C #x1 (cpu) (setf (reg-e cpu) (1+ (reg-e cpu))))
(define-instruction inc-l   #x2C #x1 (cpu) (setf (reg-l cpu) (1+ (reg-l cpu))))
(define-instruction inc-a   #x3C #x1 (cpu) (setf (reg-a cpu) (1+ (reg-a cpu))))

;; 16-bit dec-X's
(define-instruction dec-bc #x0B #x1 (cpu) (setf (reg-bc cpu) (1- (reg-bc cpu))))

;; 16-bit inc-X's
(define-instruction inc-bc #x03 #x1 (cpu) (setf (reg-bc cpu) (1+ (reg-bc cpu))))

;; inc-hl needs ram access
(define-instruction inc-hl  #x34 #x1 (cpu) (setf (reg-hl cpu) (1+ (reg-hl cpu))))

;; or instructions
(define-instruction or-c #xB1 #x1 (cpu)
  (let ((result (logior (reg-a cpu)
                        (reg-c cpu))))
    (setf (reg-a cpu) result
          (reg-f cpu) (if (eq result 0)
                          64
                          0))))
;; xor instructions
(define-instruction xor-b #xA8 #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-b cpu))))
(define-instruction xor-c #xA9 #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-c cpu))))
(define-instruction xor-d #xAA #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-d cpu))))
(define-instruction xor-e #xAB #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-e cpu))))
(define-instruction xor-h #xAC #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-h cpu))))
(define-instruction xor-l #xAD #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-l cpu))))
(define-instruction xor-z #xAE #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (mem-hl cpu))))
(define-instruction xor-a #xAF #x1 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (reg-a cpu))))
(define-instruction xor-n #xEE #x2 (cpu) (setf (reg-a cpu) (logxor (reg-a cpu) (fetch-byte-from-ram cpu))))

;; Relative jumps
(define-instruction jr-nz-n #x20 #x2 (cpu)
  (when (zero-flag-set? cpu)
    (incf (pc cpu) (unsigned->signed (fetch-byte-from-ram cpu) 1))))

;; Absolute jumps
(define-instruction jp-nn #xC3 #x3 (cpu)
  (setf (pc cpu) (fetch-word cpu)))

(define-instruction jp-hl #xE9 #x1 (cpu)
  (setf (pc cpu) (reg-hl cpu)))

(define-instruction jp-nc-nn #xD2 #x3 (cpu)
  (when (not (carry-flag-set? cpu))
    (funcall (microcode jp-nn) cpu)))

;; I/O instructions
(define-instruction in-a-nn #xDB #x2 (cpu)
  (setf (reg-a cpu) (read-port cpu (fetch-byte-from-ram cpu))))
