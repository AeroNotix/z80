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

(defun rshift (integer count)
  (ash integer (- 0 count)))

(defun 8-bit-parity (x)
  (if (evenp (logcount x)) 1 0))

(defun ~ (x)
  (1- (- 0 x)))

(defun twos-complement (x &optional (bit-count 8))
  (let ((mask (expt 2 (1- bit-count))))
    (+ (- 0 (logand x mask) (logand x (~ mask))))))

(defun reset-bit-at (integer position)
  (logand integer (lognot (ash 1 position))))

(defun set-bit-at (integer position)
  (logior integer (ash 1 position)))
