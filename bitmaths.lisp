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
