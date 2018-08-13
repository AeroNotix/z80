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
  (loop
     with y = (logxor x (rshift x 1))
     for i from 2 to 7
     do (setq y (logxor y (rshift y i)))
     return (logand y 1)))

(defun 8-bit-parity (x)
  (loop
     with parity = 0
     for i from 0 to 7
     do
       (setq parity (logxor parity (logand x 1)))
       (setq x (ash x -1))
     finally (return (logxor parity 1))))
