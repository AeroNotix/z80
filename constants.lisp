(in-package :z80)

(defparameter logging-enabled nil)

(defparameter s-flag-pos 7)
(defparameter z-flag-pos 6)
(defparameter y-flag-pos 5)
(defparameter h-flag-pos 4)
(defparameter x-flag-pos 3)
(defparameter p-flag-pos 2)
(defparameter n-flag-pos 1)
(defparameter c-flag-pos 0)

(defparameter s-mask (ash 1 s-flag-pos))
(defparameter z-mask (ash 1 z-flag-pos))
(defparameter y-mask (ash 1 y-flag-pos))
(defparameter h-mask (ash 1 h-flag-pos))
(defparameter x-mask (ash 1 x-flag-pos))
(defparameter p-mask (ash 1 p-flag-pos))
(defparameter n-mask (ash 1 n-flag-pos))
(defparameter c-mask (ash 1 c-flag-pos))

(defparameter *8-bit-registers-base*
  (list 'reg-b 'reg-c 'reg-d 'reg-e 'reg-h 'reg-l 'mem-hl 'reg-a))

(defparameter *8-bit-registers-indexed-ix*
  (list 'reg-b 'reg-c 'reg-d 'reg-e 'reg-ixh 'reg-ixl 'ix-with-offset 'reg-a))

(defparameter *8-bit-registers-indexed-iy*
  (list 'reg-b 'reg-c 'reg-d 'reg-e 'reg-iyh 'reg-iyl 'iy-with-offset 'reg-a))

(defparameter *16-bit-registers-base*
  (list 'reg-bc 'reg-de 'reg-hl 'reg-sp))

(defparameter *16-bit-registers-indexed-ix*
  (list 'reg-bc 'reg-de 'reg-ix 'reg-sp))

(defparameter *16-bit-registers-indexed-iy*
  (list 'reg-bc 'reg-de 'reg-iy 'reg-sp))

(defparameter *16-bit-registers%-base*
  (list 'reg-bc 'reg-de 'reg-hl 'reg-af))

(defparameter *16-bit-registers%-indexed-ix*
  (list 'reg-bc 'reg-de 'reg-ix 'reg-af))

(defparameter *16-bit-registers%-indexed-iy*
  (list 'reg-bc 'reg-de 'reg-iy 'reg-af))

(defparameter *8-bit-registers* *8-bit-registers-base*)
(defparameter *16-bit-registers* *16-bit-registers-base*)
(defparameter *16-bit-registers%* *16-bit-registers%-base*)

;; put this somewhere better, external library?
(defconstant single-float-positive-infinity
  #+sbcl sb-ext:single-float-positive-infinity
  #-sbcl(progn
      (warn "Non SBCL platforms need a better representation of
      infinity! This will terminate the CPU after 3.4028235f83 cycles!")
      sbcl most-positive-single-float))
