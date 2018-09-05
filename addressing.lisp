(in-package :z80)


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

(defparameter %base-addressing-mode%
  (list '*8-bit-registers-base* '*16-bit-registers-base* '*16-bit-registers%-base*))
(defparameter %ix-addressing-mode%
  (list '*8-bit-registers-indexed-ix* '*16-bit-registers-indexed-ix* '*16-bit-registers%-indexed-ix*))
(defparameter %iy-addressing-mode%
  (list '*8-bit-registers-indexed-iy* '*16-bit-registers-indexed-iy* '*16-bit-registers%-indexed-iy*))

(defmacro with-addressing-mode (mode &body body)
  (let ((mode (case mode
                (:base %base-addressing-mode%)
                (:ix %ix-addressing-mode%)
                (:iy %iy-addressing-mode%))))
    `(destructuring-bind (*8-bit-registers* *16-bit-registers* *16-bit-registers%*) (list ,@mode)
       ,@body)))
