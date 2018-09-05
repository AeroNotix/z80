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
  (list 'reg-bc% 'reg-de% 'reg-hl% 'reg-af%))

(defparameter *16-bit-registers%-indexed-ix*
  (list 'reg-bc% 'reg-de% 'reg-ix% 'reg-af%))

(defparameter *16-bit-registers%-indexed-iy*
  (list 'reg-bc% 'reg-de% 'reg-iy% 'reg-af%))

(defparameter *8-bit-registers* *8-bit-registers-base*)
(defparameter *16-bit-registers* *16-bit-registers-base*)
(defparameter *16-bit-registers%* *16-bit-registers%-base*)

(defparameter %base-addressing-mode%
  (list '*8-bit-registers-base* '*16-bit-registers-base* '*16-bit-registers%-base* ''reg-hl ''mem-hl))
(defparameter %ix-addressing-mode%
  (list '*8-bit-registers-indexed-ix* '*16-bit-registers-indexed-ix* '*16-bit-registers%-indexed-ix* ''reg-ix ''ix-with-offset))
(defparameter %iy-addressing-mode%
  (list '*8-bit-registers-indexed-iy* '*16-bit-registers-indexed-iy* '*16-bit-registers%-indexed-iy* ''reg-iy ''iy-with-offset))

(defparameter *address-register* 'reg-hl)

(defmethod address-register ((cpu cpu))
  (funcall *address-register* cpu))

(defmethod (setf address-register) (value (cpu cpu))
  (funcall (setf-of *address-register*) value cpu))

(defparameter *address-register-memory* 'mem-hl)

(defmethod address-register-memory ((cpu cpu))
  (funcall *address-register-memory* cpu))

(defmethod (setf address-register-memory) (value (cpu cpu))
  (setf (elt (ram cpu) (address-register cpu)) value))

(defmacro with-addressing-mode (mode &body body)
  (let ((mode (case mode
                (:base %base-addressing-mode%)
                (:ix %ix-addressing-mode%)
                (:iy %iy-addressing-mode%))))
    `(destructuring-bind (*8-bit-registers*
                          *16-bit-registers*
                          *16-bit-registers%*
                          *address-register*
                          *address-register-memory*) (list ,@mode)
       ,@body)))
