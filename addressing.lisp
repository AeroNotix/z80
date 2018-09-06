(in-package :z80)

(defparameter *addressing-mode* :base)

(defmethod reg-hl+ ((cpu cpu))
  (case *addressing-mode*
    (:base (reg-hl cpu))
    (:ix (reg-ix cpu))
    (:iy (reg-iy cpu))))

(defmethod (setf reg-hl+) (value (cpu cpu))
  (case *addressing-mode*
    (:base (setf (reg-hl cpu) value))
    (:ix (setf (reg-ix cpu) value))
    (:iy (setf (reg-iy cpu) value))))

(defmethod index-register ((cpu cpu))
  (let* ((offset (unsigned->signed (fetch-byte-from-ram cpu) 1))
         (index (+ offset (reg-hl+ cpu))))
    (incf (pc cpu) 2)
    (elt (ram cpu) index)))

(defmethod (setf index-register) (value (cpu cpu))
  (incf (pc cpu))
  (let* ((offset (unsigned->signed (read-byte-from-ram cpu) 1))
         (index (+ offset (reg-hl+ cpu))))
    (incf (pc cpu))
    (setf (elt (ram cpu) index) value)))

(defmethod mem-hl+ ((cpu cpu))
  (case *addressing-mode*
    (:base (mem-hl cpu))
    (t (index-register cpu))))

(defmethod (setf mem-hl+) (value (cpu cpu))
  (case *addressing-mode*
    (:base (setf (mem-hl cpu) value))
    (t (setf (index-register cpu) value))))

(defparameter *8-bit-registers*
  (list 'reg-b 'reg-c 'reg-d 'reg-e 'reg-h 'reg-l 'mem-hl+ 'reg-a))

(defparameter *16-bit-registers*
  (list 'reg-bc 'reg-de 'reg-hl+ 'reg-sp))

(defparameter *16-bit-registers%*
  (list 'reg-bc% 'reg-de% 'reg-hl% 'reg-af%))

(defmacro with-addressing-mode (mode &body body)
  `(let ((*addressing-mode* ,mode))
     ,@body))
