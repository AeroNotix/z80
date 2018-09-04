(in-package :z80)


(defmacro while (condition &body body)
  `(loop
      while ,condition
      do
        ,@body))

(defmacro setf-of (place)
  `(fdefinition `(setf ,,place)))

(defmacro incf-of (place)
  `(fdefinition `(incf ,,place)))

(defmacro decf-of (place)
  `(fdefinition `(decf ,,place)))

(defmacro updatef (place update-function)
  `(let ((value ,place))
     (setf ,place (funcall value))))

(defmacro with-addressing-mode (mode &body body)
  (let ((mode (case mode
                (:base (list '*8-bit-registers-base* '*16-bit-registers-base* '*16-bit-registers%-base*))
                (:ix (list '*8-bit-registers-indexed-ix* '*16-bit-registers-indexed-ix* '*16-bit-registers%-indexed-ix*))
                (:iy (list '*8-bit-registers-indexed-iy* '*16-bit-registers-indexed-iy* '*16-bit-registers%-indexed-iy*)))))
    `(destructuring-bind (*8-bit-registers* *16-bit-registers* *16-bit-registers%*) ,mode
       ,@body)))
