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
