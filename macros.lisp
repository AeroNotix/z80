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

(defmacro funcallf (f place &rest args)
  `(setf ,place (apply ,f ,place (list ,@args))))
