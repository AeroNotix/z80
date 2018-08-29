(in-package :z80)


(defmacro while (condition &body body)
  `(loop
      while ,condition
      do
        ,@body))
