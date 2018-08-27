(in-package :z80)


(defgeneric read-from (peripheral)
  (:documentation "Read a byte from the peripheral"))

(defgeneric write-to (peripheral value)
  (:documentation "Write a byte to the peripheral"))

(defclass terminal-printer-peripheral () ()
  (:documentation "Simple peripheral that prints chars to the terminal"))

(defmethod write-to ((tpp terminal-printer-peripheral) (b integer))
  (format t "~C" (code-char b))
  (finish-output))
