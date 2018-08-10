(in-package :z80)


(defgeneric read-from (peripheral)
  (:documentation "Read a byte from the peripheral"))

(defgeneric write-to (peripheral)
  (:documentation "Write a byte to the peripheral"))
