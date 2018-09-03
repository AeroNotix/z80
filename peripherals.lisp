(in-package :z80)

(defclass peripheral () ())

(defgeneric read-from (peripheral)
  (:documentation "Read a byte from the peripheral"))

(defgeneric write-to (peripheral value)
  (:documentation "Write a byte to the peripheral"))

(defclass simple-io-peripheral (peripheral)
  ((input-stream :initarg :input-stream :initform nil :accessor input-stream)
   (output-stream :initarg :input-stream :initform nil :accessor output-stream))
  (:documentation "Simple peripheral that prints chars to the terminal"))

(defmethod write-to ((tpp simple-io-peripheral) (b integer))
  (let ((s (or (output-stream tpp) *standard-output*)))
    (format s "~C" (code-char b))
    (finish-output s)))

(defmethod read-from ((tpp simple-io-peripheral))
  (let ((s (or (input-stream tpp) *standard-input*)))
    (read-char (input-stream tpp))))
