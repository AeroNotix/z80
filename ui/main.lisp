(in-package :z80-ui)

(named-readtables:in-readtable :qt)

(qt:ensure-smoke "qtuitools")

(defclass main-window ()
  ((cpu :accessor cpu :initform (make-instance 'z80::cpu)))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow"))

(defun read-ui-file (filename)
  (when (probe-file filename)
    (let ((file (#_new QFile filename))
	      (loader (#_new QUiLoader)))
      (#_open file 1)
      (#_load loader file))))

(defmethod z80::execute-next-intruction :after ((cpu z80::cpu))
  (format t "HELLO~%"))

(defun main ()
  (with-main-window (window (read-ui-file "/home/xeno/dev/z80/ui/main.ui"))))
