(in-package :z80)


(defun spy (term)
  (when logging-enabled
    (format t "~A~%" term))
  term)

(defun int-to-hex (&rest ints)
  (format t "~{~X~^ ~}~%" ints))

(defun int-to-bin (&rest ints)
  (format t "~{~B~^ ~}~%" ints))

(defmethod debug-cpu ((cpu cpu))
  (dump-flags cpu))

(defun dump-registers-to-values (cpu)
  (values (reg-a cpu)
          (reg-b cpu)
          (reg-c cpu)
          (reg-d cpu)
          (reg-e cpu)
          (reg-h cpu)
          (reg-l cpu)
          (reg-f cpu)))

(defun dump-registers-to-list (cpu)
  (list (reg-a cpu)
        (reg-b cpu)
        (reg-c cpu)
        (reg-d cpu)
        (reg-e cpu)
        (reg-h cpu)
        (reg-f cpu)))

(defun dump-registers (cpu)
  (format t "~{~{~2a ~^| ~}~%~}~%"
          (list '("a" "b" "c" "d" "e" "h" "f")
                (dump-registers-to-list cpu))))

(defun dump-flags (cpu)
  (format t "~{~{~3a ~^| ~}~%~}~%"
          (list '("C" "Z" "P" "S" "N" "H")
                (list (flag-c cpu)
                      (flag-z cpu)
                      (flag-p cpu)
                      (flag-s cpu)
                      (flag-n cpu)
                      (flag-h cpu)))))
