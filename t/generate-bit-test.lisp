(defparameter registers (list "A" "B" "C" "D" "E" "H" "L" "(HL)"))


(defun generate-bit-flip-test (path initial-value operation)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (format out "~C.ORG 0000H~%~%" #\Tab)

    (loop for register in registers
       do
         (progn
           (format out "~CLD ~A, ~A~%" #\Tab register initial-value)
           (loop for bit-index from 0 upto 7
              do
                (format out "~C~A ~D, ~A~%" #\Tab operation bit-index register))))

    (format out "~CHALT" #\Tab)))
