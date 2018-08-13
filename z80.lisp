(in-package :z80)

(defun spy (term)
  (when logging-enabled
    (format t "~A~%" term))
  term)

(defun int-to-hex (&rest ints)
  (format t "~{~X~^ ~}~%" ints))

(defun int-to-bin (&rest ints)
  (format t "~{~B~^ ~}~%" ints))

(defclass cpu ()
  ((ram :initform (make-array 65535) :accessor ram)
   (elapsed-cycles :initform 0)
   (interrupts :initform :enabled :accessor interrupts)
   (halted? :initform nil)
   ;; This z80 emulator only implements the CPU. This peripherals list
   ;; allows code using the emulator to attach peripherals on the I/O
   ;; ports that will be written to/read from when the IN/OUT
   ;; instructions are executed.
   (peripherals :initform nil :accessor peripherals :initarg :peripherals)
   ;; AF: 8-bit accumulator (A) and flag bits (F) carry, zero, minus,
   ;;     parity/overflow, half-carry (used for BCD), and an
   ;;     Add/Subtract flag (usually called N) also for BCD
   ;; BC: 16-bit data/address register or two 8-bit registers
   ;; DE: 16-bit data/address register or two 8-bit registers
   ;; HL: 16-bit accumulator/address register or two 8-bit registers
   ;; RR': 16-bit shadow registers
   ;; SP: stack pointer, 16 bits
   ;; PC: program counter, 16 bits
   (af :initform 0)
   (bc :initform 0)
   (de :initform 0)
   (hl :initform 0)
   (af% :initform 0)
   (bc% :initform 0)
   (de% :initform 0)
   (hl% :initform 0)
   (sp :initform 0 :accessor sp)
   (pc :initform 0 :accessor pc)))

(defmethod debug-cpu ((cpu cpu))
  (let ((next-instruction (elt (ram cpu) (slot-value cpu 'pc))))
    (format t "NEXT INSTRUCTION : ~X / ~A PC: ~D~%"
            next-instruction next-instruction (pc cpu))
    ;; (format t "RAM: ~A~%" (ram cpu))
    ))

(defmethod carry-flag ((cpu cpu))
  (logand 1 (reg-f cpu)))

(defmethod carry-flag-set? ((cpu cpu))
  (eq 1 (carry-flag cpu)))

(defmethod zero-flag ((cpu cpu))
  (logand #x40 (reg-f cpu)))

(defmethod zero-flag-set? ((cpu cpu))
  (eq 1 (carry-flag cpu)))

(defmethod load-ram-from-seq ((cpu cpu) rom &key (offset 0))
  (replace (ram cpu) rom :start1 offset))

(defmethod load-ram-from-rom-file ((cpu cpu) rom-path &key (offset 0))
  (let ((rom (alexandria:read-file-into-byte-vector rom-path)))
    (load-ram-from-seq cpu rom :offset offset)))

(defmethod read-byte-from-ram ((cpu cpu) &key (address (pc cpu)))
  (logand #xFFFF (elt (ram cpu) address)))

(defmethod fetch-byte-from-ram ((cpu cpu))
  (read-byte-from-ram cpu :address (1+ (pc cpu))))

(defmethod read-word ((cpu cpu) &key (address (pc cpu)))
  (spy (logior (logand #xFFFF (elt (ram cpu) address))
               (logand #xFFFF (ash (elt (ram cpu) (1+ address)) 8)))))

(defmethod fetch-word ((cpu cpu))
  (read-word cpu :address (1+ (pc cpu))))

(defmethod read-port ((cpu cpu) port-id)
  (let ((peripheral (nth port-id (peripherals cpu))))
    (if peripheral
        (read-from peripheral)
        0)))

(defmethod write-port ((cpu cpu) port-id value)
  (let ((peripheral (nth port-id (peripherals cpu))))
    (when peripheral
      (write-from peripheral))))

(defmethod execute-next-instruction ((cpu cpu))
  (let* ((next-instruction (elt (ram cpu) (pc cpu)))
         (opcode (elt opcode-table next-instruction))
         (orig-pc (pc cpu)))
    (when logging-enabled
      (debug-cpu cpu))
    (when (eq 0 opcode)
      (error
       (format nil "Unknown instruction: ~X / position: ~S"
               next-instruction (pc cpu))))
    (funcall (microcode opcode) cpu)
    (when (eq orig-pc (pc cpu))
      (incf (pc cpu) (size opcode)))))

(defmacro define-register-operators (register-name upper-name lower-name)
  (let ((upper-accessor (intern (format nil "REG-~S" upper-name)))
        (lower-accessor (intern (format nil "REG-~S" lower-name)))
        (whole-accessor (intern (format nil "REG-~S" register-name)))
        (mem-accessor (intern (format nil "MEM-~S" register-name)))
        (c (gensym))
        (value (gensym))
        (upper-reg-value (gensym))
        (lower-reg-value (gensym)))
  `(progn

     (defun ,upper-accessor (,c)
       (ash (slot-value ,c ',register-name) -8))

     (defun ,lower-accessor (,c)
       (logand (slot-value ,c ',register-name) #xFF))

     (defun ,whole-accessor (,c)
       (slot-value ,c ',register-name))

     (defun (setf ,upper-accessor) (,value ,c)
       (let ((,lower-reg-value (,lower-accessor ,c)))
         (setf (slot-value ,c ',register-name)
               (logior (ash (logand ,value #xFF) 8) ,lower-reg-value))))

     (defun (setf ,lower-accessor) (,value ,c)
       (let ((,upper-reg-value (,upper-accessor ,c)))
         (setf (slot-value ,c ',register-name)
               (logior (ash ,upper-reg-value 8) (logand ,value #xFF)))))

     (defun (setf ,whole-accessor) (,value ,c)
       (setf (slot-value ,c ',register-name) (logand ,value #xFFFF)))

     (defun ,mem-accessor (,c)
       (elt (ram ,c) (,whole-accessor ,c)))

     (defun (setf ,mem-accessor) (,value ,c)
       (setf (elt (ram ,c) (,whole-accessor ,c)) ,value)))))

(define-register-operators af a f)
(define-register-operators bc b c)
(define-register-operators de d e)
(define-register-operators hl h l)

(define-register-operators af% a% f%)
(define-register-operators bc% b% c%)
(define-register-operators de% d% e%)
(define-register-operators hl% h% l%)

(defmethod flag-s ((cpu cpu))
  (logbitp s-flag-pos (reg-f cpu)))

(defmethod flag-h ((cpu cpu))
  (logbitp h-flag-pos (reg-f cpu)))

(defmethod flag-z ((cpu cpu))
  (logbitp z-flag-pos (reg-f cpu)))

(defmethod flag-nz ((cpu cpu))
  (not (flag-z cpu)))

(defmethod flag-c ((cpu cpu))
  (logbitp c-flag-pos (reg-f cpu)))

(defmethod flag-nc ((cpu cpu))
  (not (flag-c cpu)))

(defmethod flag-p ((cpu cpu))
  (logbitp p-flag-pos (reg-f cpu)))

(defmethod flag-n ((cpu cpu))
  (logbitp n-flag-pos (reg-f cpu)))

(defun dump-registers (cpu)
  (format t "~{~{~2a ~^| ~}~%~}~%"
          (list '("a" "b" "c" "d" "e" "h" "f")
                (list (reg-a cpu)
                      (reg-b cpu)
                      (reg-c cpu)
                      (reg-d cpu)
                      (reg-e cpu)
                      (reg-h cpu)
                      (reg-f cpu)))))

(defun dump-flags (cpu)
  (format t "~{~{~3a ~^| ~}~%~}~%"
          (list '("C" "Z" "P" "S" "N" "H")
                (list (flag-c cpu)
                      (flag-z cpu)
                      (flag-p cpu)
                      (flag-s cpu)
                      (flag-n cpu)
                      (flag-h cpu)))))

(defun halt (cpu)
  (setf (slot-value cpu 'halted?) t))

(defun unhalt (cpu)
  (setf (slot-value cpu 'halted?) nil))

(defun emulate-rom (cpu rom-path &key (max-instructions single-float-positive-infinity))
  (load-ram-from-rom-file cpu rom-path)
  (emulate cpu :max-instructions max-instructions))

(defun emulate (cpu &key (max-instructions single-float-positive-infinity))
  (let ((num-instructions 0))
    (loop do
         (execute-next-instruction cpu)
         (incf num-instructions)
       while (and (not (>= num-instructions max-instructions))
                  (not (slot-value cpu 'halted?))))
    cpu))
