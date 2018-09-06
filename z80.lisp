(in-package :z80)

(defclass cpu ()
  ((ram :initform (make-array 65536 :initial-element #x00) :accessor ram)
   (elapsed-cycles :initform 0)
   (interrupts-enabled? :initform T :accessor interrupts-enabled?)
   (halted? :initform nil :accessor halted?)
   ;; This z80 emulator only implements the CPU. This peripherals list
   ;; allows code using the emulator to attach peripherals on the I/O
   ;; ports that will be written to/read from when the IN/OUT
   ;; instructions are executed.
   (peripherals :initform (list (make-instance 'simple-io-peripheral))
                :accessor peripherals :initarg :peripherals)
   ;; R: contains the DRAM refresh count.
   (r :initform 0 :accessor reg-r)
   ;; I: the IV register
   (i :initform 0 :accessor reg-i)
   ;; Index registers
   (ix :initform 0)
   (iy :initform 0)
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

(defparameter 16-bit-register->8-bit-registers (make-hash-table))

(defun 16-bit-register->8-bit-registers (16-bit-register-place)
  (gethash 16-bit-register-place 16-bit-register->8-bit-registers))

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
       (setf (elt (ram ,c) (,whole-accessor ,c)) ,value))

     (setf (gethash ',whole-accessor 16-bit-register->8-bit-registers)
           (cons ',upper-accessor ',lower-accessor)))))

(define-register-operators ix ixh ixl)
(define-register-operators iy iyh iyl)
(define-register-operators af a f)
(define-register-operators bc b c)
(define-register-operators de d e)
(define-register-operators hl h l)
(define-register-operators sp s p)
(define-register-operators pc pc-p pc-c)
(define-register-operators af% a% f%)
(define-register-operators bc% b% c%)
(define-register-operators de% d% e%)
(define-register-operators hl% h% l%)
(define-register-operators sp% s% s%)

(defmethod load-ram-from-seq ((cpu cpu) rom &key (offset 0))
  (replace (ram cpu) rom :start1 offset))

(defmethod load-ram-from-rom-file ((cpu cpu) rom-path &key (offset 0))
  (let ((rom (alexandria:read-file-into-byte-vector rom-path)))
    (load-ram-from-seq cpu rom :offset offset)))

(defmethod read-byte-from-ram ((cpu cpu) &key (address (pc cpu)))
  (logand #xFFFF (elt (ram cpu) address)))

(defmethod fetch-byte-from-ram ((cpu cpu))
  (read-byte-from-ram cpu :address (1+ (pc cpu))))

(defmethod read-word-from-ram ((cpu cpu) &key (address (pc cpu)))
  (logior (logand #xFFFF (elt (ram cpu) address))
          (logand #xFFFF (ash (elt (ram cpu) (1+ address)) 8))))

(defmethod fetch-word-from-ram ((cpu cpu) &key (address (1+ (pc cpu))))
  (read-word-from-ram cpu :address address))

(defmethod write-word-to-ram ((cpu cpu) address value)
  (setf (elt (ram cpu) address) (rshift value 8))
  (setf (elt (ram cpu) (1+ address)) (logand #x00FF value)))

(defmethod read-port ((cpu cpu) port-id)
  (let ((peripheral (nth port-id (peripherals cpu))))
    (if peripheral
        (read-from peripheral)
        0)))

(defmethod write-port ((cpu cpu) (peripheral peripheral) value)
  (write-to peripheral value))

(defmethod write-port ((cpu cpu) port-id value)
  (let ((peripheral (nth port-id (peripherals cpu))))
    (when peripheral
      (write-to peripheral value))))

(defmethod port-c ((cpu cpu))
  (read-from (elt (peripherals cpu) (reg-c cpu))))

(defmethod (setf port-c) (value (cpu cpu))
  (when (>= (length (peripherals cpu)) (reg-c cpu))
    (write-port cpu (elt (peripherals cpu) (reg-c cpu)) value)))

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

(defun (setf flag-c) (value cpu)
  (setf (reg-f cpu) (logior (reg-f cpu) (ash value c-flag-pos))))

(defmethod flag-nc ((cpu cpu))
  (not (flag-c cpu)))

(defmethod flag-p ((cpu cpu))
  (logbitp p-flag-pos (reg-f cpu)))

(defmethod flag-po ((cpu cpu))
  (warn "flag-po must be implemented to interpret flag-po as flag-p = 0")
  (logbitp p-flag-pos (reg-f cpu)))

(defmethod flag-pe ((cpu cpu))
  (warn "flag-pe must be implemented to interpret flag-po as flag-p = 1")
  (logbitp p-flag-pos (reg-f cpu)))

(defmethod flag-m ((cpu cpu))
  (warn "flag-m must be implemented to interpret flag-s as flag-s = 1")
  (logbitp n-flag-pos (reg-f cpu)))

(defmethod flag-n ((cpu cpu))
  (logbitp n-flag-pos (reg-f cpu)))

(defmethod flag-h ((cpu cpu))
  (logbitp h-flag-pos (reg-f cpu)))

(defmethod reset-cpu ((cpu cpu))
  (setf (slot-value cpu 'r) #x00)
  (setf (slot-value cpu 'i) #x00)
  (setf (reg-af cpu) #x0000
        (reg-bc cpu) #x0000
        (reg-hl cpu) #x0000
        (reg-ix cpu) #x0000
        (reg-iy cpu) #x0000)
  (setf (reg-pc cpu) #x0000))

(defmethod execute-instruction ((cpu cpu) next-instruction opcode)
  (funcall (microcode next-instruction) cpu opcode))

(defgeneric execute-next-instruction (cpu &optional instruction-table handle-pc))

(defmethod execute-next-instruction ((cpu cpu) &optional (instruction-table unprefixed-table) (handle-pc t))
  (let* ((opcode (elt (ram cpu) (pc cpu)))
         (next-instruction (next-instruction instruction-table opcode))
         (orig-pc (pc cpu)))
    (when logging-enabled
      (debug-cpu cpu))
    (execute-instruction cpu next-instruction opcode)
    (when (and (eq orig-pc (pc cpu)) handle-pc)
      (incf (pc cpu) (instruction-size instruction-table next-instruction)))
    next-instruction))

(defun emulate-rom (cpu rom-path &key (starting-pc 0) (max-instructions single-float-positive-infinity))
  (load-ram-from-rom-file cpu rom-path)
  (emulate cpu :starting-pc starting-pc :max-instructions max-instructions))

(defun emulate (cpu &key (starting-pc 0) (max-instructions single-float-positive-infinity))
  (setf (pc cpu) starting-pc)
  (let ((num-instructions 0))
    (loop
       do
         (execute-next-instruction cpu)
         (incf num-instructions)
       while (and (not (>= num-instructions max-instructions))
                  (not (halted? cpu))))
    cpu))
