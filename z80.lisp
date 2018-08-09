(in-package :z80)

(defparameter logging-enabled nil)

(defun int-to-hex (&rest ints)
  (format t "~{~X~^ ~}~%" ints))

(defclass cpu ()
  ((ram :initform (make-array 65535) :accessor ram)
   (elapsed-cycles :initform 0)
   (halted? :initform nil)
   ;; AF: 8-bit accumulator (A) and flag bits (F) carry, zero, minus,
   ;;     parity/overflow, half-carry (used for BCD), and an
   ;;     Add/Subtract flag (usually called N) also for BCD
   ;; BC: 16-bit data/address register or two 8-bit registers
   ;; DE: 16-bit data/address register or two 8-bit registers
   ;; HL: 16-bit accumulator/address register or two 8-bit registers
   ;; SP: stack pointer, 16 bits
   ;; PC: program counter, 16 bits
   (af :initform 0)
   (bc :initform 0)
   (de :initform 0)
   (hl :initform 0)
   (sp :initform 0 :accessor sp)
   (pc :initform 0 :accessor pc)))

(defmethod load-ram-from-seq ((cpu cpu) rom &key (offset 0))
  (replace (ram cpu) rom :start1 offset))

(defmethod load-ram-from-rom-file ((cpu cpu) rom-path &key (offset 0))
  (let ((rom (alexandria:read-file-into-byte-vector rom-path)))
    (load-ram-from-seq cpu rom :offset offset)))

(defmethod read-word ((cpu cpu) &key (address (pc cpu)))
  (logior (logand #xFFFF (elt (ram cpu) (1+ address)))
          (logand #xFFFF (ash (elt (ram cpu) address) 8))))

(defmethod execute-next-instruction ((cpu cpu))
  (let ((next-instruction (elt (ram cpu) (slot-value cpu 'pc))))
    (when logging-enabled
      (format t "NEXT INSTRUCTION: ~X~%" next-instruction))
    (incf (sp cpu))
    (funcall (microcode (elt opcode-table next-instruction)) cpu)))

(defmacro define-register-operators (register-name upper-name lower-name)
  (let ((upper-accessor (intern (format nil "REG-~S" upper-name)))
        (upper-setter (intern (format nil "SET-REG-~S" upper-name)))
        (lower-accessor (intern (format nil "REG-~S" lower-name)))
        (lower-setter (intern (format nil "SET-REG-~S" lower-name)))
        (whole-accessor (intern (format nil "REG-~S" register-name)))
        (whole-setter (intern (format nil "SET-REG-~S" register-name)))
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

     (defun ,upper-setter (,c ,value)
       (let ((,lower-reg-value (,lower-accessor ,c)))
         (setf (slot-value ,c ',register-name)
               (logior (ash (logand ,value #xFF) 8) ,lower-reg-value))))

     (defun ,lower-setter (,c ,value)
       (let ((,upper-reg-value (,upper-accessor ,c)))
         (setf (slot-value ,c ',register-name)
               (logior (ash ,upper-reg-value 8) (logand ,value #xFF)))))

     (defun ,whole-setter (,c ,value)
       (setf (slot-value ,c ',register-name) (logand ,value #xFFFF)))

     (defsetf ,upper-accessor ,upper-setter)
     (defsetf ,lower-accessor ,lower-setter)
     (defsetf ,whole-accessor ,whole-setter))))

(define-register-operators af a f)
(define-register-operators bc b c)
(define-register-operators de d e)
(define-register-operators hl h l)

(defun dump-registers (cpu)
  (format t "~{~{~2a ~^| ~}~%~}~%"
          (list '("a" "b" "c" "d" "e" "h")
                (list (reg-a cpu)
                      (reg-b cpu)
                      (reg-c cpu)
                      (reg-d cpu)
                      (reg-e cpu)
                      (reg-h cpu)))))

(defclass instruction ()
  ((name :initarg :name)
   (opcode :initarg :opcode)
   (size :initarg :size)
   (cycles :initarg :cycles)
   (microcode :initarg :microcode :accessor microcode)))

(defparameter opcode-table (make-array 257 ))

(defmacro define-instruction (name opcode size args &body body)

  `(let* ((inst (make-instance 'instruction
                               :name ',name
                               :opcode ,opcode
                               :size ,size
                               :microcode (lambda ,args ,@body))))
     (defparameter ,name inst)
     (setf (elt opcode-table ,opcode) ,name)
     inst))

(defun mem-hl (cpu)
  (elt (ram cpu) (reg-hl cpu)))

(defun (setf mem-hl) (value cpu)
  (setf (elt (ram cpu) (reg-hl cpu)) value))

(defun halt (cpu)
  (setf (slot-value cpu 'halted?) t))

(defun emulate-rom (cpu rom-path &key (max-instructions -1))
  (load-ram-from-rom-file cpu rom-path)
  (emulate cpu))

(defun emulate (cpu)
  (let
  (loop do
       (progn
         (execute-next-instruction cpu)
         (incf (slot-value cpu 'pc) 1))
     while (not (slot-value cpu 'halted?)))))

(let* ((c (make-instance 'cpu)))
  (emulate-rom c "/home/xeno/dev/z80/c-tests/next.rom"))
