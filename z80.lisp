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

(define-instruction nop #x00 #x1 (cpu))

(define-instruction ld-b-b #x40 #x1 (cpu) (setf (reg-b cpu) (reg-b cpu)))
(define-instruction ld-b-c #x41 #x1 (cpu) (setf (reg-b cpu) (reg-c cpu)))
(define-instruction ld-b-d #x42 #x1 (cpu) (setf (reg-b cpu) (reg-d cpu)))
(define-instruction ld-b-e #x43 #x1 (cpu) (setf (reg-b cpu) (reg-e cpu)))
(define-instruction ld-b-h #x44 #x1 (cpu) (setf (reg-b cpu) (reg-h cpu)))
(define-instruction ld-b-l #x45 #x1 (cpu) (setf (reg-b cpu) (reg-l cpu)))
(define-instruction ld-b-hl #x46 #x1 (cpu) (setf (reg-b cpu) (mem-hl cpu)))
(define-instruction ld-b-a #x47 #x1 (cpu) (setf (reg-b cpu) (reg-a cpu)))
(define-instruction ld-c-b #x48 #x1 (cpu) (setf (reg-c cpu) (reg-b cpu)))
(define-instruction ld-c-c #x49 #x1 (cpu) (setf (reg-c cpu) (reg-c cpu)))
(define-instruction ld-c-d #x4A #x1 (cpu) (setf (reg-c cpu) (reg-d cpu)))
(define-instruction ld-c-e #x4B #x1 (cpu) (setf (reg-c cpu) (reg-e cpu)))
(define-instruction ld-c-h #x4C #x1 (cpu) (setf (reg-c cpu) (reg-h cpu)))
(define-instruction ld-c-l #x4D #x1 (cpu) (setf (reg-c cpu) (reg-l cpu)))
(define-instruction ld-c-hl #x4E #x1 (cpu) (setf (reg-c cpu) (mem-hl cpu)))
(define-instruction ld-c-a #x4F #x1 (cpu) (setf (reg-c cpu) (reg-a cpu)))

(define-instruction ld-d-b #x50 #x1 (cpu) (setf (reg-d cpu) (reg-b cpu)))
(define-instruction ld-d-c #x51 #x1 (cpu) (setf (reg-d cpu) (reg-c cpu)))
(define-instruction ld-d-d #x52 #x1 (cpu) (setf (reg-d cpu) (reg-d cpu)))
(define-instruction ld-d-e #x53 #x1 (cpu) (setf (reg-d cpu) (reg-e cpu)))
(define-instruction ld-d-h #x54 #x1 (cpu) (setf (reg-d cpu) (reg-h cpu)))
(define-instruction ld-d-l #x55 #x1 (cpu) (setf (reg-d cpu) (reg-l cpu)))
(define-instruction ld-d-hl #x56 #x1 (cpu) (setf (reg-d cpu) (mem-hl cpu)))
(define-instruction ld-d-a #x57 #x1 (cpu) (setf (reg-d cpu) (reg-a cpu)))
(define-instruction ld-e-b #x58 #x1 (cpu) (setf (reg-e cpu) (reg-b cpu)))
(define-instruction ld-e-c #x59 #x1 (cpu) (setf (reg-e cpu) (reg-c cpu)))
(define-instruction ld-e-d #x5A #x1 (cpu) (setf (reg-e cpu) (reg-d cpu)))
(define-instruction ld-e-e #x5B #x1 (cpu) (setf (reg-e cpu) (reg-e cpu)))
(define-instruction ld-e-h #x5C #x1 (cpu) (setf (reg-e cpu) (reg-h cpu)))
(define-instruction ld-e-l #x5D #x1 (cpu) (setf (reg-e cpu) (reg-l cpu)))
(define-instruction ld-e-hl #x5E #x1 (cpu) (setf (reg-e cpu) (mem-hl cpu)))
(define-instruction ld-e-a #x5F #x1 (cpu) (setf (reg-e cpu) (reg-a cpu)))

(define-instruction ld-h-b #x60 #x1 (cpu) (setf (reg-h cpu) (reg-b cpu)))
(define-instruction ld-h-c #x61 #x1 (cpu) (setf (reg-h cpu) (reg-c cpu)))
(define-instruction ld-h-d #x62 #x1 (cpu) (setf (reg-h cpu) (reg-d cpu)))
(define-instruction ld-h-e #x63 #x1 (cpu) (setf (reg-h cpu) (reg-e cpu)))
(define-instruction ld-h-h #x64 #x1 (cpu) (setf (reg-h cpu) (reg-h cpu)))
(define-instruction ld-h-l #x65 #x1 (cpu) (setf (reg-h cpu) (reg-l cpu)))
(define-instruction ld-h-hl #x66 #x1 (cpu) (setf (reg-h cpu) (mem-hl cpu)))
(define-instruction ld-h-a #x67 #x1 (cpu) (setf (reg-h cpu) (reg-a cpu)))
(define-instruction ld-l-b #x68 #x1 (cpu) (setf (reg-l cpu) (reg-b cpu)))
(define-instruction ld-l-c #x69 #x1 (cpu) (setf (reg-l cpu) (reg-c cpu)))
(define-instruction ld-l-d #x6A #x1 (cpu) (setf (reg-l cpu) (reg-d cpu)))
(define-instruction ld-l-e #x6B #x1 (cpu) (setf (reg-l cpu) (reg-e cpu)))
(define-instruction ld-l-h #x6C #x1 (cpu) (setf (reg-l cpu) (reg-h cpu)))
(define-instruction ld-l-l #x6D #x1 (cpu) (setf (reg-l cpu) (reg-l cpu)))
(define-instruction ld-l-hl #x6E #x1 (cpu) (setf (reg-l cpu) (mem-hl cpu)))
(define-instruction ld-l-a #x6F #x1 (cpu) (setf (reg-l cpu) (reg-a cpu)))

(define-instruction ld-hl-b #x70 #x1 (cpu) (setf (mem-hl cpu) (reg-b cpu)))
(define-instruction ld-hl-c #x71 #x1 (cpu) (setf (mem-hl cpu) (reg-c cpu)))
(define-instruction ld-hl-d #x72 #x1 (cpu) (setf (mem-hl cpu) (reg-d cpu)))
(define-instruction ld-hl-e #x73 #x1 (cpu) (setf (mem-hl cpu) (reg-e cpu)))
(define-instruction ld-hl-h #x74 #x1 (cpu) (setf (mem-hl cpu) (reg-h cpu)))
(define-instruction ld-hl-l #x75 #x1 (cpu) (setf (mem-hl cpu) (reg-l cpu)))
(define-instruction halt #x76 #x1 (cpu) (halt cpu))
(define-instruction ld-hl-a #x77 #x1 (cpu) (setf (mem-hl cpu) (reg-a cpu)))
(define-instruction ld-a-b #x78 #x1 (cpu) (setf (reg-a cpu) (reg-b cpu)))
(define-instruction ld-a-c #x79 #x1 (cpu) (setf (reg-a cpu) (reg-c cpu)))
(define-instruction ld-a-d #x7A #x1 (cpu) (setf (reg-a cpu) (reg-d cpu)))
(define-instruction ld-a-e #x7B #x1 (cpu) (setf (reg-a cpu) (reg-e cpu)))
(define-instruction ld-a-h #x7C #x1 (cpu) (setf (reg-a cpu) (reg-h cpu)))
(define-instruction ld-a-l #x7D #x1 (cpu) (setf (reg-a cpu) (reg-l cpu)))
(define-instruction ld-a-hl #x7E #x1 (cpu) (setf (reg-a cpu) (mem-hl cpu)))
(define-instruction ld-a-a #x7F #x1 (cpu) (setf (reg-a cpu) (reg-a cpu)))

;; 16-bit inc-XX's -- probably doesn't work
(define-instruction inc-bc #x03 #x1 (cpu) (setf (reg-bc cpu) (+1 reg-bc cpu)))

;; 8-bit inc-X's
(define-instruction inc-b #x04 #x1 (cpu) (setf (reg-b cpu) (1+ (reg-b cpu))))
(define-instruction inc-d #x14 #x1 (cpu) (setf (reg-d cpu) (1+ (reg-d cpu))))
(define-instruction inc-h #x24 #x1 (cpu) (setf (reg-h cpu) (1+ (reg-h cpu))))
(define-instruction inc-c #x0C #x1 (cpu) (setf (reg-c cpu) (1+ (reg-c cpu))))
(define-instruction inc-e #x1C #x1 (cpu) (setf (reg-e cpu) (1+ (reg-e cpu))))
(define-instruction inc-l #x2C #x1 (cpu) (setf (reg-l cpu) (1+ (reg-l cpu))))
(define-instruction inc-a #x3C #x1 (cpu) (setf (reg-a cpu) (1+ (reg-a cpu))))

;; inc-hl needs ram access
(define-instruction inc-hl #x34 #x1 (cpu) (setf (reg-hl cpu) (1+ (reg-hl cpu))))

(define-instruction jp-nn #xC3 #x3 (cpu)
  (setf (pc cpu) (read-word cpu)))

(defun halt (cpu)
  (setf (slot-value cpu 'halted?) t))

(defun emulate-rom (cpu rom-path &key (max-instructions -1))
  (load-ram-from-rom-file cpu rom-path)
  (emulate cpu))

(defun emulate (cpu &key (max-instructions -1))
  (let
  (loop do
       (progn
         (incf max-
         (execute-next-instruction cpu)
         (incf (slot-value cpu 'pc) 1))
     while (not (slot-value cpu 'halted?))))

(let* ((c (make-instance 'cpu)))
  (emulate-rom c "/home/xeno/dev/z80/c-tests/next.rom"))
