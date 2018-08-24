(in-package :z80-ui)

(named-readtables:in-readtable :qt)

(qt:ensure-smoke "qtuitools")

;;; TODO: macro/implement cluic
(defclass main-window ()
  ((cpu :accessor cpu :initform (make-instance 'z80::cpu))
   (run-btn :accessor run-btn)
   (step-btn :accessor step-btn)
   (sp-register-le :accessor sp-register-le)
   (pc-register-le :accessor pc-register-le)
   (a-register-le :accessor a-register-le)
   (b-register-le :accessor b-register-le)
   (c-register-le :accessor c-register-le)
   (d-register-le :accessor d-register-le)
   (e-register-le :accessor e-register-le)
   (h-register-le :accessor h-register-le)
   (l-register-le :accessor h-register-le)
   (f-register-le :accessor f-register-le)
   (s-flag-cb :accessor s-flag-cb)
   (z-flag-cb :accessor z-flag-cb)
   (f5-flag-cb :accessor f5-flag-cb)
   (h-flag-cb :accessor h-flag-cb)
   (f3-flag-cb :accessor f3-flag-cb)
   (p-flag-cb :accessor p-flag-cb)
   (n-flag-cb :accessor n-flag-cb)
   (c-flag-cb :accessor c-flag-cb))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots ("single_step_emulator()" step-emulator)
          ("run_emulator()" run-emulator)))

(defmacro cpu-flag-to-ui (ui-element cpu-slot cpu)
 `(#_setChecked ,ui-element (funcall ,cpu-slot ,cpu)))

(defmethod cpu-state-to-ui ((instance main-window) (cpu z80::cpu))
  (with-slots (a-register-le b-register-le c-register-le d-register-le
                             e-register-le h-register-le l-register-le
                             f-register-le pc-register-le
                             sp-register-le s-flag-cb z-flag-cb
                             f5-flag-cb h-flag-cb f3-flag-cb p-flag-cb
                             n-flag-cb c-flag-cb) instance
    (#_setText sp-register-le (format nil "~d" (z80::sp (cpu instance))))
    (#_setText pc-register-le (format nil "~d" (z80::pc (cpu instance))))
    (cpu-flag-to-ui s-flag-cb #'z80::flag-s cpu)
    (cpu-flag-to-ui z-flag-cb #'z80::flag-z cpu)
    (cpu-flag-to-ui p-flag-cb #'z80::flag-p cpu)
    (cpu-flag-to-ui h-flag-cb #'z80::flag-h cpu)
    (multiple-value-bind (a-reg b-reg c-reg d-reg e-reg h-reg l-reg f-reg) (z80::dump-registers-to-values cpu)
      (#_setText a-register-le (format nil "0x~X" a-reg))
      (#_setText b-register-le (format nil "0x~X" b-reg))
      (#_setText c-register-le (format nil "0x~X" c-reg))
      (#_setText d-register-le (format nil "0x~X" d-reg))
      (#_setText e-register-le (format nil "0x~X" e-reg))
      (#_setText f-register-le (format nil "0x~X" f-reg))
      (#_setText h-register-le (format nil "0x~X" h-reg))
      (#_setText l-register-le (format nil "0x~X" l-reg)))))

(defmethod run-emulator ((instance main-window))
  (z80::emulate (cpu instance)))

(defmethod step-emulator ((instance main-window))
  (z80::execute-next-instruction (cpu instance))
  (cpu-state-to-ui instance (cpu instance)))

(defun find-child (object name)
 (let ((children (#_children object)))
      (or
         (loop for child in children
               when (equal name (#_objectName child))
               return child)
         (loop for child in children
            thereis (find-child child name)))))

;;;; TODO: macro the everliving fuck out of this or get around to
;;;; implementing https://github.com/AeroNotix/cluic
(defmethod initialize-instance :after ((instance main-window) &key)
  (new instance)
  (with-objects ((file (#_new QFile "/home/xeno/dev/z80/ui/main.ui"))
                 (loader (#_new QUiLoader)))
    (when (#_open file 1)
      (let ((window (#_load loader file instance)))
        (#_close file)
        (with-slots (run-btn
                     step-btn
                     a-register-le
                     b-register-le
                     c-register-le
                     d-register-le
                     e-register-le
                     h-register-le
                     f-register-le
                     l-register-le
                     sp-register-le
                     pc-register-le
                     s-flag-cb
                     z-flag-cb
                     f5-flag-cb
                     h-flag-cb
                     f3-flag-cb
                     p-flag-cb
                     n-flag-cb
                     c-flag-cb) instance
          (setf run-btn (find-child window "btn_Run")
                step-btn (find-child window "btn_Step")
                a-register-le (find-child window "le_A_Reg")
                b-register-le (find-child window "le_B_Reg")
                c-register-le (find-child window "le_C_Reg")
                d-register-le (find-child window "le_D_Reg")
                e-register-le (find-child window "le_E_Reg")
                h-register-le (find-child window "le_H_Reg")
                l-register-le (find-child window "le_L_Reg")
                f-register-le (find-child window "le_F_Reg")
                sp-register-le (find-child window "le_SP_Reg")
                pc-register-le (find-child window "le_PC_Reg")
                s-flag-cb (find-child window "cb_S_flag")
                z-flag-cb (find-child window "cb_Z_flag")
                f5-flag-cb (find-child window "cb_F5_flag")
                h-flag-cb (find-child window "cb_H_flag")
                f3-flag-cb (find-child window "cb_F3_flag")
                p-flag-cb (find-child window "cb_PV_flag")
                n-flag-cb (find-child window "cb_N_flag")
                c-flag-cb (find-child window "cb_C_flag"))
          (connect run-btn "clicked()" instance "run_emulator()")
          (connect step-btn "clicked()" instance "single_step_emulator()")
          (cpu-state-to-ui instance (cpu instance)))))))

(defun main ()
  (make-qapplication)
  (let ((mw (make-instance 'main-window)))
    (z80::load-ram-from-rom-file (cpu mw) "/home/xeno/dev/z80/t/test-roms/flags.rom")
    (with-main-window (window mw))))
