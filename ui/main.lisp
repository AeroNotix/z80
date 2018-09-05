(in-package :z80-ui)

(named-readtables:in-readtable :qt)

(qt:ensure-smoke "qtuitools")

;;; TODO: macro/implement cluic
(defclass main-window ()
  ((cpu :accessor cpu :initform
        (make-instance
         'z80::cpu
         :peripherals (list (make-instance 'z80::simple-io-peripheral))))
   (run-btn :accessor run-btn)
   (step-btn :accessor step-btn)
   (current-instruction-le :accessor current-instruction-le)
   (sp-register-le :accessor sp-register-le)
   (pc-register-le :accessor pc-register-le)
   (af-register-le :accessor a-register-le)
   (bc-register-le :accessor b-register-le)
   (de-register-le :accessor d-register-le)
   (hl-register-le :accessor h-register-le)

   (ix-register-le :accessor ix-register-le)
   (iy-register-le :accessor iy-register-le)

   (halted-cb :accessor halted-cb)
   (s-flag-cb :accessor s-flag-cb)
   (z-flag-cb :accessor z-flag-cb)
   (f5-flag-cb :accessor f5-flag-cb)
   (h-flag-cb :accessor h-flag-cb)
   (f3-flag-cb :accessor f3-flag-cb)
   (p-flag-cb :accessor p-flag-cb)
   (n-flag-cb :accessor n-flag-cb)
   (c-flag-cb :accessor c-flag-cb)
   (open-file-action :accessor open-file-action))
  (:metaclass qt-class)
  (:qt-superclass "QMainWindow")
  (:slots
   ("halted_state_changed(int)" halted-state-changed)
   ("single_step_emulator()" step-emulator)
   ("run_emulator()" run-emulator)
   ("open_file_dialog()" open-file-dialog)))

(defmacro cpu-flag-to-ui (ui-element cpu-slot cpu)
 `(#_setChecked ,ui-element (funcall ,cpu-slot ,cpu)))

(defmethod cpu-state-to-ui ((instance main-window) (cpu z80::cpu))
  (with-slots (af-register-le
               bc-register-le
               de-register-le
               hl-register-le
               pc-register-le
               sp-register-le
               ix-register-le
               iy-register-le
               halted-cb
               s-flag-cb
               z-flag-cb
               f5-flag-cb
               h-flag-cb
               f3-flag-cb
               p-flag-cb
               n-flag-cb
               c-flag-cb
               current-instruction-le) instance
    (cpu-flag-to-ui c-flag-cb #'z80::flag-c cpu)
    (#_blockSignals halted-cb t)
    (cpu-flag-to-ui halted-cb #'z80::halted? cpu)
    (#_blockSignals halted-cb nil)
    (cpu-flag-to-ui s-flag-cb #'z80::flag-s cpu)
    (cpu-flag-to-ui z-flag-cb #'z80::flag-z cpu)
    (cpu-flag-to-ui p-flag-cb #'z80::flag-p cpu)
    (cpu-flag-to-ui h-flag-cb #'z80::flag-h cpu)
    (#_display pc-register-le (z80::reg-pc cpu))
    (#_display sp-register-le (z80::reg-sp cpu))
    (#_display af-register-le (z80::reg-af cpu))
    (#_display bc-register-le (z80::reg-bc cpu))
    (#_display de-register-le (z80::reg-de cpu))
    (#_display hl-register-le (z80::reg-hl cpu))
    (#_display ix-register-le (z80::reg-ix cpu))
    (#_display iy-register-le (z80::reg-iy cpu))))

(defmethod run-emulator ((instance main-window))
  ;; TODO: make the emulator run asynchronously
  ;; TODO: remove hacks like #_processEvents
  (loop
     do (progn
          (step-emulator instance)
          (#_processEvents qt:*qapplication*))
     while (not (z80::halted? (cpu instance)))))

(defmethod halted-state-changed ((instance main-window) state)
  (setf (z80::halted? (cpu instance)) (eq state 2)))

(defmethod step-emulator ((instance main-window))
  (z80::execute-next-instruction (cpu instance))
  (cpu-state-to-ui instance (cpu instance)))

(defmethod open-file-dialog ((instance main-window))
  (let ((cpu (cpu instance))
        (filename (#_QFileDialog::getOpenFileName
                   instance
                   "Select ROM" "" "Z80 ROM (*.rom *.z80);;All Files(*)")))
    (when (not (string= filename ""))
      (z80::load-ram-from-rom-file cpu filename)
      (z80::reset-cpu cpu)
      (cpu-state-to-ui instance cpu))))

(defun find-child (object name)
 (let ((children (#_children object)))
      (or
         (loop for child in children
               when (equal name (#_objectName child))
               return child)
         (loop for child in children
            thereis (find-child child name)))))

(defun ui-resource-location (filename)
  (namestring
   (cl-fad:merge-pathnames-as-file
    (asdf:system-source-directory :z80/ui) #P"ui/" filename)))

(defun set-style-sheet (widget file)
  (with-objects ((file (#_new QFile (ui-resource-location file))))
    (when (#_open file 1)
      (#_setStyleSheet widget (#_data (#_readAll file))))))

;;;; TODO: macro the everliving fuck out of this or get around to
;;;; implementing https://github.com/AeroNotix/cluic
(defmethod initialize-instance :after ((instance main-window) &key)
  (new instance)
  (with-objects ((file (#_new QFile (ui-resource-location #P"main.ui")))
                 (loader (#_new QUiLoader)))
    (when (#_open file 1)
      (let ((window (#_load loader file instance)))
        (#_close file)
        (with-slots (run-btn
                     step-btn

                     current-instruction-le

                     af-register-le
                     bc-register-le
                     c-register-le
                     de-register-le
                     hl-register-le
                     sp-register-le
                     pc-register-le
                     ix-register-le
                     iy-register-le

                     halted-cb

                     s-flag-cb
                     z-flag-cb
                     f5-flag-cb
                     h-flag-cb
                     f3-flag-cb
                     p-flag-cb
                     n-flag-cb
                     c-flag-cb) instance
          (setf current-instruction-le (find-child window "le_current_instruction")
                run-btn (find-child window "btn_Run")
                step-btn (find-child window "btn_Step")

                af-register-le (find-child window "AF_Reg")
                bc-register-le (find-child window "BC_Reg")
                de-register-le (find-child window "DE_Reg")
                hl-register-le (find-child window "HL_Reg")
                sp-register-le (find-child window "SP_Reg")
                pc-register-le (find-child window "PC_Reg")
                ix-register-le (find-child window "IX_Reg")
                iy-register-le (find-child window "IY_Reg")

                halted-cb (find-child window "cb_halted")

                s-flag-cb (find-child window "cb_S_flag")
                z-flag-cb (find-child window "cb_Z_flag")
                f5-flag-cb (find-child window "cb_F5_flag")
                h-flag-cb (find-child window "cb_H_flag")
                f3-flag-cb (find-child window "cb_F3_flag")
                p-flag-cb (find-child window "cb_PV_flag")
                n-flag-cb (find-child window "cb_N_flag")
                c-flag-cb (find-child window "cb_C_flag")

                open-file-action (find-child window "actionLoad_Rom"))
          (set-style-sheet instance #P"qlcdnumber.qss")
          (connect halted-cb "stateChanged(int)" instance "halted_state_changed(int)")
          (connect open-file-action "triggered()" instance "open_file_dialog()")
          (connect run-btn "clicked()" instance "run_emulator()")
          (connect step-btn "clicked()" instance "single_step_emulator()")
          (cpu-state-to-ui instance (cpu instance)))))))

(defun main ()
  (make-qapplication)
  (let ((mw (make-instance 'main-window)))
    (with-main-window (window mw))))
