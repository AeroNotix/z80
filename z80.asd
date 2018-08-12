(asdf:defsystem #:z80
  :version "0.0.1"
  :description "Zilog Z80 Emulator"
  :licence "BSD"
  :serial t
  :components ((:file "package")
               (:file "z80")
               (:file "instructions")
               (:file "peripherals"))
  :depends-on (:alexandria
               :flexi-streams))

(asdf:defsystem #:z80/tests
  :version "0.0.1"
  :description "Zilog Z80 Emulator"
  :licence "BSD"
  :depends-on (:z80 :fiveam)
  :components ((:module "t"
                        :serial t
                        :components
                        ((:file "package")
			             (:file "roms")
                         (:file "registers"))))
  ;; :perform (test-op (o s)
  ;;           (uiop:symbol-call :fiveam :run! 'z80-tests:all-tests))
  )
