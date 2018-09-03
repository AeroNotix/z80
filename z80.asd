(asdf:defsystem #:z80
  :version "0.0.1"
  :description "Zilog Z80 Emulator"
  :licence "BSD"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "constants")
               (:file "z80")
               (:file "debug")
               (:file "instructions")
               (:file "tables")
               (:file "bitmaths")
               (:file "peripherals"))
  :depends-on (:alexandria
               :cl-fad
               :flexi-streams
               :place-utils))

(asdf:defsystem #:z80/tests
  :version "0.0.1"
  :description "Zilog Z80 Emulator test package"
  :licence "BSD"
  :depends-on (:z80 :fiveam)
  :components ((:module "t"
                        :serial t
                        :components
                        ((:file "package")
			             (:file "roms")
                         (:file "registers")))))

(asdf:defsystem #:z80/ui
  :version "0.0.1"
  :description "Zilog Z80 Emulator debugger/ui"
  :licence "BSD"
  :depends-on (:z80 :qt+libs :qtools :qtcore :qtgui)
  :components ((:module "ui"
                        :serial t
                        :components
                        ((:file "package")
			             (:file "main")))))
