(defsystem #:z80/asm
  :version "0.0.1"
  :description "Zilog Z80 Assembler"
  :license "BSD"
  :depends-on (:alexandria)
  :components ((:module "asm"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "assembler")))))


(defsystem #:z80
  :version "0.0.1"
  :description "Zilog Z80 Emulator"
  :licence "BSD"
  :serial t
  :components ((:file "package")
               (:file "peripherals")
               (:file "z80")
               (:file "macros")
               (:file "addressing")
               (:file "constants")
               (:file "instructions")
               (:file "tables")
               (:file "bitmaths")
               (:file "debug"))
  :depends-on (:alexandria
               :cl-fad
               :flexi-streams
               :place-utils
               :z80/asm))

(defsystem #:z80/tests
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

(defsystem #:z80/ui
  :version "0.0.1"
  :description "Zilog Z80 Emulator debugger/ui"
  :licence "BSD"
  :depends-on (:z80 :qt+libs :qtools :qtcore :qtgui)
  :components ((:module "ui"
                        :serial t
                        :components
                        ((:file "package")
			             (:file "main")))))
