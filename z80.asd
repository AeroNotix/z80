;; -*- mode: common-lisp -*-
(defsystem z80
  :version "0.0.1"
  :description "Zilog Z80 Emulator"
  :licence "BSD"
  :serial t
  :components ((:file "package")
               (:file "z80")
               (:file "instructions"))
  :depends-on (:alexandria
               :flexi-streams))
