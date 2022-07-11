(defpackage z80
  (:use :common-lisp)
  (:import-from :alexandria :assoc-value)
  (:export

   ;; CPU
   #:cpu
   #:load-ram-from-rom-file
   #:emulate
   #:emulate-rom
   #:execute-instruction
   #:execute-next-instruction
   #:halted?
   #:pc
   #:ram
   #:reset-cpu

   ;; I/O
   #:peripheral
   #:read-from
   #:simple-io-peripheral
   #:write-to

   ;; Register functionality
   #:reg-a
   #:reg-a%
   #:reg-af
   #:reg-af%
   #:reg-b
   #:reg-b%
   #:reg-bc
   #:reg-bc%
   #:reg-c
   #:reg-c%
   #:reg-d
   #:reg-d%
   #:reg-de
   #:reg-de%
   #:reg-e
   #:reg-e%
   #:reg-f
   #:reg-f%
   #:reg-h
   #:reg-h%
   #:reg-hl
   #:reg-hl%
   #:reg-hl+
   #:reg-hl+%
   #:reg-i
   #:reg-ix
   #:reg-ix%
   #:reg-ixh
   #:reg-ixl
   #:reg-iy
   #:reg-iy%
   #:reg-iyh
   #:reg-iyl
   #:reg-l
   #:reg-l%
   #:reg-p
   #:reg-pc
   #:reg-pc-c
   #:reg-pc-p
   #:reg-r
   #:reg-s
   #:reg-s%
   #:reg-sp
   #:reg-sp%
   #:mem-hl

   ;; Flags
   #:flag-c
   #:flag-h
   #:flag-p
   #:flag-s
   #:flag-z))
