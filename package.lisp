(defpackage z80
  (:use :common-lisp)
  (:import-from :place-utils :applyf :funcallf)
  (:import-from :alexandria :assoc-value)
  (:export

   ;; CPU
   #:cpu
   #:emulate
   #:emulate-rom
   #:execute-instruction
   #:execute-next-instruction
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
   #:reg-hl+-register
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
   #:mem-hl))
