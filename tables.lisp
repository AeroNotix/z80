(in-package :z80)

(defparameter instruction-table
  (make-array 256 :initial-contents
              (list

	           nop
	           ld-rr-nn
	           ld-indirect-bc-a
	           inc-rr
	           inc-r
	           dec-r
	           ld-r-n
	           rlca

	           ex-af-af%
	           add-hl-rr
	           ld-a-indirect-bc
	           dec-rr
	           inc-r
	           dec-r
	           ld-r-n
	           rrca

	           djnz-d
	           ld-rr-nn
	           ld-indirect-de-a
	           inc-rr
	           inc-r
	           dec-r
	           ld-r-n
	           rla

	           jr-d
	           add-hl-rr
	           ld-a-indirect-de
	           dec-rr
	           inc-r
	           dec-r
	           ld-r-n
	           rra

	           jr-cc-d
	           ld-rr-nn
	           ld-indirect-nn-hl
	           inc-rr
	           inc-r
	           dec-r
	           ld-r-n
	           daa

	           jr-cc-d
	           add-hl-rr
	           ld-hl-indirect-nn
	           dec-rr
	           inc-r
	           dec-r
	           ld-r-n
	           cpl

	           jr-cc-d
	           ld-rr-nn
	           ld-indirect-nn-a
	           inc-rr
	           inc-indirect-hl
	           dec-indirect-hl
	           ld-indirect-hl-n
	           scf

	           jr-cc-d
	           add-hl-rr
	           ld-a-indirect-nn
	           dec-rr
	           inc-r
	           dec-r
	           ld-r-n
	           ccf

	           nop
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-indirect-hl
	           ld-r-r

	           ld-r-r
	           nop
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-indirect-hl
	           ld-r-r

	           ld-r-r
	           ld-r-r
	           nop
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-indirect-hl
	           ld-r-r

	           ld-r-r
	           ld-r-r
	           ld-r-r
	           nop
	           ld-r-r
	           ld-r-r
	           ld-r-indirect-hl
	           ld-r-r

	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           nop
	           ld-r-r
	           ld-r-indirect-hl
	           ld-r-r

	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           nop
	           ld-r-indirect-hl
	           ld-r-r

	           ld-indirect-hl-r
	           ld-indirect-hl-r
	           ld-indirect-hl-r
	           ld-indirect-hl-r
	           ld-indirect-hl-r
	           ld-indirect-hl-r
	           halt
	           ld-indirect-hl-r

	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-r
	           ld-r-indirect-hl
	           nop

	           add-r
	           add-r
	           add-r
	           add-r
	           add-r
	           add-r
	           add-r
	           add-r

	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-r

	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-r

	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r

	           and-r
	           and-r
	           and-r
	           and-r
	           and-r
	           and-r
	           and-r
	           and-r

	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-r

	           or-r
	           or-r
	           or-r
	           or-r
	           or-r
	           or-r
	           or-r
	           or-r

	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-r

	           ret-cc
	           pop-register
	           jp-cc-nn
	           jp-nn
	           call-cc-nn
	           push-register
	           add-n
	           rst-p

	           ret-cc
	           ret
	           jp-cc-nn
	           cb-prefix
	           call-cc-nn
	           call-nn
	           adc-n
	           rst-p

	           ret-cc
	           pop-register
	           jp-cc-nn
	           out-n-a
	           call-cc-nn
	           push-register
	           sub-n
	           rst-p

	           ret-cc
	           exx
	           jp-cc-nn
	           in-a-n
	           call-cc-nn
	           dd-prefix
	           sbc-n
	           rst-p

	           ret-cc
	           pop-register
	           jp-cc-nn
	           ex-indirect-sp-hl
	           call-cc-nn
	           push-register
	           and-n
	           rst-p

	           ret-cc
	           jp-hl
	           jp-cc-nn
	           ex-de-hl
	           call-cc-nn
	           ed-prefix
	           xor-n
	           rst-p

	           ret-cc
	           pop-register
	           jp-cc-nn
	           di
	           call-cc-nn
	           push-register
	           or-n
	           rst-p

	           ret-cc
	           ld-sp-hl
	           jp-cc-nn
	           ei
	           call-cc-nn
	           fd-prefix
	           cp-n
	           rst-p
               )))

(defparameter flag-table
  (list #'flag-nz #'flag-z #'flag-nc #'flag-c
        #'flag-po #'flag-pe #'flag-p #'flag-m))

(defparameter condition-table
  (list #'flag-nz #'flag-nc #'flag-z #'flag-c))

(defun find-condition (opcode-y)
  (elt condition-table opcode-y))
