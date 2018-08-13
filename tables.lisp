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

	           djnz-e
	           ld-rr-nn
	           ld-indirect-de-a
	           inc-rr
	           inc-r
	           dec-r
	           ld-r-n
	           rla

	           jr-e
	           add-hl-rr
	           ld-a-indirect-de
	           dec-rr
	           inc-r
	           dec-r
	           ld-r-n
	           rra

	           jr-dd-e
	           ld-rr-nn
	           ld-indirect-nn-hl
	           inc-rr
	           inc-r
	           dec-r
	           ld-r-n
	           daa

	           jr-dd-e
	           add-hl-rr
	           ld-hl-indirect-nn
	           dec-rr
	           inc-r
	           dec-r
	           ld-r-n
	           cpl

	           jr-dd-e
	           ld-rr-nn
	           ld-indirect-nn-a
	           inc-rr
	           inc-indirect-hl
	           dec-indirect-hl
	           ld-indirect-hl-n
	           scf

	           jr-dd-e
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
	           add-indirect-hl
	           add-r

	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-r
	           adc-indirect-hl
	           adc-r

	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-r
	           sub-indirect-hl
	           sub-r

	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-r
	           sbc-indirect-hl
	           sbc-r

	           and-r
	           and-r
	           and-r
	           and-r
	           and-r
	           and-r
	           and-indirect-hl
	           and-r

	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-r
	           xor-indirect-hl
	           xor-r

	           or-r
	           or-r
	           or-r
	           or-r
	           or-r
	           or-r
	           or-indirect-hl
	           or-r

	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-r
	           cp-indirect-hl
	           cp-r

	           ret-cc
	           pop-ss
	           jp-cc-nn
	           jp-nn
	           call-cc-nn
	           push-ss
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
	           pop-ss
	           jp-cc-nn
	           out-n-a
	           call-cc-nn
	           push-ss
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
	           pop-ss
	           jp-cc-nn
	           ex-indirect-sp-hl
	           call-cc-nn
	           push-ss
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
	           pop-ss
	           jp-cc-nn
	           di
	           call-cc-nn
	           push-ss
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
  #(#'flag-nz #'flag-z #'flag-nc #'flag-c
    #'flag-po #'flag-pe #'flag-p #'flag-m))

(defparameter szyx-flags-table
  (list
	#x40 #x00 #x00 #x00 #x00 #x00 #x00 #x00
	#x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
	#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
	#x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
	#x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20
	#x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28
	#x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20
	#x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28
	#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
	#x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
	#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
	#x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
	#x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20
	#x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28
	#x20 #x20 #x20 #x20 #x20 #x20 #x20 #x20
	#x28 #x28 #x28 #x28 #x28 #x28 #x28 #x28
	#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80
	#x88 #x88 #x88 #x88 #x88 #x88 #x88 #x88
	#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80
	#x88 #x88 #x88 #x88 #x88 #x88 #x88 #x88
	#xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0
	#xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8
	#xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0
	#xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8
	#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80
	#x88 #x88 #x88 #x88 #x88 #x88 #x88 #x88
	#x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80
	#x88 #x88 #x88 #x88 #x88 #x88 #x88 #x88
	#xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0
	#xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8
	#xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0 #xa0
	#xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8 #xa8))

(defparameter szyxp-flags-table
  (list
	#x44 #x00 #x00 #x04 #x00 #x04 #x04 #x00
	#x08 #x0c #x0c #x08 #x0c #x08 #x08 #x0c
	#x00 #x04 #x04 #x00 #x04 #x00 #x00 #x04
	#x0c #x08 #x08 #x0c #x08 #x0c #x0c #x08
	#x20 #x24 #x24 #x20 #x24 #x20 #x20 #x24
	#x2c #x28 #x28 #x2c #x28 #x2c #x2c #x28
	#x24 #x20 #x20 #x24 #x20 #x24 #x24 #x20
	#x28 #x2c #x2c #x28 #x2c #x28 #x28 #x2c
	#x00 #x04 #x04 #x00 #x04 #x00 #x00 #x04
	#x0c #x08 #x08 #x0c #x08 #x0c #x0c #x08
	#x04 #x00 #x00 #x04 #x00 #x04 #x04 #x00
	#x08 #x0c #x0c #x08 #x0c #x08 #x08 #x0c
	#x24 #x20 #x20 #x24 #x20 #x24 #x24 #x20
	#x28 #x2c #x2c #x28 #x2c #x28 #x28 #x2c
	#x20 #x24 #x24 #x20 #x24 #x20 #x20 #x24
	#x2c #x28 #x28 #x2c #x28 #x2c #x2c #x28
	#x80 #x84 #x84 #x80 #x84 #x80 #x80 #x84
	#x8c #x88 #x88 #x8c #x88 #x8c #x8c #x88
	#x84 #x80 #x80 #x84 #x80 #x84 #x84 #x80
	#x88 #x8c #x8c #x88 #x8c #x88 #x88 #x8c
	#xa4 #xa0 #xa0 #xa4 #xa0 #xa4 #xa4 #xa0
	#xa8 #xac #xac #xa8 #xac #xa8 #xa8 #xac
	#xa0 #xa4 #xa4 #xa0 #xa4 #xa0 #xa0 #xa4
	#xac #xa8 #xa8 #xac #xa8 #xac #xac #xa8
	#x84 #x80 #x80 #x84 #x80 #x84 #x84 #x80
	#x88 #x8c #x8c #x88 #x8c #x88 #x88 #x8c
	#x80 #x84 #x84 #x80 #x84 #x80 #x80 #x84
	#x8c #x88 #x88 #x8c #x88 #x8c #x8c #x88
	#xa0 #xa4 #xa4 #xa0 #xa4 #xa0 #xa0 #xa4
	#xac #xa8 #xa8 #xac #xa8 #xac #xac #xa8
	#xa4 #xa0 #xa0 #xa4 #xa0 #xa4 #xa4 #xa0
	#xa8 #xac #xac #xa8 #xac #xa8 #xa8 #xac))
