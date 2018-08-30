(in-package :z80)

(defclass instruction-table ()
  ((prefixed-p :initarg :prefixed-p :accessor prefixed-p :initform nil)
   (instructions :initarg :instructions
                 :accessor instructions
                 :initform (error "An instruction set is required for an instruction table"))))

(defmethod next-instruction ((it instruction-table) opcode)
  (elt (instructions it) opcode))

(defmethod instruction-size ((it instruction-table) opcode)
  (if (prefixed-p it)
      (1- (size opcode))
      (size opcode)))

(defparameter unprefixed-table
  (make-instance 'instruction-table
                 :prefixed-p nil
                 :instructions
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
                              ))))

(defparameter ed-prefix-table
  (make-instance 'instruction-table
                 :prefixed-p t
                 :instructions
                 (make-array 256 :initial-contents
                             ;; unfuck this
                             (list ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined in-r-c out-c-r sbc-hl-rr
                                   ld-indirect-nn-rr neg reti-retn im-n ld-i-a-ld-r-a in-r-c out-c-r adc-hl-rr
                                   ld-rr-indirect-nn neg reti-retn im-n ld-i-a-ld-r-a in-r-c out-c-r sbc-hl-rr
                                   ld-indirect-nn-rr neg reti-retn im-n ld-a-i-ld-a-r in-r-c out-c-r adc-hl-rr
                                   ld-rr-indirect-nn neg reti-retn im-n ld-a-i-ld-a-r in-r-c out-c-r sbc-hl-rr
                                   ld-indirect-nn-rr neg reti-retn im-n rld-rrd in-r-c out-c-r adc-hl-rr
                                   ld-rr-indirect-nn neg reti-retn im-n rld-rrd in-r-c out-c-r sbc-hl-rr
                                   ld-indirect-nn-rr neg reti-retn im-n ed-undefined in-r-c out-c-r adc-hl-rr
                                   ld-rr-indirect-nn neg reti-retn im-n ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ldi cpi ini outi ed-undefined ed-undefined ed-undefined ed-undefined ldd cpd
                                   ind outd ed-undefined ed-undefined ed-undefined ed-undefined ldir cpir inir
                                   otir ed-undefined ed-undefined ed-undefined ed-undefined lddr cpdr indr otdr
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined ed-undefined
                                   ed-undefined ed-undefined))))

(defparameter cb-prefix-table
  (make-instance 'instruction-table
                 :prefixed-p t
                 :instructions
                 (make-array 256 :initial-contents
                             (list

	                          rlc-r
	                          rlc-r
	                          rlc-r
	                          rlc-r
	                          rlc-r
	                          rlc-r
	                          rlc-indirect-hl
	                          rlc-r

	                          rrc-r
	                          rrc-r
	                          rrc-r
	                          rrc-r
	                          rrc-r
	                          rrc-r
	                          rrc-indirect-hl
	                          rrc-r

	                          rl-r
	                          rl-r
	                          rl-r
	                          rl-r
	                          rl-r
	                          rl-r
	                          rl-indirect-hl
	                          rl-r

	                          rr-r
	                          rr-r
	                          rr-r
	                          rr-r
	                          rr-r
	                          rr-r
	                          rr-indirect-hl
	                          rr-r

	                          sla-r
	                          sla-r
	                          sla-r
	                          sla-r
	                          sla-r
	                          sla-r
	                          sla-indirect-hl
	                          sla-r

	                          sra-r
	                          sra-r
	                          sra-r
	                          sra-r
	                          sra-r
	                          sra-r
	                          sra-indirect-hl
	                          sra-r

	                          sll-r
	                          sll-r
	                          sll-r
	                          sll-r
	                          sll-r
	                          sll-r
	                          sll-indirect-hl
	                          sll-r

	                          srl-r
	                          srl-r
	                          srl-r
	                          srl-r
	                          srl-r
	                          srl-r
	                          srl-indirect-hl
	                          srl-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-r
	                          bit-b-indirect-hl
	                          bit-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-r
	                          res-b-indirect-hl
	                          res-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r

	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-r
	                          set-b-indirect-hl
	                          set-b-r))))

(defparameter condition-table
  (list #'flag-nz #'flag-z #'flag-nc #'flag-c
        #'flag-po #'flag-pe #'flag-p #'flag-m))

(defun find-condition (opcode-y)
  (elt condition-table opcode-y))

(defparameter rst-table
  (list #x00 #x08 #x10 #x18 #x20 #x28 #x30 #x38))

(defun find-rst-address (opcode-y)
  (elt rst-table opcode-y))

(defparameter *instruction-table* unprefixed-table)
