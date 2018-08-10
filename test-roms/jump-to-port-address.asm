    .ORG 0000H
	JP 0020H                    ; Jump to 0020h. Useful to test raw JP opcodes
	.ORG 0020H                  ;
	IN A,(0)                    ; Jump to address on input ports.
	LD L,A                      ;   For this test likely write a peripheral
	IN A,(1)                    ;   connected to PORT 0,1 giving an address within this program.
	LD H,A
	JP (HL)
    .ORG 0050H                  ; Offset the ROM again to give a known address for the next instruction
    LD A, 123                   ; Write 123 into A
