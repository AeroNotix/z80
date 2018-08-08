cd ;ROM programs for Z80 computer
;Version 4 2/17/03
	.ORG 0000H
	JP 0020H	;Jump to start of programs
	.ORG 0020H
	IN A,(0)	;Jump to address on input ports
	LD L,A
	IN A,(1)
	LD H,A
	JP (HL)
;; LOOP1	IN A,(0)	;Simple port reflector
;; 	OUT (0),A
;; 	IN A,(1)
;; 	OUT (1),A
;; 	JP LOOP1
;; 	LD A,00H	;Simple counter
;; LOOP2	OUT (0),A
;; 	INC A
;; 	JP LOOP2
;; 	LD L,0		;Count to 1,000,000
;; 	LD H,0
;; LOOP4	LD A,16
;; LOOP3	DEC A
;; 	JP NZ,LOOP3
;; 	INC HL
;; 	LD A,L
;; 	OUT (0),A
;; 	LD A,H
;; 	OUT (1),A
;; 	JP LOOP4
;; 	LD HL,0800H	;Load program in RAM from input port 0
;; LOOP5	IN A,(1)	;Look at input port 1, bits 0 and 7
;; 	AND 81H		;Loop until a switch is closed
;; 	JP Z,LOOP5
;; 	LD B,80H
;; DEBNC1	DJNZ DEBNC1	;Debounce loop for switch closure
;; 	AND 80H		;Look at input port 1, bit 7 switch
;; 	JP NZ,0800H	;If switch is closed, jump to start of RAM
;; 	IN A,(0)	;Switch open, get byte from input port 0
;; 	OUT (0),A	;Display byte on output port 0
;; 	LD (HL),A	;Store byte in RAM
;; 	INC HL		;Point to next location in RAM
;; 	LD A,0FFH	;Turn all output port 1 lights on
;; 	OUT (1),A
;; LOOP6	IN A,(1)	;Look at input port 1, bit 0 switch
;; 	AND 01H
;; 	JP NZ,LOOP6	;Loop until switch for bit 0 opens
;; 	LD B,80H
;; DEBNC2	DJNZ DEBNC2	;Debounce loop for switch opening
;; 	LD A,0
;; 	OUT (1),A	;Turn off port 1 lights
;; 	JP LOOP5	;Start over
;; 	.END
