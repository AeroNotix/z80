#include "cpm-bios.asm"
	.ORG	100H
    LD SP, 65535
	LD	DE, TESTMSG                ; bios routine requires DE to be filled with the string we want to print
	LD	C, 0x9                     ; in the bios register C specifies the operation
	CALL BIOS                      ; call the bios routine
    HALT

TESTMSG:	DB	'THISISATEST',10,13,'$'
