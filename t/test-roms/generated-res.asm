	.ORG 0000H

	LD A, 0xFF
	RES 0, A
	RES 1, A
	RES 2, A
	RES 3, A
	RES 4, A
	RES 5, A
	RES 6, A
	RES 7, A
	LD B, 0xFF
	RES 0, B
	RES 1, B
	RES 2, B
	RES 3, B
	RES 4, B
	RES 5, B
	RES 6, B
	RES 7, B
	LD C, 0xFF
	RES 0, C
	RES 1, C
	RES 2, C
	RES 3, C
	RES 4, C
	RES 5, C
	RES 6, C
	RES 7, C
	LD D, 0xFF
	RES 0, D
	RES 1, D
	RES 2, D
	RES 3, D
	RES 4, D
	RES 5, D
	RES 6, D
	RES 7, D
	LD E, 0xFF
	RES 0, E
	RES 1, E
	RES 2, E
	RES 3, E
	RES 4, E
	RES 5, E
	RES 6, E
	RES 7, E
	LD H, 0xFF
	RES 0, H
	RES 1, H
	RES 2, H
	RES 3, H
	RES 4, H
	RES 5, H
	RES 6, H
	RES 7, H
	LD L, 0xFF
	RES 0, L
	RES 1, L
	RES 2, L
	RES 3, L
	RES 4, L
	RES 5, L
	RES 6, L
	RES 7, L
	LD (HL), 0xFF
	RES 0, (HL)
	RES 1, (HL)
	RES 2, (HL)
	RES 3, (HL)
	RES 4, (HL)
	RES 5, (HL)
	RES 6, (HL)
	RES 7, (HL)
	HALT