    .ORG 0000H
    LD E, 123
    LD HL, 3E8H
    LD (HL), E
    LD A, (HL)
    HALT
