    .ORG 0000H
    LD HL, S
    LD B, 6
    OTIR
    LD HL, S2-1
    LD B, 6
    OTDR
    LD HL, S
    LD B, 1
    OUTI
    LD HL, S
    LD B, 1
    OUTD
    HALT
S:
    db 'H'
    db 'E'
    db 'L'
    db 'L'
    db 'O'
    db '!'
S2:
