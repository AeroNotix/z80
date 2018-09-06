    .ORG 0000H
    LD IX, 65535
    LD IY, DATA
    LD A, (IY)
    LD B, (IY+1)
    LD (IY+3), A
    LD (IY+4), B
    HALT
DATA:
    db 'A'
    db 'B'
