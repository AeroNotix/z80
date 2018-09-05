    .ORG 0000H
    LD IX, 65535
    LD IY, DATA
    LD A, (IY+0)
    LD B, (IY+1)
    HALT
DATA:
    db 'A'
    db 'B'
