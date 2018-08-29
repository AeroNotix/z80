    .ORG 0000H
    JP START
DATA:
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
    DB  $56
START:
    LD HL, DATA
    LD DE, DATA2
    LD BC, 20
    LDIR
    HALT
DATA2:
