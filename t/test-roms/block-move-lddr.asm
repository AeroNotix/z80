    .ORG 0000H
    JP START
DATA:
    DB  $56                     ; $56 is any old data
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
    LD HL, START - 1
    LD DE, DATA2 + (START + DATA)
    LD BC, START - DATA
    LDIR
    HALT
DATA2:
