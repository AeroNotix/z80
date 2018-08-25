;;;  Simple BIOS providing input/output routines
#target bin
#code _BIOS,0x0000
    .ORG 0x0005
BIOS:
    LD A, C
    CP 0x2
    JP Z, OUTPUT_BYTE
    CP 0x9
    JP Z, OUTPUT_STRING
    RET
OUTPUT_BYTE:
    LD A, E
    OUT 0x0, A
    RET
OUTPUT_STRING:
    LD A, (DE)
    CP '$'
    RET Z
    OUT 0x0, A
    INC DE
    JP OUTPUT_STRING
