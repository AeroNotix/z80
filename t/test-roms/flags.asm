    .ORG 0000H
    LD BC, 65535
Loop:
    DEC BC         ; Decrease the counter
    LD A, B        ; Load one byte of the counter into the accumulator
    OR C           ; Bitwise OR with the other byte
    JR NZ, Loop    ; If Z is reset then neither B or C was zero, so repeat
    LD A, 123
    HALT
