    .ORG 0000H                  ; offset rom
START:
    LD A, 1                     ; put 1 in the A register
    DEC A                       ; decrement a
    JP Z, END                   ; if zero flag set, jump to end
    JP START                    ; should never get here. If flags don't work, infinite loop
END:
    LD A, 255                   ; put 255 in the A register for our test to assert
    HALT
