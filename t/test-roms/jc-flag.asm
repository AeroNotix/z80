    .ORG 0000H                  ; offset rom
START:
    LD A, 255                   ; put 1 in the A register
    INC A                       ; increment a
    JP C, END                   ; if carry flag set, jump to end
    JP START                    ; should never get here. If flags don't work, infinite loop
END:
    LD A, 66                   ; put 66 in the A register for our test to assert
    HALT
