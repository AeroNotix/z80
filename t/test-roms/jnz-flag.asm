    .ORG 0000H                  ; offset rom
START:
    LD A, 1                     ; put 1 in the A register
    INC A                       ; increment a
    JP NZ, END                  ; if zero flag not set, jump to end
    JP START                    ; should never get here. If flags don't work, infinite loop
END:
    LD A, 255                   ; put 255 in the A register for our test to assert
    HALT
