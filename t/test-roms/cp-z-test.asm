    .ORG 0000H
    LD A, 127
    CP 123
    JP Z, SET_FAIL_TEST
    JP SET_PASS_TEST

SET_PASS_TEST:
    LD A, 255
    HALT
SET_FAIL_TEST:
    LD A, 0
    HALT
