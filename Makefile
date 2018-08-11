.PHONY: test
LISP=sbcl
ASM_DIR=t/test-roms
TEST_ASM_FILES=$(wildcard ${ASM_DIR}/*.asm)
TEST_ROMS=$(patsubst ${ASM_DIR}/%.asm,${ASM_DIR}/%.rom,${TEST_ASM_FILES})
TEST_OPTS=--noinform --disable-debugger --load t/run-tests.lisp --quit

all: test

compile: ${TEST_ROMS}

${ASM_DIR}/%.rom: ${ASM_DIR}/%.asm
	zasm -l0 -i $^ -o $@

test: compile
	@${LISP} ${TEST_OPTS}
