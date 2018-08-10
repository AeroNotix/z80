.PHONY: test
ASM_DIR=test-roms
TEST_ASM_FILES:=$(wildcard ${ASM_DIR}/*.asm)
TEST_ROMS:=$(patsubst ${ASM_DIR}/%.asm,${ASM_DIR}/%.rom,${TEST_ASM_FILES})

all: test

compile: ${TEST_ROMS}

${ASM_DIR}/%.rom: ${ASM_DIR}/%.asm
	zasm -l0 -i $^ -o $@
