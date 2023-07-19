#!/usr/bin/make
# Makefile for 6809 SYL LISP by Dave Warker
# (As in Sylvester)

DPROM_PORT = /dev/usbserial-DHBCb116L16
DPROM_BAUD = 19200

G6CC_PORT = /dev/usbserial-FTD5QISJ
G6CC_BAUD = 115200

#

BIN_PATH = ../../bin
SRC_PATH = ./src

ASM6809 = $(BIN_PATH)/lwasm
ASM6809_FLAGS = --format=ihex

#

syl: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) $(ASM6809_FLAGS) $(SRC_PATH)/syl.asm --output=syl.hex --list=syl.lst

debug: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=DEBUG $(ASM6809_FLAGS) $(SRC_PATH)/syl.asm --output=debug.hex --list=debug.lst

clean:
	rm *\.hex *\.lst
