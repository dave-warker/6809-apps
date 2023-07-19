#!/usr/bin/make
# Makefile for 6809 ALF by Dave Warker
# (Alf's Like Forth)

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

alf: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) $(ASM6809_FLAGS) $(SRC_PATH)/alf.asm --output=alf.hex --list=alf.lst

debug: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=DEBUG $(ASM6809_FLAGS) $(SRC_PATH)/alf.asm --output=debug.hex --list=debug.lst

clean:
	rm *\.hex *\.lst
