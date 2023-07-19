#!/usr/bin/make
# Makefile for 6809 Demon by Dave Warker
# Uses LWTOOLS assembler for 6809 (http://www.lwtools.ca)
# Last modified 2022-nov-09 by DWW

DPROM_PORT = /dev/usbserial-DHBCb116L16
DPROM_BAUD = 19200

G6CC_PORT = /dev/usbserial-FTD5QISJ
G6CC_BAUD = 115200

#

BIN_PATH = ../../bin
SRC_PATH = ./src

ASM6809 = $(BIN_PATH)/lwasm
ASM6809_FLAGS = --format=ihex

# "make demon" for EPROM version at $E000, "make debug" for RAM debug version at $1000

demon: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) $(ASM6809_FLAGS) $(SRC_PATH)/demon.asm --output=demon.hex --list=demon.lst

debug: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=DEBUG $(ASM6809_FLAGS) $(SRC_PATH)/demon.asm --output=debug.hex --list=debug.lst

clean:
	rm *\.hex *\.lst
