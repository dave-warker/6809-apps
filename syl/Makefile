#!/usr/bin/make
# Makefile for 6809 SYL LISP by Dave Warker
# (As in Sylvester)

DPROM_PORT = /dev/usbserial-DHBCb116L16
DPROM_BAUD = 19200

G6CC_PORT = /dev/usbserial-FTD5QISJ
G6CC_BAUD = 115200

#

#BIN_PATH = ../../bin
BIN_PATH = /opt/pkg/bin
SRC_PATH = ./src

ASM6809 = $(BIN_PATH)/lwasm

#

all: syl syl-pg09

syl: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --format=ihex $(SRC_PATH)/syl.asm --output=syl.hex --list=syl.lst

syl-pg09: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=PG09 --define=PG09_BROM --format=raw $(SRC_PATH)/syl.asm --output=syl-pg09.brom --list=syl-pg09-brom.lst
	$(ASM6809) --define=PG09 --define=PG09_HBRAM --format=srec $(SRC_PATH)/syl.asm --output=syl-pg09.s19 --list=syl-pg09-hbram.lst

debug: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=DEBUG $(ASM6809_FLAGS) $(SRC_PATH)/syl.asm --output=debug.hex --list=debug.lst

clean:
	rm *\.hex *\.lst
