#!/usr/bin/make
# Makefile for 6809 ALF by Dave Warker
# (Alf's Like Forth)

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

all: alf alf-pg09

alf: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --format=ihex $(SRC_PATH)/alf.asm --output=alf.hex --list=alf.lst

alf-pg09: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=PG09 --define=PG09_BROM --format=raw $(SRC_PATH)/alf.asm --output=alf-pg09.brom --list=alf-pg09-brom.lst
	$(ASM6809) --define=PG09 --define=PG09_HBRAM --format=srec $(SRC_PATH)/alf.asm --output=alf-pg09.s19 --list=alf-pg09-hbram.lst

debug: $(SRC_PATH)/*.asm $(SRC_PATH)/*.lib
	$(ASM6809) --define=DEBUG $(ASM6809_FLAGS) $(SRC_PATH)/alf.asm --output=debug.hex --list=debug.lst

clean:
	rm *\.hex *\.lst
