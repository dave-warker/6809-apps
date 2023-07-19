#!/usr/bin/make
# Makefile for 6809 Demon, Alf & Syl EPROM
# Last modified 2023-mar-16 by DWW

eprom:
	cd alf; make alf; cd ..
	cd demon; make demon; cd ..
	cd syl; make syl; cd ..
	sed '$$d' alf/alf.hex >eprom.hex; sed '$$d' syl/syl.hex >>eprom.hex; cat demon/demon.hex >>eprom.hex

clean:
	rm eprom.hex; cd alf; make clean; cd ../demon; make clean; cd ../syl; make clean; cd ..
