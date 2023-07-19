# Dave's Demon Debugger for the Motorola 6809

Demon is one third of my mini-suite of 6809 apps including: the Syl LISP interpreter,
Alf threaded interpreter and Demon for debugging.

A reasonably complete ROM resident 6809 debugger that fits in 8KB and includes an inline assembler,
disassembler and instruction tracer as well as Intel Hex and Motorola S Record I/O.

Demon is distributed under MIT license rules (see `license.txt`)

## A Little History

Way back in the mists of time (the 1970s-80s) when I was doing Z80 and 6502 assembly language programming
I really, really wanted to work with the Motorola 6809 processor but never had a chance. In 2022 I discovered
[Grant's 6-Chip 6809 Computer (aka G6CC)](http://searle.x10host.com/6809/Simple6809.html) and maybe $20 in Aliexpress
orders later I had a working basic 6809 computer with a serial port interface (rather than muck with RS232
I omitted the level converter chip and instead wired up a FTDI TTL level header so I can use that as a USB
serial interface I could plug into my iMac. So technically it's a 5 chip computer.)

After playing with the ExBasRom 16K Basic a bit I wanted to dig deeper into the 6809 architecture and decided
to port an old Z80 debugger I wrote called Demon to the 6809. One complete rewrite later and I had Demon for
the 6809. And yeah, the 6809 is as cool as I'd hoped it would be.

## The Hardware

G6CC's memory map is 32KB of RAM at $0000-$7FFF, 16KB of EPROM at $C000-$FFFF and a serial port at $A000-$A001
(it's actually loosely mapped between $A000-$BFFF.) Demon resides in EPROM at $E000-$FFFF so it has access to
the 6809 hardware vectors at $FFF0-$FFFF. It also uses RAM from $7F00-$7FFF for stack and private variables.
EPROM from $C000-$DFFF is reserved for other uses, such as my Alf language interpreter elsewhere in this repo.

Demon uses the hardware vectors at $FFFE (Reset) and $FFFC (SWI) for reset and breakpoints respectively. The
remaining interrupt vectors including SWI2 and SWI3 are not used.

The file `g6cc.lib` contains assembler equates for all of the G6CC hardware.

There are plenty of docs out there for the 6809 instruction set but I really liked the **Motorola 6809 and
Hitachi 6309 Programmer's Reference** Copyright 2009 by Darren Atkinson, particularly the summaries
near the end.

## The Debugger

Demon gains control on power-up and reset and displays a version banner and command prompt. Details of the
commands are in `demon-cmds.txt`.

## Building Demon

I built Demon using the **lwasm** assembler, part of [LWTools](http://www.lwtools.ca). You can use a different
assembler but you'll probably have to change my use of macros and local symbols as they might be unique
to lwasm.

A sample Makefile is included. `make demon` builds a release Intel Hex file named `demon.hex`. Use
`make debug` to build a debug version that resides in RAM from $1000-$2FFF for testing. And yes you can debug
Demon using Demon with the exception of the breakpoint code as that needs the hardware SWI vector.

It should be easy to port Demon to other 6809 hardware with just changes to the memory map (in `demon.asm`)
and replace the console I/O routines (in `io.asm`) with whatever is appropriate to your system.

I think that the assembler, disassembler and tracer are correct but if you run into any errors in the
way they handle 6809 code please let me know.

