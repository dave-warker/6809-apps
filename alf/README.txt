# Dave's Alf for the Motorola 6809 (Alf's Like FORTH)

Alf is one third of my mini-suite of 6809 apps including: the Demon debugger, Syl LISP interpreter
and Alf for threaded code.

I wanted to see how efficient the 6809 would be running a threaded language like FORTH (answer: really,
really efficient) and Alf is the result. It's a more or less complete FORTH-a-like package. Some of the
word names are different in some cases because I never liked what FORTH used. And they differ in others
(control structures) because I thought I had a better solution. In any case with some judicious renaming
you can probably get pretty close to a standard FORTH set of definitions.

One of the main things that's missing is some variant of FORTH's <BUILDS DOES> words to define new
classes of words. Not because it's hard, I just didn't need it in my experiments.

There are no assembler words defined. I started it but given the sheer number of unique mnemonics
the 6809 features and the variousd addressing mode it was NOT going to fit in my space constraints
(I wanted Alf to fit in a 4KB block in the EPROM for my little G6CC 6809 computer and it does, barely.)

Loading from the console serial port is supported via the LOAD word. The 6809 can't keep up with a
115kbps text stream and compile at the same time so the LOAD word causes Alf to turn OFF the CTS
line except while it is filling the line buffer, which it can keep up with, so make sure you've
configured your terminal program to honor CTS flow control. You might also want to send CR/LFs as
line ends or put a short (few MS) delay after each line otherwise the first character of the next
line might get lost. Put an EOF word at the end of your file to switch back to normal input.

The 6809 is a really good fit for a threaded language. The "inner interpreter" to execute the next
word in the current definition is a mere 2 BYTES (jmp [,y++]) and two stack pointers means S can be
used for the parameter stack and U for the return stack. Cool beans.

Porting Alf to other hardware should be pretty easy. Replace the RAW (HARDWARE) CONSOLE I/O code
and the bottom of `con.asm` with whatever's appropriate for your hardware and you should be good.
If your hardware supports actual storage (SD or CF cards say) then you'll want to replace/augment
LOAD with something more elaborate.

Alf is distributed under MIT license rules (see `license.txt`)

## Building Alf

I wrote Alf using the **lwasm** assembler, part of [LWTools](http://www.lwtools.ca). You can use a different
assembler but you'll probably have to change my use of macros and local symbols as they might be unique
to lwasm.

A sample Makefile is included. `make alf` builds a release Intel Hex file named `alf.hex`. Use
`make debug` to build a debug version that resides in RAM from $5000-$5FFF for testing. Check
the memory map definitions at the top of `equ.asm` to change it to whatever fits your need.
