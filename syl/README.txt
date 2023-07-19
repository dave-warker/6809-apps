# Dave's Syl for the Motorola 6809 (a LISP Interpreter)

Syl is one third of my mini-suite of 6809 apps including: the Demon debugger, Alf threaded interpreter
and the Syl LISP interpreter.

As to why Syl exists, mainly just because LISP seems like a nice reasonably compact language that I've
never written before and I wanted to see how the 6809 could handle it (spoiler: pretty well. Yeah, I'm
kind of a 6809 fanboy.)

It's a more or less complete LISP interpreter. I started with the LISP 1.5 user guide with changes where
I thought I could be more efficient (looking at you `attributes`) and some omissions where I didn't need
features for what I wanted to do. Similarly to Alf, Syl is too slow to handle 115kbps console input to
load files via the console so I've provided the (LOAD) function to enable CTS handshaking for console
input. Make sure your terminal program is configured to honor it and maybe also add a 10ms or so delay
after each line so you don't lose the first character of the next.

A basic "stop the world" garbage collector for CONS cells is provided. I do NOT perform GC on ATOMs as
it was more involved than I wanted to deal with, especially given the 4KB size limit I imposed so Syl
would fit in my G6CC EPROM. Some tips on how it might be done are provided in the source code. Please
let me know if you decide to try it.

Porting Syl to other hardware is possible with maybe a bit of work on the memory map as defined at the
top of `syl.asm`. I took advantage of the precise memory map of Grant's 6 Chip 6809 Computer to
efficiently encode the data types I needed (ATOMs, CONS cells, Built-Ins and Integers) in 16 bits.
Sadly that means I only support 15 bit signed integers but that was sufficient for me. If you switch
to 5 byte CONS cells and use the 5th byte to encode type info (and the GC LIVE flag) you can support
more memory and bigger Ints, but that will also require lots of changes to code that handles CARs and
CDRs (CARs especially as I use the 6809's extra indirection addressing mode to get a "free" CAR in
some cases.)

Other than that, if you replace the hardware I/O routines at the bottom of `con.asm` with whatever works
for your hardware you'll be good to go. Again, if your hardware supports actual storage (SD or CF cards
say) then you'll want to replace/augment LOAD with something more elaborate.

Syl is distributed under MIT license rules (see `license.txt`)

## Building Syl

I wrote Syl using the **lwasm** assembler, part of [LWTools](http://www.lwtools.ca). You can use a different
assembler but you'll probably have to change my use of macros and local symbols as they might be unique
to lwasm.

A sample Makefile is included. `make syl` builds a release Intel Hex file named `syl.hex`. Use
`make debug` to build a debug version that resides in RAM from $7000-$7EFF for testing.
