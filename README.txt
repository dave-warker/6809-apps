# Dave's 6809 Apps (Demon, Alf & Syl)

Demon is my debugger, Alf is a threaded language like FORTH and Syl is a LISP interpreter. Alf and Syl
are essentially proof-of-concepts for how well a threaded language and basic LISP interpreter would
fit on a 6809 processor.

All of these were written for Grant's 6 Chip 6809 Computer (see http://searle.x10host.com/6809/Simple6809.html).
My only changes were to use an FTDI serial connection instead of an actual RS232 port and I used a 27256
EPROM instead of a 27128 so I could have two 16KB EPROM banks.

My main 16KB EPROM image for the G6CC includes: Alf ($C000-$CFFF), Syl ($D000-4DFFF) and Demon ($E000-$FFFF.)

## Building the EPROM

Assuming you've been able to install the **lwasm** assembler ([LWTools](http://www.lwtools.ca)) there is
a Makefile in this directory to create the Intel HEX EPROM image with a simple `make eprom`. You should
see `eprom.hex` which is ready to burn in your EPROM. For a 27128 just burn it starting at offset $0000
in the EPROM. If you're using 27256's like me pick that range or $4000-$7FFF in the EPROM to use
the alternate bank.

If you don't have a 27256 EPROM burner, I built one with an ESP8266 that should also be in my repo
(or bug me and Ill send the code + schematic.)

If you do something cool with this please let me know (dave@warker.com)
