; "alf.asm" - main file for 6809 ALF (ALF's Like Forth) by Dave Warker
; Dave's latest attempt at a FORTH-like language.
; 2022-oct-14 dww  created.
;
; 6809 REGISTER USAGE:
; Y = (IP) Interpreter Pointer
; U = (RP) Return stack Pointer
; S = (PP) Parameter stack Pointer
; D,X are scratch registers for now
;
; MEMORY MAP (ON THE G6CC HARDWARE ROM)
; $DFFF +------------------------------+
;       | Alf R/O Kernel               |
; $C0FF +------------------------------+
;       | Direct Page Accessible Code  |
;       | & Entry Vectors              |
; $C000 +------------------------------+ <- DP register
;       :                              :
;       :                              :
; $7FFF +------------------------------+ <- end of RAM
;       | Reserved for Demon Debugger  |
; $7F00 +------------------------------+ <- initial RP (reg U)
;       | (RS) Return Stack            |
; $7E00 +------------------------------+ <- initial PP (reg S)
;       | (PS) Parameter Stack         |
;       +..............................+ <- current PP
;       : free space                   :
;       +..............................+ <- Dict Pointer (DP)
;       | User Defined Words           |
;       +------------------------------+
;       | Static Kernel Vars           |
; $0000 +------------------------------+
;
; NOTE: For debug builds kernel $C000-$DFFF is moved to $5000-$6FFF,
; RS to $4F00-$4FFF, and initial PS to $4F00.
;
; SYMBOL CONVENTIONS:
; Where needed, defined words have their code address labeled the same as the defined string.
; Where char not valid is a symbol are used I'll substitute something similar (e.g. foo? -> fooq).
; Internal words and labels not intended for the end user contain a '$'. Subroutines (as opposed
; to word list) symbols end with a '_'. Words that return a pointer end with a '&'. Words that
; set a specific state end with a '!'. Predicates that return true/false end with a '?' ('q' in label).
; No guarantee it will be 100% consistent but I'll try.
;
; STRINGS:
; Assuming most of our strings will be fairly short I'm using the old MacOS "pascal"
; string contention of length byte followed by the content bytes. Longer strings will
; have to be handled with a length word or some other contention.

			pragma	6809,dollarnotlocal,cescapes

			pragma	nolist
			IFDEF PG09
			include "pg09.lib"
			ELSE
			include	"g6cc.lib"
			ENDIF
			include "ascii.lib"
			pragma	list

			include	"equ.asm"
			setdp	ALF_DP_ORG/256

;@@@
;@@@ STATIC KERNEL VARS (READ/WRITE)
;@@@
			org	ALF_SKV_ORG
			include	"skv.asm"
alf_dp$		equ		*						;initial dictionary pointer

;@@@
;@@@ KERNEL (READ/ONLY)
;@@@
			org		ALF_KER_ORG
			include	"ker.asm"
			include	"repl.asm"
			include	"con.asm"
			include	"bops.asm"
			include	"cops.asm"
			include	"expr.asm"
			include	"defs.asm"
			include	"strs.asm"
			include	"vars.asm"
;
alf_end$	equ		*						;end of all code
alf_defs$	equ		_head					;last defined word

			end
