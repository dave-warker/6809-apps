; "bops.asm" - basic stack operations for Alf
; 2022-nov-18 dww  created

;@@@
;@@@ PARAMETER STACK MANIPULATION
;@@@

; >>> Fetch word at addr
; CONSUMES: 0,PP = address
; PRODUCES: 0,PP = word at address
			DEF		1,"@"
at			ldx		,s
			ldd		,x
@at			std		,s
			NEXT
;
; >>> Fetch byte at addr
; CONSUMES: 0,PP = address
; PRODUCES: 0,PP = byte at address
			DEF		2,".@"
bat			ldx		,s
			ldb		,x
			clra
			bra		@at

; >>> Store word at address (because '=' means assign dammit)
; CONSUMES: 0,PP = address, 2,PP = value to store there
			DEF		1,"="
store		puls	x									;X = address
			puls	d									;D = value
			std		,x
			NEXT

; >>> Store byte at address (because '=' means assign dammit)
; CONSUMES: 0,PP = address, 2,PP = byte value to store there
			DEF		2,".="
bstore		puls	x									;X = address
			puls	d									;B = value
			stb		,x
			NEXT

; >>> Duplicate top of stack
; PRODUCES: 0,PP = copy of 2,PP
			DEF		3,"dup"
dup			ldd		,s
			pshs	d
			NEXT

; >>> Duplicate top 2 stack entries
; PRODUCES: 0,PP, 2,PP = copies of 4,PP, 6,PP respectively
			DEF		4,"dup2"
dup2		ldd		,s
			ldx		2,s
			pshs	d,x
			NEXT

; >>> Swap top two stack entries
			DEF		4,"swap"
swap		ldd		,s
			ldx		2,s
			std		2,s
			stx		,s
			NEXT

; >>> Drop the top stack entry
			DEF		4,"drop"
drop		leas	2,s
			NEXT

; >>> Drop the top 2 stack entries
			DEF		5,"drop2"
drop2		leas	4,s
			NEXT

; >>> Make a copy of the 2nd stack entry
			DEF		4,"over"
over		ldd		2,s
			pshs	d
			NEXT

; >>> Rotate the 3rd stack entry up to the top
; 1 2 3 4 rot -> 1 3 4 2
			DEF		3,"rot"
rot			ldd		,s
			ldx		2,s
			std		2,s
			ldd		4,s
			stx		4,s
			std		,s
			NEXT

;@@@
;@@@ RETURN STACK MANIPULATION
;@@@

; >>> Push top of param stack to return stack
; CONSUMES: 0,PP = value to push
; PRODUCES: 0,RP = value pushed
			DEF		5,"rpush"
rpush		puls	d								;D = top of param stack
@push		pshu	d								;push on return stack
			NEXT
;
; >>> Push a 0 on the return stack
; PODUCES: 0,RP = 0
rpush0		clrb
			clra
			bra		@push

; >>> Pop return stack to top of param stack
; CONSUMES: 0,RP = value to push
; PRODUCES: 0,PP = value pushed
			DEF		4,"rpop"
rpop		pulu	d								;D = top of return stack
			pshs	d								;push on param stack
			NEXT

; >>> Swap top two return stack values
			DEF		5,"rswap"
rswap		pulu	d,x
			exg		d,x
			pshu	d,x
			NEXT

; >>> Drop top of return stack
			DEF		5,"rdrop"
rdrop		leau	2,u
			NEXT

;@@@
;@@@ MEMORY MANIPULATION
;@@@

; >>> Fill memory range with byte value
; <addr> <count> <value> mfill
			DEF		4,"mset"
mset		pshu	y
			puls	d,x,y							;B = value, X = count, Y = addr
			cmpx	#0
			beq		@done							;if nothing to do
;
@lp			stb		,y+								;fill range with value
			leax	-1,x
			bne		@lp
;
@done		pulu	y
			NEXT

; >>> Copy memory range
; Correctly handles overlapping source, dest ranges.
; <src> <count> <dest> mcpy
			DEF		4,"mcpy"
mcpy		pshu	y
			puls	y								;Y = dest addr
			puls	d,x								;X = source, D = count
			pshs	u
			bsr		mcpy_							;do the copy
			puls	u
			pulu	y
			NEXT

; >>> Subroutine to copy memory range
; Correctly handles overlapping source, dest ranges.
; PASSED:  X = source addr, Y = dest, D = count
mcpy_		pshs	y
			cmpx	,s++							;source < dest?
			blo		@dsc							;yep, have to copy descending
;
			lsra									;convert byte count to word
			rorb
			tfr		d,u								;U = vword count
			bcc		@asclp							;if count was even
			lda		,x+								;else copy odd byte
			std		,y+
;
@asclp		cmpu	#0
			beq		@done							;if all done
			ldd		,x++							;else copy a word
			std		,y++
			leau	-1,u
			bra		@asclp
;
@dsc		leax	d,x								;copy descending addresses
			leay	d,y
;
			lsra									;convert byte count to word
			rorb
			tfr		d,u								;U = word count
			bcc		@dsclp							;if byte count was even
			lda		,-x								;else copy odd byte
			sta		,-y
;
@dsclp		cmpu	#0
			beq		@done							;if all done
			ldd		,--x							;copy a word
			std		,--y
			leau	-1,u
			bra		@dsclp
;
@done		rts
