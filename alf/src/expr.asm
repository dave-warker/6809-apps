; "expr.asm" - numeric expression words for the Alf nucleus
; 2022-nov-18 dww  created

;@@@
;@@@ NUMERIC PRINTING
;@@@

; >>> Print (display) signed decimal
; CONSUMES: 0,PP = signed number
			DEF		2,"pr"
			puls	d							;D = number
			bsr		prsign_						;display sign if necessary
			bra		@pru						;display unsigned
;
prsign_		tsta
			bpl		@ret						;if it's a positive number
			pshs	d
			ldb		#'-'						;else display sign
			jsr		prc_
			puls	d
			jsr		neg16_						;convert to positive
@ret		rts
;
; >>> Print (display) unsigned decimal
; CONSUMES: 0,PP = unsigned number
			DEF		3,"pru"
			puls	d							;D = unsigned number
@pru		ldx		#10							;display in base 10
			bsr		prnum_
			NEXT

; >>> Print (display) an unsigned number in arbitrary base
; PASSED: D = number to display, X = base
prnum_		stx		base$						;save base
			bsr		@prnum
			rts
;
@prnum		cmpd	base$						;compare to base
			blo		@prdig						;if this is most sig digit
			ldx		base$						;else generate next digit
			jsr		div16u_						;B = digit, X = remaining digits
			pshs	b							;save to print later
			tfr		x,d							;D = remaining digits
			bsr		@prnum						;recurse to print them first
			puls	b							;now we print ours
@prdig		addb	#'0'						;assume decimal digit
			cmpb	#'9'
			bls		@prc						;if it is
			addb	#'A'-10-'0'					;else use alphas
@prc		jmp		prc_

; >>> Print (display) an unsigned number in hexadecimal
; CONSUMES: 0,PP = unsigned number
			DEF		3,"prx"
			ldb		#'0'						;display prefix
			jsr		prc_
			ldb		#'x'
			jsr		prc_
			puls	d							;D = number
			ldx		#16							;X = base
			bsr		prnum_
			NEXT

; >>> Print (display) an unsigned number in binary
; CONSUMES: 0,PP = unsigned number
			DEF		3,"prb"
			ldb		#'0'						;display prefix
			jsr		prc_
			ldb		#'b'
			jsr		prc_
			puls	d							;D = number
			ldx		#2							;X = base
			bsr		prnum_
			NEXT

;@@@
;@@@ BASIC MATH
;@@@

; >>> Add
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 + n2
			DEF		1,"+"
add			puls	d
@add		addd	,s
			std		,s
			NEXT
;
; >>> Subtract 1
; CONSUMES: 0,PP = n
; PRODUCES:	0,PP = n-1
			DEF		2,"1-"
sub1		ldd		#-1
			bra		@add
;
; >>> Add 1
			DEF		2,"1+"
add1		ldd		#1
			bra		@add

; >>> Subtract
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 - n2
			DEF		1,"-"
			ldd		2,s
			subd	,s++
			std		,s
			NEXT

; >>> Unsigned multiply
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 * n2
			DEF		2,"u*"
			puls	d,x							;D = n2, X = n1
			bsr		mul16u_						;X = n1 * n2
			pshs	x
			NEXT

; Unsigned 16 bit multiply
; PASSED:  D = n1, X = n2
; RETURNS: X,D = low 16 bits of unsigned product
mul16u_		pshs	d,x
			tfr		x,a
			mul
			exg		d,x
			ldb		1,s
			mul
			tfr		b,a
			clrb
			leax	d,x
			ldb		,s
			lda		3,s
			mul
			tfr		b,a
			clrb
			leax	d,x
			tfr		x,d
			leas	4,s
			rts

; >>> Signed multiply ($$$ NEEDED?)
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 * n2
			DEF		1,"*"
			puls	d,x							;D = n2, X = n1
			bsr		signs$_						;N flag is sign of result
			pshs	cc
			bsr		mul16u_						;D = unsigned product
			puls	cc
			bcc		@done						;if positive result
			bsr		neg16_						;else negative result
@done		pshs	d
			NEXT

; >>> Adjust signs of binary op operands
; PASSED:  D = n2, X = n1
; RETURNS: D,X = absolute values, CS if result should be negative else CC
signs$_		clr		,-s							;0,s will tell result sign
			tsta
			bpl		@n2							;if n1 is positive
			bsr		neg16_						;else make it positive
			inc		,s							;and flip sign bit
@n2			exg		d,x							;D = n1, X = n2
			tsta
			bpl		@res						;if already positive
			bsr		neg16_						;else make it so
			inc		,s							;and flip sign
@res		exg		d,x							;D = n2, X = n1
			lsr		,s							;CS = neg result else CC
			leas	1,s
			rts

; >>> Negate value
; CONSUMES: 0,PP = number
; PRODUCES: 0,PP = -number
			DEF		3,"neg"
			ldd		,s							;D = number
			bsr		neg16_						;D = -D
			std		,s
			NEXT

; >>> Negate 16 bit value
; PASSED:  D = value
; RETURNS: D = -value
neg16_		coma
			comb
			addd	#1							;D = -D
			rts

; >>> Unsigned divide
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 / n2
			DEF		2,"u/"
			puls	d,x							;D = n2, X = n1
			exg		d,x							;D = n1, X = n2
			bsr		div16u_						;D = remainder, X = quotient
			pshs	x
			NEXT

; >>> Signed divide
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 / n2
			DEF		1,"/"
			puls	d,x							;D = n2, X = n1
			exg		d,x							;D = n1, X = n2
			bsr		signs$_						;N flag is sign of result
			pshs	cc
			bsr		div16u_						;D = remainder, X = quotient
			tfr		x,d
			puls	cc
			bcc		@done						;if positive result
			bsr		neg16_						;else negative result
@done		pshs	d
			NEXT

; >>> Unsigned modulo
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 % n2
			DEF		2,"u%"
			puls	d,x							;D = n2, X = n1
			exg		d,x							;D = n1, X = n2
			bsr		div16u_						;D = remainder, X = quotient
			pshs	d
			NEXT

; >>> Signed modulo
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 % n2
			DEF		1,"%"
			puls	d,x							;D = n2, X = n1
			exg		d,x							;D = n1, X = n2
			bsr		signs$_						;N flag is sign of result
			pshs	cc
			bsr		div16u_						;D = remainder, X = quotient
			puls	cc
			bcc		@done						;if positive result
			bsr		neg16_						;else negative result
@done		pshs	d
			NEXT

; Unsigned 16 bit divide
; PASSED:  D = dividend, X = divisor
; RETURNS: D = remainder, X = quotient
div16u_		pshs	x,d							;0,s = dividend, 2,s = divisor
			clra								;D = accumulator (inited to 0)
			clrb
;
			ldx		#16
@lp			lsl		1,s							; acc,dividend <<= 1
			rol		0,s
			rolb
			rola
			cmpd	2,s
			blo		@nxt						;if next result bit is 0
			subd	2,s							;else actually subtract it
			inc		1,s							;and set result bit
@nxt		leax	-1,x
			bne		@lp							;if not done yet
;
			puls	x							;D = remainder, X = quotient
			leas	2,s
			rts

; >>> Bitwise And
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 & n2
			DEF		1,"&"
			puls	d
			anda	,s
			andb	1,s
@save		std		,s
			NEXT
;
; >>> Bitwise Or
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 | n2
			DEF		1,"|"
			puls	d
			ora		,s
			orb		1,s
			bra		@save
;
; >>> Bitwise Exclusive Or
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = n1 ^ n2
			DEF		1,"^"
			puls	d
			eora	,s
			eorb	1,s
			bra		@save

; >>> Shift Left one bit
			DEF		2,"<<"
			lsl		1,s
			rol		,s
			NEXT

; >>> Signed Shift Right one bit
			DEF		2,">>"
			asr		,s
			ror		1,s
			NEXT

; >>> Unsigned Shift Right one bit
			DEF		3,"u>>"
			lsr		,s
			ror		1,s
			NEXT

;@@@
;@@@ COMPARISONS
;@@@

; >>> Equal
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 == n2 else 0
			DEF		2,"=="
equal		puls	d
			cmpd	,s++
			lbeq	push1							;if n1 == n2
@false		lbra	push0							;else n1 != n2
;
; >>> Not Equals
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 != n2 else 0
			DEF		2,"!="
			puls	d
			cmpd	,s++
			lbne	push1							;if n1 != n2
			bra		@false							;else n1 == n2
;
; >>> Signed Less Than
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		1,"<"
			puls	d
			cmpd	,s++
			lbgt	push1							;if n1 < n2
			bra		@false
;
; >>> Signed Greater Than
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		1,">"
			puls	d
			cmpd	,s++
			lblt	push1							;if n1 > n2
			bra		@false
;
; >>> Signed Less Than or Equal
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		2,"<="
			puls	d
			cmpd	,s++
			lbge	push1							;if n1 <= n2
			bra		@false
;
; >>> Signed Greater Than or Equal
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		2,">="
gteq		puls	d
			cmpd	,s++
			lble	push1							;if n1 >= n2
			bra		@false
;
; >>> Unsigned Less Than
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		2,"u<"
			puls	d
			cmpd	,s++
			lbhi	push1							;if n1 < n2
			bra		@false
;
; >>> Unsigned Greater Than
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		2,"u>"
			puls	d
			cmpd	,s++
			lblo	push1							;if n1 > n2
			bra		@false

; >>> Unsigned Less Than or Equal
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		3,"u<="
			puls	d
			cmpd	,s++
			lbhs	push1							;if n1 <= n2
@false		lbra	push0
;
; >>> Unsigned Greater Than
; CONSUMES: 0,PP = n2, 2,PP = n1
; PRODUCES: 0,PP = 1 if n1 < n2 else 0
			DEF		3,"u>="
			puls	d
			cmpd	,s++
			lbls	push1							;if n1 >= n2
			bra		@false

; >>> Signed minimum
; <n1> <n2> min -> smaller of the two
			DEF		3,"min"
			puls	d								;D = n2
			cmpd	,s
			bgt		@n1								;if n1 is smaller
@n2			std		,s								;else n2 is
@n1			NEXT
;
; >>> Unsigned minimum
; <n1> <n2> umin -> smaller of the two
			DEF		4,"umin"
umin		puls	d
			cmpd	,s
			bhi		@n1
			bra		@n2
;
; >>> Signed maximum
; <n1> <n2> umin -> larger of the two
			DEF		3,"max"
			puls	d
			cmpd	,s
			blt		@n1
			bra		@n2
;
; >>> Unsigned maximum
; <n1> <n2> umin -> larger of the two
			DEF		4,"umax"
			puls	d
			cmpd	,s
			blo		@n1
			bra		@n2
