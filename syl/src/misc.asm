; "misc.asm" - miscellaneous routines
; 2023-feb-19 dww  created


; ==> 16 bit multiplication
; PASSED:  D = multiplicand, X = multiplier
; RETURNS: D = X = low 16 bits of product
mul16:	pshs	d,x
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

; ==> 16 bit division
; PASSED:  D = dividend, X = divisor
; RETURNS: D = quotient, X = remainder
div16:	bsr		mod16
		exg		d,x
		rts

; ==> 16 Bit modulo
; PASSED:  D = dividend, X = divisor
; RETURNS: D = remainder, X = quotient
mod16:	pshs	x,d												;0,s = dividend, 2,s = divisor
		clra													;D = accumulator (inited to 0)
		clrb
;
		ldx		#16
@lp		lsl		1,s												; acc,dividend <<= 1
		rol		0,s
		rolb
		rola
		cmpd	2,s
		blo		@nxt											;if next result bit is 0
		subd	2,s												;else actually subtract it
		inc		1,s												;and set result bit
@nxt	leax	-1,x
		bne		@lp												;if not done yet
;
		puls	x												;D = remainder, X = quotient
		leas	2,s
		rts
