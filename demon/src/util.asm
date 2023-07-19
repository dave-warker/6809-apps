; "util.asm" - utility functions for DeMon.
; 2022-aug-22 dww  Created.

; @@@
; @@@ Numbers
; @@@

; ==> Display 16 bit word in hex.
; PASSED:  D = number to display
disp_hex_word:
		bsr		disp_hex_byte									;A = high byte
		pshs	a
		tfr		b,a												;B = low byte
		bsr		disp_hex_byte
		puls	a
		rts

; ==> Display 8 bit byte in hex.
; PASSED:  A = byte to display
disp_hex_byte:
		pshs	a
		lsra													;high nibble
		lsra
		lsra
		lsra
		bsr		@nib
		puls	a												;low nibble
		pshs	a
		bsr		@nib
		puls	a
		rts
;
@nib	anda	#$0F											;display hex nibble in A
		adda	#'0'
		cmpa	#'9'
		bls		@pch											;if it's 0-9
		adda	#'A'-'0'-10										;else A-F
@pch	jmp		io_put_ch
;
; ==> Display signed decimal word.
; PASSED:  D = word to display
disp_dec_sword:
		tsta													;negative?
		bpl		disp_dec_word									;nope
		comb													;yep, make it positive
		coma
		addd	#1
		pshs	a												;and display minus
		lda		#'-'
		bsr		@pch
		puls	a
;
; ==> Display unsigned decimal word.
; PASSED:  D = word to display
disp_dec_word:
		pshs	d,x
;
		lda		#'&'											;so they know it's decimal
		jsr		io_put_ch
		lda		,s
;
		bsr		@disp
		puls	d,x
		rts
;
@disp	cmpd	#10
		blo		@putd											;if is most significant digit
		ldx		#10												;else generate this digit
		bsr		mod16
		pshs	b
		tfr		x,d												;recurse for higher digits
		bsr		@disp
		puls	b
@putd	tfr		b,a
		adda	#'0'											;then display this digit
		jmp		io_put_ch

; @@@
; @@@ Math
; @@@

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

; ==> 16 Bit Division
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

; ==> Test if 16 bit value is a valid byte offset (-128..127)
; PASSED:  D = 16 bit value
; RETURNS: CC if value is valid else CS
is_byte_offset:
		cmpd	#127
		bra		@thi
;
; ==> Test if 16 bit value is a valid signed or unsigned byte value (-128..255)
; PASSED:  D = 16 bit value
; RETURNS: CC if value is valid else CS
is_byte_value:
		cmpd	#255
@thi	bgt		@nope								;if not byte safe
		cmpd	#-128
		blt		@nope
		andcc	#~CC_C								;return CC
		rts
;
@nope	orcc	#CC_C								;return CS
		rts

; @@@
; @@@ Messages
; @@@

; ==> Display message following call.
; PASSED:  (return address) = pointer to NUL terminated string.
; RETURNS: to instruction after string.
msg:	pshs	x
		ldx		2,s												;return address
		bsr		msgx											;display message
		stx		2,s												;update return address
		puls	x
		rts

; ==> Display message.
; PASSED:  X = pointer to NUL terminated string
; RETURNS: X = address after NUL, A = NUL
@disp	jsr		io_put_ch
msgx	lda		,x+
		bne		@disp
		rts

; ==> Display newline (CR, LF)
nl:		pshs	a
		lda		#CR
		jsr		io_put_ch
		lda		#LF
		jsr		io_put_ch
		puls	a
		rts

; ==> Display spaces
; PASSED:	A = number of spaces to display (0 - 127)
spaces:	deca
		bmi		@done											;if all done
		pshs	a												;else display a space
		jsr		io_put_space
		puls	a
		bra		spaces
@done	rts

; @@@
; @@@ Characters
; @@@

; ==> Convert lower case char to upper.
; PASSED:	A = possible lower case char
; RETURNS:	A = converted char
to_upper_ch:
		cmpa	#'a'
		blo		@done											;if not lower case
		cmpa	#'z'
		bhi		@done											;ditto
		anda	#~$20											;force upper case
@done	rts

; ==> Convert char to printable (replace non-printable with dot)
; PASSED:  A = char to convert
; RETURNS: A = converted char
to_printable_ch:
		cmpa	#' '
		blo		@rep											;replace control chars
		cmpa	#'~'
		bls		@ret											;don't replace printable
@rep	lda		#'.'											;replace it
@ret	rts

; @@@
; @@@ Etc
; @@@

; ==> Calculate CRC-16 of a memory range
; PASSED:  X = start address, D = count (assumed to be non-zero)
; RETURNS: D = CRC-16 for the range
crc16:
		pshs	y
		tfr		d,y												;Y = byte count
		ldd		#$FFFF											;D = CRC
;
@lp		eora	,x+												;next byte
		bsr		@upd											;update 8 bits
		bsr		@upd
		bsr		@upd
		bsr		@upd
		bsr		@upd
		bsr		@upd
		bsr		@upd
		bsr		@upd
		leay	-1,y
		bne		@lp												;if range not done
;
		puls	y
		rts
;
@upd	lslb													;<< 1
		rola
		bcc		@ret											;if no carry out
		eora	#$10											;else flip some bits
		eorb	#$21
@ret	rts

; ==> Table mapping bit number to mask
byte_masks:
		fcb		%00000001	;bit 0
		fcb		%00000010	;bit 1
		fcb		%00000100	;bit 2
		fcb		%00001000	;bit 3
		fcb		%00010000	;bit 4
		fcb		%00100000	;bit 5
		fcb		%00100000	;bit 6
		fcb		%00100000	;bit 7

; ==> Display register
; PASSED:  A = register index (REG_A, ...)
disp_reg:
		pshs	a,b,y
;
		tfr		a,b
		lslb
		clra
		tfr		d,y												;Y = register index * 2
		ldd		ln_reg_table,y									;A = 1st char, B = 2nd
;
		bsr		io_put_ch
		tstb
		beq		@ret											;if no 2nd char
		tfr		b,a
		bsr		io_put_ch
;
@ret	puls	a,b,y
		rts
