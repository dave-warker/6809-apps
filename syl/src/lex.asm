; "lex.asm" - lexer for Syl 6809 Lisp
; 2023-feb-16 dww  created

; Get next token.
; RETURNS: B = token type (TOK_LPAR, etc); X = optional associated value
lex$get$tok:
			jsr		con$getnb								;next non-blank char
			ldx		<con_buf_next							;save token start for errors
			stx		<lex$tok$start
;
; SIMPLE TOKENS
;
			lbsr	lex$is$token							;is it a single char token?
			bne		@neg?									;nope
			ldx		#ATOM_NIL								;else char is token tag
			rts
;
; INTEGER
;
@neg?		cmpb	#'-'
			bne		@int?									;if not negative integer
			jsr		con$getc								;else get first digit
			lbsr	lex$is$digit
			beq		@neg									;if it *is* a negative number
			jsr		con$ungetc								;else put char back
			ldb		#'-'									;and treat it as an ATOM
			bra		@atom
;
@neg		lbsr	lex$parse$int							;get integer value
			tfr		x,d										;negate the int token value
			coma
			comb
			addd	#1										;-n = ~n + 1
			ora		#CCS_TYPE_INT/256						;set type as int
			tfr		d,x
@rint		ldb		#TOK_INT								;return TOK_INT
			rts
;
@int?		bsr		lex$is$digit
			bne		@atom									;if not integer then assume atom
			lbsr	lex$parse$int							;X = int value
			bra		@rint									;return TOK_INT
;
; ATOMS
;
@atom		bsr		lex$is$atom								;is it a legit ATOM char?
			bne		@badch									;nope
;
			ldx		<atom_unalloc							;copy to start of unallocated ATOM space
			leax	1,x										;first byte will be length
@atomlp		cmpx	#ATOM_POOL_END
			bhs		@full									;if ATOM pool is full
			lbsr	lex$is$alpha
			bne		@store									;if not alphabetic
			andb	#$DF									;else force UPPER
@store		stb		,x+										;save ATOM char
			jsr		con$getc								;next char
			bsr		lex$is$atom
			beq		@atomlp									;if not done yet
			jsr		con$ungetc								;uneat the delimiter
			tfr		x,d										;set the ATOM length
			subb	<atom_unalloc+1
			decb
			ldx		<atom_unalloc
			stb		,x
;
			jsr		lex$find$atom							;is it already in the pool?
			beq		@ret									;if so (X = addr/ref)
			pshs	x										;else add at the end
			ldb		,x
			incb
			abx
			stx		<atom_unalloc
			puls	x										;X = addr/ref
@ret		ldb		#TOK_ATOM								;return TOK_ATOM, X = ref
			rts
;
@badch		jsr		bail
			fcb		10,'B','a','d',' ','C','h','a','r','!',BEL
;
@full		jsr		bail
			fcb		16,'A','t','o','m',' ','p','o','o','l',' ','f','u','l','l','!',BEL

; >>> Is char a legit (single-char) token?
; PASSED:  B = input char
; RETURNS: EQ if it is a legit token else NE
lex$is$token:
			cmpb	#'('
			beq		@ret									;return TOK_LPAR
			cmpb	#')'
			beq		@ret									;return TOK_RPAR
			cmpb	#'.'
			beq		@ret									;return TOK_DOT
			cmpb	#SQUOT
@ret		rts

; >>> Is char an integer digit?
; PASSED:  B = input char
; RETURNS: EQ if it is a digit else NE
lex$is$digit:
			cmpb	#'0'
			blo		@ret									;not a digit
			cmpb	#'9'
			bhi		@ret									;ditto
			orcc	#$04									;else return EQ
@ret		rts

; >>> Is char a valid ATOM char?
; PASSED:  B = input char
; RETURNS: EQ if it is a valid ATOM char else NE
lex$is$atom:
			bsr		lex$is$alpha
			beq		@ret									;yep, it's a letter
			bsr		lex$is$digit
			beq		@ret									;yep, it's a digit
			cmpb	#SPACE+1
			blo		@ret									;nope, it's whitespace
			bsr		lex$is$token
			pshs	a
			tfr		cc,a									;it is is it's not a token
			eora	#$04									;(flip EQ->NE, NE->EQ)
			tfr		a,cc
			puls	a
@ret		rts

; >>> Is char an alphabetic (letter)?
; PASSED:  B = input char
; RETURNS: EQ if it is a valid alpha char else NE
lex$is$alpha:
			cmpb	#'A'
			blo		@ret									;nope
			cmpb	#'Z'
			bls		@yep									;yep
			cmpb	#'a'
			blo		@ret									;nope
			cmpb	#'z'
			bhi		@ret									;nope
@yep		orcc	#$04									;yep, return EQ
@ret		rts

; >>> Parse integer cell value
; PASSED:  B = first char of integer (assumed valid)
; RETURNS: X = integer cell value
; NOTE: bails on invalid integer value (illegal char or out of range)
lex$parse$int:
			subb	#'0'									;X = first digit value
			clra
			tfr		d,x
;
@lp			jsr		con$getc								;next char
			bsr		lex$is$digit
			bne		@end									;if end of integer
			subb	#'0'									;X = X*10 + digit
			pshs	b
			tfr		x,d
			leax	d,x										;X *= 2
			aslb
			rola
			aslb
			rola
			leax	d,x										;X *= 6
			leax	d,x										;X *= 10
			puls	b
			leax	b,x
			bra		@lp
;
@end		bsr		lex$is$alpha							;can't end on letter
			beq		@bad
			jsr		con$ungetc								;put delim back
;
			cmpx	#CCS_INT_MAX
			bgt		@bad
;
			tfr		x,d										;convert to slot INT
			ora		#CCS_TYPE_INT/256
			tfr		d,x
			rts
;
@bad		jsr		bail
			fcb		9,'B','a','d',' ','I','n','t','!',BEL

; >>> Find ATOM ref in the pool
; PASSED:  X = addr of ATOM name (length byte first)
; RETURNS: EQ, X = ATOM ref/addr else NE and X unchanged
; NOTE: Was going to use a hash table for speed but I don't think the complexity
; is worth it. A straight linear search should be fast enough since we're only
; ever doing ATOM searches from interactive input so a few ms isn't going to matter.
; Also, the compare loop assumes ATOMs are < 128 chars long which is safe as
; long as the input buffer length is also < 128 chars.
lex$find$atom:
			pshs	y,d
			ldy		#ATOM_POOL_ORG							;Y = next ATOM name in pool
;
@atomlp		cmpy	<atom_unalloc
			bhs		@nope									;if ATOM not found
;
			pshs	y,x										;see if names match
			lda		,y										;A = pool name len = compare count - 1
@namelp		ldb		,x+
			cmpb	,y+
			bne		@next									;if they don't match
			deca
			bpl		@namelp									;if not done yet
;
			puls	d,x										;X = pool ATOM addr/ref
			clra											;return EQ
@done		puls	d,y
			rts
;
@next		leay	a,y										;skip to next pool ATOM
			puls	x										;X = target name again
			leas	2,s										;clean stack
			bra		@atomlp									;and try next
;
@nope		andcc	#~$04									;return NE
			bra		@done
