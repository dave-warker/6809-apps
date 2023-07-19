; "expr.asm" - expression processing for DeMon
; 2022-sep-05 dww. created.

BINOPS	macro noexpand
		jsr		ln_do_binops
		fdb		{1}
		endm

BINOP	macro noexpand
		fcb		{1}
		fdb		{2}
		endm

BINEND	macro noexpand
		fdb		0
		endm

; <range> ::= <start> {' ' <end> | ',' <count>}
; PASSED:	D = default range count (if none supplied)
; RETURNS:	X = range start, D = range count
; NOTE: Bails if there is an error
ln_get_range1:
		ldd		#1							;default range count of 1
ln_get_range:
		pshs	d							;2,s = range count
		bsr		ln_get_expr
		pshs	d							;0,s = range start
;
		lbsr	ln_peek_cmd_ch
		bcs		@done						;if end of command
		cmpa	#SPACE
		bne		cnt?						;if not explicit range end
		lbsr	ln_skip_ch
		bsr		ln_get_expr					;get range end
		subd	0,s							;end - start
		lblo	cmd_err						;error if end < start
		addd	#1							; + 1 = count
		std		2,s							;set range count
		bra		@done
;
cnt?	cmpa	#COMMA
		bne		@def						;if not explicit count
		lbsr	ln_skip_ch
		bsr		ln_get_expr
		std		2,s							;set range count
		bra		@done
;
@def	lbsr	ln_unskip_ch				;no range sep, use default count
@done	puls	x							;X = range start
		puls	d							;D = range count
		cmpd	#0
		lbeq	cmd_err						;count of 0 is an error
		rts

; ==> Get expression from line buffer.
; RETURNS:	D = expression value
; NOTE: Doesn't return if there is an error
ln_get_expr:
		bsr		ln_try_expr					;try for expression
		lbcs	cmd_err						;bail if failed
		rts

; ==> Try to get an expression from the line buffer.
; RETURNS: CS if no expression found else CC, D = expression value
ln_try_expr:
		pshs	x,y,u
		bsr		log_exp						;<expr> ::= <logOp>
		puls	x,y,u
		rts

; <logOp> ::= <sumOp>{'|' | '&' | '^' <sumOp>}
log_exp	BINOPS	sum_exp
		 BINOP	'&',log_and
		 BINOP	'|',log_or
		 BINOP	'^',log_eor
		BINEND
;
log_and	pshs	x							;D = LHS and RHS
		anda	,s+
		andb	,s+
		rts
;
log_or	pshs	x							;D = LHS or RHS
		ora		,s+
		orb		,s+
		rts
;
log_eor	pshs	x							;D = LHS eor RHS
		eora	,s+
		eorb	,s+
		rts

; <sumOp> ::= <prodOp>{'+' | '-' <prodOp>}
sum_exp	BINOPS	prd_exp
		 BINOP	'+',sum_add
		 BINOP	'-',sum_sub
		BINEND
;
sum_add	leax	d,x							;D = RHS + LHS
		exg		d,x
		rts
;
sum_sub	pshs	x							;D = LHS - RHS
		subd	,s++
		rts

; <prodOp> ::= <unOp>{'*' | '/' | '\' <unOp>}
prd_exp	BINOPS	una_exp
		 BINOP	'*',mul16
		 BINOP	'/',div16
		 BINOP	'\\',mod16
		BINEND

; <unop> ::= {'-' | '~'}<term>
una_exp	lda		#'-'
		jsr		ln_peek_for_ch
		beq		@neg							;if it's negate
		lda		#'~'
		jsr		ln_peek_for_ch
		bne		trm_exp							;if it's not complement
;
@com	bsr		trm_exp							;D = <term>
		bcs		@ret							;if error
		coma									;D = ~D
		comb
@ok		andcc	#~CC_C							;return CC
@ret	rts
;
@neg	bsr		@com							;D = ~<term>
		bcs		@ret							;if error
		addd	#1								;D = -<term>
		bra		@ok

; <term> ::= '#'<dec> | "'"<char> | '(' <expr> ')' | '`'<reg> | <hex>
trm_exp	jsr		ln_get_cmd_ch
		bcs		@ret						;error if end of buffer
		cmpa	#'&'
		beq		ln_get_decimal				;if should be decimal number
		cmpa	#'$'
		lbeq	ln_get_hex					;if should be hex number
		cmpa	#SQUOT
		beq		@char						;if should be character
		cmpa	#'`'
		beq		@sym						;if should be symbol
		cmpa	#'@'
		beq		@indir						;if should be indirection
		cmpa	#'('
		beq		@paren						;if should be parenthesized expression
		jsr		ln_unskip_ch
		bra		ln_get_hex					;else try hex by default
;
@char	jsr		ln_get_ch
		bcs		@ret						;error if end of buffer
		tfr		a,b
		clra
@ok		andcc	#~CC_C						;return CC
		rts
;
@sym	jsr		ln_get_register
		bcs		@sym2						;if not a register
		tfr		a,b
		clra
		tfr		d,x
		jsr		reg_get_value				;else D = register value
		bra		@ok
;
@sym2	jsr		expr_get_symbol				;D = symbol value
		bcs		@err						;if it's not a symbol
		bra		@ok
;
@indir	jsr		log_exp						;get expression value
		bcs		@ret						;if error
		tfr		d,x
		ldd		,x							;get value at that address
		bra		@ok
;
@paren	jsr		log_exp						;get parenthesized expression
		bcs		@ret						;if error
		pshs	d
		lda		#')'
		jsr		ln_peek_for_ch
		puls	d
		beq		@ok							;if got closing paren
;
@err	orcc	#CC_C						;return CS for error
@ret	rts

; ==> <dec> ::= ['0'-'9']+
; RETURNS: CS if no decimal number found else CC,D = unsigned decimal number
ln_get_decimal:
		pshs	x
;
		bsr		@ddig								;get first digit
		bcs		@ret								;error if none
		tfr		d,x									;X = accumulated number
;
@dlp	bsr		@ddig								;next digit
		bcs		@ok									;if no more
		exg		d,x									;n = n*10 + digit
		bsr		mulby10
		leax	d,x
		bra		@dlp
;
@ok		andcc	#~CC_C								;return CC, no error
		tfr		x,d
@ret	puls	x
		rts
;
@ddig	jsr		ln_peek_cmd_ch
		bcs		@ret2								;if end of command
		suba	#'0'
		bcs		@ret2								;if not decimal
		cmpa	#9
		bls		@ok2								;if it is decimal
		orcc	#CC_C								;return CS, not a decimal char
@ret2	rts
;
@ok2	jsr		ln_skip_ch							;consume the digit char
		tfr		a,b
		clra										;D = digit
		andcc	#~CC_C								;return CC
		rts
;
; ==> <hex> ::= ['0'-'9','A'-'F','a'-'f']+
; RETURNS:	CS if error else CC,D = hexadecimal number
ln_get_hex:
		pshs	x
;
		bsr		@xdig								;get first digit
		bcs		@ret								;error if none
		tfr		d,x									;X = accumulated number
;
@xlp	bsr		@xdig								;next digit
		bcs		@ok									;if no more
		exg		d,x									;n = n*16 + digit
		aslb
		rola
		aslb
		rola
		aslb
		rola
		aslb
		rola
		leax	d,x
		bra		@xlp
;
@xdig:	jsr		ln_peek_cmd_ch
		bcs		@ret2								;if end of command
		bsr		decode_hex_ch
		bcs		@ret2								;if not hex digit
		bra		@ok2

; ==> 16 bit multiply by 10 (w/o overflow check)
; PASSED:  D = unsigned number
; returnS: D = number * 10
mulby10:
		aslb
		rola										;D = n * 2
		pshs	d
		aslb
		rola										;D = n * 4
		aslb
		rola										;D = n * 8
		addd	,s++								;D = n * 10
		rts

; ==> Decode hex digit
; PASSED:  A = possible hex digit
; RETURNS: CS if not a digit else CC, A = decoded digit
decode_hex_ch:
		lbsr	to_upper_ch							;force upper case
		suba	#'0'
		blo		@ret								;if not a valid digit
		cmpa	#9
		bls		@yep								;if it's a decimal digit
		suba	#'A'-'0'
		bcs		@ret								;if not a valid digit
		adda	#10
		cmpa	#15
		bls		@yep								;if it is a valid digit
;
		orcc	#CC_C								;else return error
		rts
;
@yep	andcc	#~CC_C								;no error
@ret	rts

; ==> Dispatch binary expression operators
; ,s = subexpr function, (operator char, operator function pairs)+, 0
ln_do_binops:
		ldx		,s							;X = return address
		jsr		[,x]						;D = Left Hand Side (LHS)
		bcs		@ret						;if error then return CS
;
@opslp	ldu		,s							;U = return addr = parameters
		pshs	d							;,s = LHS value
		ldx		,u++						;X = sub-expr function
@oplp	pulu	a,y							;A = op char, Y = op function
		tsta
		beq		@done						;if no ops match
		jsr		ln_peek_for_ch
		bne		@oplp						;if not this operator
;
		pshs	y
		jsr		,x							;D = Right Hand Side (RHS)
		puls	x,y
		exg		x,y
		exg		d,x							;D = LHS, X = RHS, Y = function
		bcs		@ret						;if error in RHS
		jsr		,y							;perform operator
		bra		@opslp						;check for another
;
@done	puls	d							;D = return value
		andcc	#~CC_C						;return CC to caller's caller
@ret	leas	2,s							;drop addr of params
		rts

; ==> Look for symbol as next token
; RETURNS: CC, D = symbol value else CS if none found
expr_get_symbol:
		ldd		<ln_next					;save in case we fail
		pshs	d
;
		leas	-MAX_SYM_LENGTH,s			;S = symbol name buffer
		tfr		s,y
		ldb		#MAX_SYM_LENGTH
		jsr		ln_get_uc_word				;sets MSB of last char
		bcs		@fail						;if def not a symbol
;
		leay	@syms,pcr					;Y = list of symbols
@symlp	tfr		s,x							;X = target symbol
@chlp	lda		,y
		cmpa	,x+
		bne		@next						;if don't match
		tst		,y+
		bpl		@chlp						;if not end of symbol
;
		ldd		,y							;D = symbol value
		andcc	#~CC_C						;return CC
@done	leas	MAX_SYM_LENGTH+2,s			;clean up the stack
		rts
;
@next	tst		,y+							;skip to end of symbol
		bpl		@next
		leay	2,y							;and skip the value
		bra		@symlp
;
@fail	ldd		MAX_SYM_LENGTH,s			;reset buffer index
		std		<ln_next
		orcc	#CC_C						;return CS
		bra		@done
;
@syms	fcb		'A','L','F'+$80,ALF_SYM_VALUE/256,ALF_SYM_VALUE&$FF
		fcb		'D','E','M','O','N'+$80,DEMON_SYM_VALUE/256,DEMON_SYM_VALUE&$FF
		fcb		'S','Y','L'+$80,SYL_SYM_VALUE/256,SYL_SYM_VALUE&$FF
		fcb		0
