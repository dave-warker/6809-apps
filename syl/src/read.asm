; "read.asm" - Read S-Expression support for Syl LISP
; 2023-feb-20 dww  created

; >>> Read the next S-Expression from the console
; RETURNS: X = addr/ref to the s-expression
; NOTE: It's won't be protected from following GCs unless you
; push it on the root stack.
read:
			stu		<read$root								;in case we fail
			clr		<sexp$par$depth							;no parens yet
;
@sexpr		jsr		lex$get$tok								;next token
@sexpr2		cmpb	#TOK_INT
			beq		@done									;we're done if it's an integer
			cmpb	#TOK_ATOM
			beq		@done									;ditto for ATOM
			cmpb	#TOK_LPAR
			beq		@cons									;get parened CONS
			cmpb	#TOK_QUOTE
			bne		@serr									;if not quoted expr
;
; 'expr -> (QUOTE expr)
;
@quote		bsr		@sexpr									;X = expression to quote
			pshu	x										;save & protect it
			jsr		cons$alloc								;make it into a list
			pulu	d
			std		CC_CAR_OFF,x							;CDR is inited to NIL
			pshu	x										;save/protect it
			ldd		#ATOM_QUOTE								;allocate quote ref
			jsr		cons$alloc$tag
			pulu	d
			std		CC_CDR_OFF,x							;set ref to expr list
@done		rts
;
; CONS (dotted pair or list)
;
@cons		jsr		lex$get$tok								;check for empty list
			cmpb	#TOK_RPAR
			beq		@empty									;() -> NIL
;
			inc		<sexp$par$depth							;non-empty list/pair, bump paren level
			pshs	x,b										;allocate root cell
			jsr		cons$alloc
			tfr		x,d										;2,u = root, 0,u = end of list
			pshu	x,d
			puls	b,x
;
@conslp		bsr		@sexpr2									;get next item
			tfr		x,d
			ldx		,u
			std		CC_CAR_OFF,x							;set as car op last item
;
			jsr		lex$get$tok								;more to come?
			cmpb	#TOK_RPAR
			beq		@consend								;nope
			cmpb	#TOK_DOT
			beq		@dotted									;yep, need cdr of dotted pair
;
			pshs	x,b										;allocate new last cell
			jsr		cons$alloc
			tfr		x,d										;link it to previous last
			ldx		,u
			std		CC_CDR_OFF,x
			std		,u
			puls	b,x
			bra		@conslp									;and get next item
;
@empty		ldx		#ATOM_NIL								;return empty list
			bra		@done
;
@dotted		bsr		@sexpr									;get cdr for current cell
			tfr		x,d
			ldx		,u
			std		CC_CDR_OFF,x
;
			jsr		lex$get$tok								;better be closing paren
			cmpb	#TOK_RPAR
			bne		@serr									;else bail
;
@consend	pulu	d,x										;X = root of list
			dec		<sexp$par$depth							;up a paren level
			rts
;
@serr		ldu		<read$root								;restore original root
			jsr		bail
			fcb		8,'S','y','n','t','a','x','!',BEL
