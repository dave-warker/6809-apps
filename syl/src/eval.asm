; "eval.asm" - Evaluate an S-Expression for Syl
; 2023-feb-20 dww  created

; >>> Evaluate an S-Expression
; PASSED:  X = root of the S-Expression
; RETURNS: X = root of the result
; NOTE: This uses the known order of ref types (ATOMs, Built-Ins, CONS, etc)
; for more efficient dispatch so if that changes this must too.
eval:
			cmpx	#CCS_REF_ATOM_END
			blo		eval$atom						;eval an ATOM
			cmpx	#CCS_REF_BIS_END
			blo		eval$bis						;eval a built-in
			cmpx	#CCS_REF_CONS_END
			blo		eval$cons						;eval a CONS cell
@ret		rts										;else assume value is self (e.g. Ints)
;
; Evaluate an ATOM
;
eval$atom:	cmpx	#ATOM_LAST_IDEM
			blo		@ret							;idempotent always evaluate to itself
			jsr		env$get$atom$binding
			ldx		CC_CDR_OFF,x					;X = bound value
	; fall through into (to eval BIVs) ...
;
; Evaluate a naked built-in ref if it's a BIV
;
eval$bis:	cmpx	#CCS_REF_BIV_FIRST
			blo		@ret							;return self if not a built-in value
			cmpx	#CCS_REF_BIV_END
			bhs		@ret							;ditto
;
@biv		leax	bis$base-CCS_REF_BIV_FIRST,x	;X = address of BIV routine
			jmp		,x								;invoke it and return
;
; Evaluate a CONS
;
eval$cons:	pshu	x								;protect expr
			ldx		CC_CAR_OFF,x
			bsr		eval							;X = (EVAL (CAR e)) = possible invocation target
;
			cmpx	#ATOM_QUOTE
			beq		@quote							;if it's a quoted expr
			cmpx	#CCS_REF_BIF_FIRST
			blo		@self							;if it's an ATOM or BIV
			cmpx	#CCS_REF_BIF_END
			blo		@bif							;if it's a built-in function
			cmpx	#CCS_REF_BIQ_END
			blo		@biq							;if it's a built-in quoted function
			cmpx	#CCS_REF_CONS_FIRST
			blo		@self							;if it's not a CONS
			cmpx	#CCS_REF_CONS_END
			blo		@cons							;if it is a CONS
;
@self		pulu	x								;value is original CONS (?)
			rts
;
; Possible invocation target is a CONS
; X = invocation target; ,U = original CONS expression
;
@cons		ldd		CC_CAR_OFF,x
			cmpd	#ATOM_FN
			beq		@fn								;if it's a function invocation
			cmpd	#ATOM_FNQ
			bne		@self							;if it's not a quoted function invocation
;
; Invoking an FNQ function: ((FNQ (arg-names) (body)) arg-values)
; X = invocation target; ,U = original CONS expression
;
@fnq		pshu	x								;protect FNQ definition
			ldx		2,u
			ldx		CC_CDR_OFF,x					;X = arg-values
;
@apply		ldy		,u								;Y = (FNQ ...)
			ldy		[CC_CDR_OFF,y]					;Y = (CADR (FNQ ...)) = arg-names
			bsr		eval$apply$args					;push ENV with assigned arg values to Y
;
			ldx		2,u								;X = (FNQ ...)
			ldx		CC_CDR_OFF,x
			ldx		[CC_CDR_OFF,x]					;X = (CADDR (FNQ)) = function body
			jsr		eval							;evaluate it
;
			leau	6,u								;pop ENV, clean up the stack
			rts
;
; Invoking an FN function: ((FNQ (arg-names) (body)) arg-values)
; X = invocation target; ,U = original CONS expression
;
@fn			pshu	x								;protect FN definition
			ldx		2,u
			ldx		CC_CDR_OFF,x					;X = arg-values
			bsr		eval$list						;X = (EVAL arg-values)
			bra		@apply							;finish like FNQ
;
; (QUOTE s) - return unevaluated arg
;
@quote		pulu	x								;X = original CONS
			ldx		[CC_CDR_OFF,x]					;return unevalated arg
			rts
;
; (BIF args) - built-in function with evaluated args
;
@bif		leax	bis$base-CCS_REF_BIV_FIRST,x	;X = address of BIF routine
			pshs	x
			pulu	x								;X = original CONS
			ldx		CC_CDR_OFF,x					;X = arg list
			bsr		eval$list						;X = evaluated arg list
			rts										;off to the built-in
;
; (BIQ args) - built-in function with unevaluated args
;
@biq		leax	bis$base-CCS_REF_BIV_FIRST,x	;X = address of BIQ routine
			pshs	x
			pulu	x								;X = original CONS
			ldx		CC_CDR_OFF,x					;X = unevaluated arg list
			rts										;off to the built-in

; >>> Evaluate a list of items
; PASSED:  X = ref to first CONS in list
; RETURNS: X = ref to new list with evaluated items
eval$list:
			ldd		#eval							;D = subr to apply
	; fall through into ...
; >>> Generate a new list by calling a subr for each item of an existing list.
; PASSED:  X = ref to an existing list, D = addr of one arg subroutine to call on each item.
; RETURNS: X = ref to a new list with the results of the calls.
eval$maplist:
			cmpx	#ATOM_NIL
			beq		@ret							;NIL -> NIL
			pshs	d								;,s = subroutine to call
;
			pshu	x								;4,u = current source list item
			jsr		cons$alloc
			tfr		x,d
			pshu	x,d								;2,u = root of dest list; ,u = current dest item
;
@maplp		ldx		4,u
			ldx		CC_CAR_OFF,x					;X = current item
			jsr		[,s]							;call the function
			tfr		x,d
			ldx		,u								;store result in dest list
			std		CC_CAR_OFF,x
;
			ldx		4,u								;advance to next source item
			ldx		CC_CDR_OFF,x
			beq		@mapend							;if hit end of list
			stx		4,u								;update current item
			jsr		cons$alloc						;allocate another dest item
			tfr		x,d
			ldx		,u
			std		CC_CDR_OFF,x
			std		,u
			bra		@maplp							;handle next item
;
@mapend		ldx		2,u								;X = new list
			leau	6,u								;clean up stacks
			leas	2,s
@ret		rts

; >>> Create an ENV with actual args values assigned to arg names
; PASSED:  Y = list of names, X = list of values (may be NIL)
; Pushes new ENV with assigned values to U.
eval$apply$args:
			pshu	y,x								;2,U = values, 4,U = names
			jsr		env$alloc
			pshu	x								;,U = env
;
@lp			ldx		4,u								;X = next item in names list
			beq		@done							;if no more
			ldd		CC_CAR_OFF,x					;D = name ATOM
			ldx		CC_CDR_OFF,x
			stx		4,u
;
			ldx		2,u								;X = next item in values list
			beq		@add							;use NIL if none left
			ldy		CC_CDR_OFF,x
			sty		2,u
			ldx		CC_CAR_OFF,x					;X = value
;
@add		jsr		env$alloc$binding				;X = binding entry
			tfr		x,d
			ldx		,u								;X = ENV head
			jsr		env$add$binding					;add binding to ENV
			bra		@lp
;
@done		pulu	d								;clean up ref stack
			leau	2,u
			std		,u								;but leave ENV on top
			rts
