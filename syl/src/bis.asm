; "bis.asm" - Implementations for Built-ins for Syl.
; 2023-feb-21 dww  created
;
; Built-Ins refer to values and functions implemented in 6809
; code as part of the interpreter itself. Syl refs other than Ints are
; essentially pointers into specific memory ranges that hold each type
; of data (ATOMs, CONS, etc). Can't really do that for Built-Ins since
; the ROM address range they occupy are part of the Integer ref space.
; So instead we use the range of addresses for variables (VAR_ORG..VAR_END)
; as an offset into this built-in function module. That means we're limited
; to VAR_SIZE values (1K at the moment) so all the built-ins must at least
; start in 1KB of space. If that becomes an issue I can use a vector table
; instead of a plain offset but we'll cross that bridge when it's officially
; in flames.
;
; There are three different types of built-ins: values (BIVs), functions (BIFs)
; and quoted functions (BIQs). BIFs args are passed evaluated but BIQs are not.
; Each type occupies a separate range of offsets so we can tell them apart.

; >>> Base address for all built-in references:
bis$base	equ		*

;@@@
;@@@ BUILT-IN VALUES
;@@@
biv$base	equ		*

; >>> Syl version
BIV_VER		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		#SYL_VER*10+SYL_REV+CCS_TYPE_INT
			rts

; >>> Current ENVironment
BIV_ENV		equ		*-bis$base+CCS_REF_BIS_FIRST
			jsr		env$cur
			tfr		y,x
			rts

; >>> Current free CONS cell count
BIV_FREE	equ		*-bis$base+CCS_REF_BIS_FIRST
			jmp		gc$free

biv$end		equ		*
;
CCS_REF_BIV_FIRST	equ		biv$base-bis$base+CCS_REF_BIS_FIRST
CCS_REF_BIV_END		equ		biv$end-bis$base+CCS_REF_BIS_FIRST

;@@@
;@@@ BUILT-IN FUNCTIONS (X = EVALUATED ARGS)
;@@@
bif$base	equ		*

;@@@ BASIC LIST MANIPULATION

; >>> (CAR e)
BIF_CAR		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		[CC_CAR_OFF,x]						;X = (CAR arg1)
			rts

; >>> (CDR e)
BIF_CDR		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;X = arg1
			ldx		CC_CDR_OFF,x						;value = (CDR x)
			rts

BIF_CADR	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;X = arg
@cadr		ldx		[CC_CDR_OFF,x]						;X = (CAR (CDR arg))
			rts
;
BIF_CADDR	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;X = arg
			ldx		CC_CDR_OFF,x						;X = (CDR arg)
			bra		@cadr

BIF_CDDR	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;X = arg
			ldx		CC_CDR_OFF,x
			ldx		CC_CDR_OFF,x						;X = (CDDR arg)
			rts

; >>> (CONS e1 e2)
BIF_CONS	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;new CAR
			ldx		[CC_CDR_OFF,x]						;new CDR
			pshu	x,d
			jsr		cons$alloc							;allocate a CONS cell
			pulu	d
			std		CC_CAR_OFF,x						;set CAR
			pulu	d
			std		CC_CDR_OFF,x						;set CDR
			rts

; >>> (LIST e1 e2 ...)
BIF_LIST	equ		*-bis$base+CCS_REF_BIS_FIRST
			rts

; >>> (RPLACA c e)
BIF_RPLACA	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x
			ldx		[CC_CDR_OFF,x]
			exg		d,x									;X = CONS, D = new CAR
			std		CC_CAR_OFF,x
			rts

; >>> (RPLACD c e)
BIF_RPLACD	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x
			ldx		[CC_CDR_OFF,x]
			exg		d,x									;X = CONS, D = new CDR
			std		CC_CDR_OFF,x
			rts

;@@@ PREDICATES

; >>> (ATOM e)
BIF_ATOM	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;argument
			jsr		ref$is$atom
ret$bool	beq		ret$true							;if it is an ATOM
;
ret$nil:	ldx		#ATOM_NIL							;return NIL/F
			rts
;
ret$true:	ldx		#ATOM_T								;return T
			rts

; >>> (NULL e)
BIF_NULL	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;argument
			bra		ret$bool

; >>> (EQ e1 e2)
BIF_EQ		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;arg1
			cmpd	[CC_CDR_OFF,x]						;compare to arg2
			bra		ret$bool

; >>> (EQUAL e1 e2)
BIF_EQUAL	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;D = arg1
			ldx		[CC_CDR_OFF,x]						;X = arg2
			bsr		@equal								;recursive compare
			bra		ret$bool
;
@equal		pshs	x
			cmpd	,s++
			beq		@ret								;if they're the same
;
			jsr		ref$is$cons
			bne		@ret								;nope, and they're not CONS
			exg		d,x
			jsr		ref$is$cons
			bne		@ret								;ditto
;
			pshs	x,d									;compare CARs
			ldd		CC_CAR_OFF,x
			ldx		[,s]
			bsr		@equal
			puls	d,x
			bne		@ret								;if they're not equal
;
			pshs	d									;compare CDRs
			ldd		CC_CDR_OFF,x
			puls	x
			ldx		CC_CDR_OFF,x
			bsr		@equal
@ret		rts											;return EQ if EQUAL else NE

; >>> (NOT e)
BIF_NOT		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;arg
			bra		ret$bool

; >>> (!= e1 e2)
BIF_NE		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;arg1
			cmpd	[CC_CDR_OFF,x]						;compare to arg2
			bne		ret$true
			bra		ret$nil

BIF_LT		equ		*-bis$base+CCS_REF_BIS_FIRST
			bsr		cmp$int$args						;compare arg1, arg2
			blt		ret$true
			bra		ret$nil

BIF_GT		equ		*-bis$base+CCS_REF_BIS_FIRST
			bsr		cmp$int$args						;compare arg1, arg2
			bgt		ret$true
			bra		ret$nil

BIF_LE		equ		*-bis$base+CCS_REF_BIS_FIRST
			bsr		cmp$int$args						;compare arg1, arg2
			ble		ret$true
			bra		ret$nil

BIF_GE		equ		*-bis$base+CCS_REF_BIS_FIRST
			bsr		cmp$int$args						;compare arg1, arg2
			bge		ret$true
			bra		ret$nil

; >>> Compare two integer arguments
; PASSED:  X = list of args
; RETURNS: CC = compare arg1,arg2
cmp$int$args:
			ldd		[CC_CDR_OFF,x]						;D = arg2
			bsr		int$sext							;sign extend to 16 bits
			pshs	d
			bsr		int$arg								;D = arg1
			cmpd	,s++								;set flags
			rts
;
int$arg		ldd		CC_CAR_OFF,x						;D = current arg
int$sext	bita	#$40								;negative?
			bne		@ret								;yep, already sign extended
			anda	#$7F								;else positive
@ret		rts

;@@@ MATH

; >>> (+ e ...)
BIF_ADD		equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@add,pcr							;Y = op
;
@op			bsr		int$arg
			pshs	d									;,S = current value = arg1
@lp			ldx		CC_CDR_OFF,x						;next arg
			beq		@done								;if no more
			bsr		int$arg								;D = arg
			jsr		,y									;update current with arg value
			bra		@lp
@done		puls	d
ret$int		ora		#CCS_TYPE_INT/256
			tfr		d,x									;X = final value
			rts
;
@add		addd	2,s									;update sum
			std		2,s
			rts
;
; >>> (- e ...)
BIF_SUB		equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@sub,pcr							;Y = op
			bra		@op
;
@sub		pshs	d									;sure wish we had SUBD X|Y!
			ldd		4,s
			subd	,s++
			std		2,s
			rts
;
; >>> (MIN e ...)
BIF_MIN		equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@min,pcr							;Y = op
			bra		@op
;
@min		cmpd	2,s
			bge		@ret								;if arg >= cur
@upd		std		2,s									;else update cur
@ret		rts
;
; >>> (MAX e ...)
BIF_MAX		equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@max,pcr							;Y = op
			bra		@op
;
@max		cmpd	2,s
			bgt		@upd								;if arg > cur
			rts
;
; >>> (| e ...) - bitwise OR
BIF_BIT_OR	equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@or,pcr
			bra		@op
;
@or			ora		2,s
			orb		3,s
			bra		@upd
;
; >>> (& e ...) - bitwise AND
BIF_BIT_AND	equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@and,pcr
			bra		@op
;
@and		anda	2,s
			andb	3,s
			bra		@upd
;
; >>> (^ e ...) - bitwise XOR
BIF_BIT_XOR	equ		*-bis$base+CCS_REF_BIS_FIRST
			leay	@xor,pcr
			bra		@op
;
@xor		eora	2,s
			eorb	3,s
			bra		@upd

; >>> (* e1 e2)
BIF_MUL		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldy		#mul16
;
@op2		clr		,-s									;,S = final sign
			ldd		CC_CAR_OFF,x						;D = arg1
			bsr		@norm								;normalize to positive number
			pshu	d
			ldd		[CC_CDR_OFF,x]						;D = arg2
			bsr		@norm
			tfr		d,x
			pulu	d
			jsr		,y									;perform the operation
			ror		,s+
			bcc		ret$int								;if result should be positive
			bsr		@negd
			bra		ret$int
;
@norm		bita	#(CCS_INT_MASK-(CCS_INT_MASK/2))/256
			bne		@neg								;if it's a negative number
			anda	#CCS_INT_MASK/256					;else strip type
			rts											;and return positive number
@neg		inc		2,s									;negative number, flip result sign
@negd		coma										;and negate number
			comb
			addd	#1
			rts
;
; >>> (/ e1 e2)
BIF_DIV		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldy		#div16								;Y = op
			bra		@op2
;
; >>> (MOD e1 e2)
BIF_REM		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldy		#mod16								;Y = op
			bra		@op2

; >>> (READ)
BIF_READ	equ		*-bis$base+CCS_REF_BIS_FIRST
			jmp		read

; >>> (EVAL e)
BIF_EVAL	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldx		CC_CAR_OFF,x						;first argument
			jmp		eval

; >>> (PRINT e)
BIF_PRINT	equ		*-bis$base+CCS_REF_BIS_FIRST
bif$print	ldx		CC_CAR_OFF,x						;first argument
			pshu	x
			jsr		print
			pulu	x
			rts

; >>> (PRINTLN e)
BIF_PRINTLN	equ		*-bis$base+CCS_REF_BIS_FIRST
			bsr		bif$print
			bsr		bif$nl
			rts

; >>> (NL)
BIF_NL		equ		*-bis$base+CCS_REF_BIS_FIRST
bif$nl		jmp		con$nl

; >>> (SET v e)
BIF_SET		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldy		CC_CAR_OFF,x
			ldx		[CC_CDR_OFF,x]
			exg		x,y									;X = v, Y = e
			jsr		env$get$atom$binding				;X = CONS w/binding
			sty		CC_CDR_OFF,x						;update to new value
			tfr		y,x									;return expression
			rts

; >>> (LOAD)
BIF_LOAD	equ		*-bis$base+CCS_REF_BIS_FIRST
			jmp		con$load

; >>> (LOADED)
BIF_LOADED	equ		*-bis$base+CCS_REF_BIS_FIRST
			jmp		con$loaded

; >>> (RETURN e) - only valid within a (PROG)
BIF_RETURN	equ		*-bis$base+CCS_REF_BIS_FIRST
			pshu	x									;allocate a ret entry (ret.<value>)
			jsr		cons$alloc
			ldd		#ATOM_ret
			std		CC_CAR_OFF,x
			pulu	y
			ldd		CC_CAR_OFF,y
			std		CC_CDR_OFF,x
			tfr		x,d									;D = the ret entry
;
			jsr		prg$find$first						;Y = addr of first 'prg' entry on U stack
			std		-4,y								;overwrite current expr with ret entry
			rts

; >>> (DEF v e)
BIF_DEF		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;Y = v
			ldx		[CC_CDR_OFF,x]						;X = e
			pshu	x
			ldy		<env_globals						;always defines in global env
			jsr		env$def$in							;go define it
			pulu	x									;return e
			rts

bif$end		equ		*
;
CCS_REF_BIF_FIRST	equ		bif$base-bis$base+CCS_REF_BIS_FIRST
CCS_REF_BIF_END		equ		bif$end-bis$base+CCS_REF_BIS_FIRST

;@@@
;@@@ BUILT-IN QUOTED FUNCTIONS (X = UNEVALUATED ARGS)
;@@@
biq$base	equ		*

; >>> (LET (vars) (expr))
BIQ_LET		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldy		CC_CAR_OFF,x						;Y = list of vars
			ldd		[CC_CDR_OFF,x]						;D = expression
			pshu	d
			ldx		#ATOM_NIL							;X = values (all NIL)
			jsr		eval$apply$args						;create an ENV
			pulu	d,x
			pshu	d									;evaluate the expr
			jsr		eval
			leau	2,u									;drop ENV
			rts

; >>> (SETQ v e)
BIQ_SETQ	equ		*-bis$base+CCS_REF_BIS_FIRST
			ldy		CC_CAR_OFF,x						;Y = v
			ldx		[CC_CDR_OFF,x]						;X = e
			pshs	y
			jsr		eval								;X = (EVAL e)
			puls	y
			exg		x,y									;X = v, Y = e
			jsr		env$get$atom$binding				;X = CONS w/binding
			sty		CC_CDR_OFF,x						;update to new value
			tfr		y,x									;return expression
			rts

; >>> (GC) - run garbage collection
BIQ_GC		equ		*-bis$base+CCS_REF_BIS_FIRST
			jsr		gc$run								;collect the garbage
			jmp		gc$free								;return free space

; >>> (COND (p e) ...)
BIQ_COND	equ		*-bis$base+CCS_REF_BIS_FIRST
@lp			cmpx	#ATOM_NIL
			beq		@ret								;if end of list return NIL
			ldy		CC_CAR_OFF,x						;Y = next pred, expr pair
			ldx		CC_CDR_OFF,x
			pshu	x
			ldx		CC_CAR_OFF,y						;X = predicate
			pshu	y									;evaluate predicate
			jsr		eval
			pulu	y
			cmpx	#ATOM_NIL
			bne		@true								;if value was true (non-NIL)
			pulu	x									;else next pair
			bra		@lp
;
@true		leau	2,u									;drop remaining pairs
			ldx		[CC_CDR_OFF,y]						;X = expression
			jmp		eval								;return it's value
@ret		rts

; >>> (AND e ..) - assumes at least one arg
BIQ_AND		equ		*-bis$base+CCS_REF_BIS_FIRST
@andlp		pshu	x									;eval next arg
			ldx		CC_CAR_OFF,x
			jsr		eval
			tfr		x,y									;Y = last arg value
			pulu	x
			cmpy	#ATOM_NIL
			beq		@done								;value is NIL so return NIL
			ldx		CC_CDR_OFF,x
			bne		@andlp								;next arg (if there is one)
;
@done		tfr		y,x									;return last value
			rts			

; >>> (OR e ..) - assumes at least one arg
BIQ_OR		equ		*-bis$base+CCS_REF_BIS_FIRST
@orlp		pshu	x									;eval next arg
			ldx		CC_CAR_OFF,x
			jsr		eval
			pulu	y
			cmpx	#ATOM_NIL
			bne		@done								;return it if non-NIL
			ldx		CC_CDR_OFF,y
			bne		@orlp								;handle next arg if any (else return NIL)
@done		rts											;X = return value

; >>> (PROG (vars) e ...)
BIQ_PROG	equ		*-bis$base+CCS_REF_BIS_FIRST
			pshu	x									;4,U = PROG list
			ldy		CC_CAR_OFF,x						;Y = vars list
			ldx		#ATOM_NIL
			jsr		eval$apply$args						;2,U = our vars 'env'
			ldd		#ATOM_prg							;allocate our 'prg' frame
			jsr		cons$alloc$tag
			ldy		2,u
			ldy		CC_CDR_OFF,y						;PROG expr list
			sty		CC_CDR_OFF,x						;append to 'prg' frame
			stx		2,u									;4,U = our 'prg' frame
			pshu	y									;,U = current expr in PROG list
;
@lp			ldx		,u									;eval next expr
			beq		@done								;or return NIL if end
			ldy		CC_CDR_OFF,x						;Y = next expr (or RETURN value)
;
			ldd		CC_CAR_OFF,x						;was a (RETURN e) executed?
			cmpd	#ATOM_ret
			beq		@ret								;yep, we're done
;
			sty		,u									;else update next expr
			cmpd	#CCS_REF_ATOM_END
			blo		@lp									;skip ATOMs (labels)
			jsr		eval								;eval this expr
			bra		@lp									;and check next
;
@ret		tfr		y,x									;X = RETURN value
@done		leau	6,u									;clean up the root stack
			rts

; >>> (GO label)
BIQ_GO		equ		*-bis$base+CCS_REF_BIS_FIRST
			ldd		CC_CAR_OFF,x						;D = label to find
;
			bsr		prg$find$first						;Y = addr of ptr to first 'prg' frame
@prglp		bne		@bail								;if none found
			ldx		,y									;X = the 'prg' frame (prg e e ...)
;
@labellp	ldx		CC_CDR_OFF,x						;X = next item in PROG list
			beq		@prgnext							;if end of list
			cmpd	CC_CAR_OFF,x
			bne		@labellp							;if it's not our label
;
			ldx		CC_CDR_OFF,x						;X = expr after label
			stx		-4,y								;update the 'prg' frame's next expr
			ldx		#ATOM_NIL							;return NIL
			rts
;
@prgnext	bsr		prg$find$next						;find next 'prg' frame
			bra		@prglp
;
@bail		tfr		d,x									;label we couldn't find
			jsr		bail$ref
			fcb		8,'C','a','n',SQUOT,'t',' ','G','O'

biq$end		equ		*
;
CCS_REF_BIQ_FIRST	equ		biq$base-bis$base+CCS_REF_BIS_FIRST
CCS_REF_BIQ_END		equ		biq$end-bis$base+CCS_REF_BIS_FIRST

;@@@
;@@@ END OF BUILT-INS
;@@@
bis$end		equ		*

	IFGT (bis$end-bis$base-VARS_SIZE)
	ERROR "BUILT-INS BIGGER THAN VARS_SIZE!"
	ENDIF

;@@@
;@@@ BUILT-INS SUPPORT
;@@@

; >>> Find first/next 'prg' frame on the root stack (U)
; PASSED:  Y = last found 'prg' frame (next only)
; RETURNS: EQ, Y = addr of 'prg' frame on the U stack else NE
; DOES NOT ALTER D
; NOTE: $next is only valid if no CONS allocs happened after the $first
prg$find$first:
			tfr		u,y									;start with top of U stack
@lp			ldx		,y
			jsr		ref$is$cons
			bne		prg$find$next						;if it's not a CONS
			ldx		CC_CAR_OFF,x
			cmpx	#ATOM_prg
			beq		@ret								;if found a 'prg'
prg$find$next:
			leay	2,y									;advance to next U stack entry
			cmpy	#ROOT_STACK_TOP-1
			blo		@lp									;if not top of U stack
@ret		rts
