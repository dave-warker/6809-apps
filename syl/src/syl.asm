; "syl.asm" - main file for 6809 Syl LISP by Dave Warker
; An attempt at a basic LISP 1.5ish LISP for the 6809 because (I have no idea.)
;
; MEMORY MAP FOR G6CC HARDWARE
; DEBUG											  EPROM
; $0000	+---------------------------------------+ $0000
;		|										|
;		| ATOM Name Pool						|
;		|										|
; $0800	+---------------------------------------+ $0800 <- DP
;		| Vars & CPU Stack						|
;		|+-------------------------------------+|
;		|| Garbage Collector Mark Pool		   ||
;		|+-------------------------------------+|
;		|| Interpreter Stack				   ||		<- S
;		|+-------------------------------------+|
; $1000	+---------------------------------------+ $1000
;		| Root Context Stack					|		<- U
; $1400	+---------------------------------------+ $1400
;		|										|
;		| CONS Cell Pool						|
;		|										|
; $7000	+---------------------------------------+ $7F00
;		| Syl Interpreter Code (Debug Build)	|
; $7F00	+---------------------------------------+ $7F00
;		| Reserved for Demon Debugger			|
; $8000	+---------------------------------------+ $8000
;
; REGISTER USAGE
;  DP	= points to start of VARS area
;  S	= Interpreter CPU Stack Pointer
;  U	= Root Pointers Stack Pointer for GC
;  X	= Scratch, preferred to pass CONS Cell Slot Ref arguments
;  Y,D	= Scratch
; Don't expect the D register to be preserved unless stated otherwise.

			pragma	6809,dollarnotlocal,cescapes

			pragma	nolist
			IFDEF PG09
			include "pg09.lib"
			ELSE
			include	"g6cc.lib"
			ENDIF
			include "ascii.lib"
			pragma	list

;@@@
;@@@ EQUATES
;@@@

; Metadata
SYL_VER				equ		1								;major version
SYL_REV				equ		3								;minor revision

; Memory Map
ATOM_POOL_ORG		equ		$0000							;ATOM names pool (MUST BE $0000!)
ATOM_POOL_SIZE		equ		$0800
ATOM_POOL_END		equ		ATOM_POOL_ORG+ATOM_POOL_SIZE
VARS_ORG			equ		ATOM_POOL_END					;Variables + CPU Stack
VARS_SIZE			equ		$0800
VARS_END			equ		VARS_ORG+VARS_SIZE
VARS_STACK_TOP		equ		VARS_END						;initial stack pointer
ROOT_STACK_ORG		equ		VARS_END						;GC Root Pointers Stack
ROOT_STACK_SIZE		equ		$0400
ROOT_STACK_END		equ		ROOT_STACK_ORG+ROOT_STACK_SIZE
ROOT_STACK_TOP		equ		ROOT_STACK_END
CONS_POOL_ORG		equ		ROOT_STACK_END					;CONS Cells Pool
	IFDEF DEBUG
CONS_POOL_END		equ		$7000							;end in DEBUG build
CODE_ORG			equ		CONS_POOL_END					;Interpreter Code area (in RAM)
CODE_END			equ		$7F00							;end address (below Demon vars)
CODE_SIZE			equ		CODE_END-CODE_ORG
	ELSE
CONS_POOL_END		equ		$7F00							;end in EPROM build
	IFDEF PG09
	IFDEF PG09_BROM
CODE_ORG			equ		$C000							;Interpreter Code area (in Banked ROM)
	ELSE
CODE_ORG			equ		$A000							;Interpreter Code area (in High Banked RAM)
	ENDIF ; PG09_BROM
	ELSE
CODE_ORG			equ		$D000							;Interpreter Code arera (in EPROM)
	ENDIF
CODE_SIZE			equ		$1000
CODE_END			equ		CODE_ORG+CODE_SIZE
	ENDIF
CONS_POOL_SIZE		equ		CONS_POOL_END-CONS_POOL_ORG

; Console
NL					equ		LF								;newline character
CON_BUF_SIZE		equ		80								;size of the line input buffer
CON_BUF_EMPTY		equ		-1								;next char offset when buffer is empty

TOK_LPAR			equ		'('								;token for left paren (
TOK_RPAR			equ		')'								; right paren )
TOK_DOT				equ		'.'								; dot separator .
TOK_QUOTE			equ		SQUOT							; quote operator '
TOK_INT				equ		'0'								; integer value
TOK_ATOM			equ		'#'								; atom reference

CC_C				equ		$01								;Carry flag in condition code register
CC_V				equ		$02								;oVerflow flag
CC_Z				equ		$04								;Zero flag
CC_N				equ		$08								;Negative flag
CC_I				equ		$10								;Interrupt flag
CC_H				equ		$20								;Half carry flag
CC_F				equ		$40								;Fast interrupt flag
CC_E				equ		$80								;Entire register set flag

;@@@
;@@@ CONS CELLS
;@@@
;
; CONS CELL SLOT VALUES:
; $0000 - $07FF: ATOM Name Reference ($0000 = NIL)
; $0800 - $0FFF: Refs to Built-In Functions (BIVs, BIFs, BIQs)
; $1400 - $7FFC: CONS Cell References
; $8000 - $FFFF: 15 bit Signed Integers
;
; NOTE: NIL ATOM *must* be the first allocated to ensure it's ATOM
; value is 0! Using this setup ATOM and CONS refs are the same as
; their actual memory address which makes lots of stuff easier.
; The address range used by VARS is used for built-ins and are
; the offset from bis$base to the actual 6809 code.

CC_SLOT_SIZE		equ		2								;size of a slot in a CONS Cell
CC_SIZE				equ		2*CC_SLOT_SIZE					;size of a Cell (car + cdr)
CC_CAR_OFF			equ		0*CC_SLOT_SIZE					;offset to CAR slot
CC_CDR_OFF			equ		1*CC_SLOT_SIZE					;offset to CDR slot
;
CCS_TYPE_MASK		equ		$8000							;mask for slot type
CCS_TYPE_REF		equ		$0000							; value for references (ATOM or CONS)
CCS_TYPE_INT		equ		$8000							; value for 15 bit signed integer
;
CCS_INT_MASK		equ		$7FFF							;mask for integer value
CCS_INT_MIN			equ		-16383							;smallest integer
CCS_INT_MAX			equ		16383							;largest integer
;
CCS_REF_NIL			equ		ATOM_POOL_ORG					;should be 0
CCS_REF_ATOM_END	equ		ATOM_POOL_END					;refs below this are ATOM refs
CCS_REF_BIS_FIRST	equ		VARS_ORG						;refs to built-ins
CCS_REF_BIS_END		equ		VARS_END
CCS_REF_CONS_FIRST	equ		CONS_POOL_ORG					;refs starting here are CONS refs
CCS_REF_CONS_END	equ		CONS_POOL_END					;CONS refs must be < this

CONS_POOL_COUNT		equ		CONS_POOL_SIZE/CC_SIZE			;total Cells in the CONS pool
GC_MARK_POOL_SIZE	equ		CONS_POOL_COUNT/8				;size (bytes) of the Garbage Collector Mark pool

;@@@
;@@@ VARIABLES (REFERENCE VIA DP REG)
;@@@

		org		VARS_ORG
		setdp	VARS_ORG/256

cons_free_list	rmb		2									;first free CONS cell or CCS_REF_NIL
cons_free_count	rmb		2									;cells in the free list
cons_unalloc	rmb		2									;start of unallocated CONS space
atom_unalloc	rmb		2									;start of unallocated ATOM space
env_globals		rmb		2									;addr/ref to global environment
;
lex$tok$start	rmb		2									;buf offset to start of last token
read$root		rmb		2									;saved root stack pointer
sexp$par$depth	rmb		1									;current paren depth for parsing
;
con_buf_len		rmb		2									;total chars in the buffer
con_buf_next	rmb		2									;offset for next input char or CON_BUF_EMPTY
con_buf			rmb		CON_BUF_SIZE						;console input buffer
con_loading		rmb		1									;non-zero if we're loading from console
;
gc_mark_pool	rmb		GC_MARK_POOL_SIZE					;1 bit/cell pool of markers for GC
;
VARS_STACK_SIZE	equ		VARS_STACK_TOP-*					;available hardware stack space

;@@@
;@@@ CODE
;@@@

		org		CODE_ORG
;
; $00: Cold start entry
;
cold_start:
			lda		#VARS_ORG/256							;initialize Direct Page
			tfr		a,dp
			lds		#VARS_STACK_TOP							; and stack
			ldu		#ROOT_STACK_TOP							; and context stack
			jsr		con$init								; and console
;
; Initialize pools
;
			lda		#CON_BUF_EMPTY							;input buffer is empty
			sta		<con_buf_next
			ldd		#CONS_POOL_ORG							;all CONS are unallocated
			std		<cons_unalloc
			clra											;free list is empty
			clrb
			std		<cons_free_list
			std		<cons_free_count
;
			ldx		#gc_mark_pool							;ensure GC mark pool is zeroed
@gcpoollp	std		,x++
			std		,x++
			cmpx	#gc_mark_pool+GC_MARK_POOL_SIZE
			blo		@gcpoollp
;
; Pre-defined ATOMs
;
			ldx		#ATOM_POOL_ORG							;initialize ATOM pool
@atomlp		ldb		default_atoms-ATOM_POOL_ORG,x
			stb		,x+
			cmpx	#ATOM_POOL_ORG+end_default_atoms-default_atoms
			blo		@atomlp
			stx		<atom_unalloc
;
; Global environment & default bindings
;
			jsr		env$alloc								;allocate the global environment
			stx		<env_globals
;
			leay	default_bindings,pcr					;list of prebound ATOMs
@bindlp		jsr		env$add$default$binding					;add an entry
			cmpy	#end_default_bindings
			blo		@bindlp									;if not done yet
;
			bsr		bail									;display startup banner
			fcb		8,'S','y','l',' ','v',SYL_VER+'0','.',SYL_REV+'0'
;
; Restart REPL
;
bail:		puls	x										;X = message
			lds		#VARS_STACK_TOP
			ldu		#ROOT_STACK_TOP
			ldd		<env_globals
			pshu	d
;
			jsr		con$msg									;display message
			jsr		con$nl
			jsr		con$loaded								;not loading
			jsr		con$canl								;force new input line
;
; Read, Eval, Print Loop (REPL)
;
repl:		jsr		read									;Read
			jsr		eval									;Eval
			jsr		print									;Print
			jsr		con$nl
			bra		repl
 
; >>> Bail w/ref display
; PASSED:  X = ref to be displayed; ,S = addr of string to display after
bail$ref:	jsr		print									;display the item
			ldb		#':'
			jsr		con$putc
			bra		bail									;display the message and resume
 
;@@@
;@@@ DEFAULT UNBOUND IDEMPOTENT (SELF-VALUED) ATOMS
;@@@ NOTE: NIL MUST BE THE FIRST SO IT'S ATOM REF IS 0
;@@@
; These ATOMs are used internally and are never bound
; to a specific value. ATOM_NIL *must* be the first so
; it's ref will be 0 which some parts of the code assume.
; The lower case ATOMs are used internally and can never
; be referenced outside of the interpreter.

default_atoms:
ATOM_NIL	equ		*-default_atoms+ATOM_POOL_ORG		;NIL must be 0!
			fcb		3,'N','I','L'
ATOM_T		equ		*-default_atoms+ATOM_POOL_ORG		;Standard truth ATOM
			fcb		1,'T'
ATOM_env	equ		*-default_atoms+ATOM_POOL_ORG		;tag for environments on the U stack
			fcb		3,'e','n','v'
ATOM_prg	equ		*-default_atoms+ATOM_POOL_ORG		;tag for PROGs on the U stack
			fcb		3,'p','r','g'
ATOM_ret	equ		*-default_atoms+ATOM_POOL_ORG		;tag used by RETURNs in a PROG
			fcb		3,'r','e','t'
ATOM_QUOTE	equ		*-default_atoms+ATOM_POOL_ORG
			fcb		5,'Q','U','O','T','E'
ATOM_FN		equ		*-default_atoms+ATOM_POOL_ORG		;function with eval'ed args
			fcb		2,'F','N'
ATOM_FNQ	equ		*-default_atoms+ATOM_POOL_ORG		;function with uneval'ed args
			fcb		3,'F','N','Q'
end_default_atoms equ *
ATOM_LAST_IDEM	equ	end_default_atoms-default_atoms+ATOM_POOL_ORG

;@@@
;@@@ DEFAULT GLOBAL BINDINGS
;@@@

BIND		macro	noexpand
			fcb		{1}				;length of the name
			fcc		{2}				;the name
			fdb		{3}				;associated value
			endm

; Default binding (in roughly reverse order of popularity)
default_bindings:
			BIND	4,"LOAD",BIF_LOAD
			BIND	6,"LOADED",BIF_LOADED
			BIND	4,"READ",BIF_READ
			BIND	4,"EVAL",BIF_EVAL
			BIND	5,"PRINT",BIF_PRINT
			BIND	7,"PRINTLN",BIF_PRINTLN
			BIND	2,"NL",BIF_NL
			BIND	3,"SYL",BIV_VER
			BIND	3,"ENV",BIV_ENV
			BIND	4,"FREE",BIV_FREE
			BIND	2,"GC",BIQ_GC
;
			BIND	4,"ATOM",BIF_ATOM
			BIND	4,"NULL",BIF_NULL
			BIND	2,"EQ",BIF_EQ
			BIND	5,"EQUAL",BIF_EQUAL
			BIND	3,"AND",BIQ_AND
			BIND	2,"OR",BIQ_OR
			BIND	3,"NOT",BIF_NOT
;
			BIND	3,"LET",BIQ_LET
			BIND	3,"SET",BIF_SET
			BIND	4,"SETQ",BIQ_SETQ
			BIND	3,"DEF",BIF_DEF
			BIND	6,"RPLACA",BIF_RPLACA
			BIND	6,"RPLACD",BIF_RPLACD
;
			BIND	1,"=",BIF_EQ
			BIND	2,"!=",BIF_NE
			BIND	1,"<",BIF_LT
			BIND	1,">",BIF_GT
			BIND	2,"<=",BIF_LE
			BIND	2,">=",BIF_GE
;
			BIND	3,"REM",BIF_REM
			BIND	3,"MIN",BIF_MIN
			BIND	3,"MAX",BIF_MAX
			BIND	1,"|",BIF_BIT_OR
			BIND	1,"&",BIF_BIT_AND
			BIND	1,"^",BIF_BIT_XOR
			BIND	1,"+",BIF_ADD
			BIND	1,"-",BIF_SUB
			BIND	1,"*",BIF_MUL
			BIND	1,"/",BIF_DIV
;
			BIND	4,"PROG",BIQ_PROG
			BIND	2,"GO",BIQ_GO
			BIND	6,"RETURN",BIF_RETURN
			BIND	4,"COND",BIQ_COND
			BIND	4,"LIST",BIF_LIST
			BIND	3,"CAR",BIF_CAR
			BIND	3,"CDR",BIF_CDR
			BIND	4,"CADR",BIF_CADR
			BIND	5,"CADDR",BIF_CADDR
			BIND	4,"CDDR",BIF_CDDR
			BIND	4,"CONS",BIF_CONS
end_default_bindings equ *

;@@@
;@@@ MODULES
;@@@

		include	"lex.asm"
		include	"read.asm"
		include "eval.asm"
		include	"env.asm"
		include	"print.asm"
		include	"cons.asm"
		include	"gc.asm"
		include	"bis.asm"
		include	"misc.asm"
		include	"con.asm"

syl_end	equ *

;@@@
;@@@ ASSERTIONS
;@@@

	IFGT syl_end-CODE_END
	ERROR "CODE DOESN'T FIT!"
	ENDIF

	IF CC_CAR_OFF
	ERROR "Code optimizations require CC_CAR_OFF to be 0!"
	; This gives us a "free" CAR via [] in some cases
	ENDIF

	IF ATOM_NIL
	ERROR "ATOM_NIL must be 0!"
	; So we can rely on EQ for end of lists among other optimizations
	ENDIF

	IF GC_MARK_POOL_SIZE&(4-1)
	ERROR "GC Mark Pool init wants pool size a multiple of 4 for speed"
	ENDIF

		end	CODE_ORG
