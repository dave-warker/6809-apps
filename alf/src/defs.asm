; "defs.asm" - mechanics of defining words for Alf
; 2022-nov-18 dww  created

;@@@
;@@@ VARIABLES
;@@@

; >>> Current Dictionary Pointer value
; PRODUCES: 0,PP = current DP value
			DEF		2,"dp"
dp			VAL
			fdb		dp$

; >>> Address of current dictionary pointer Dictionary Pointer
; PRODUCES: 0,PP = address of current Dictionary Pointer
			DEF		3,"dp&"
dpa			VAR
			fdb		dp$

; >>> Push current free space count.
; PRODUCES: 0,PP = number of bytes between DP and PP
			DEF		4,"free"
free		tfr		s,d
			subd	dp$
			pshs	d
			NEXT

; >>> Return head of definitions list.
; PRODUCES: 0,PP = addr of most recently defined word
			DEF		4,"defs"
defs		VAL
			fdb		defs$

; >>> Return address of word being defined
; PRODUCES: 0,PP = address of word currently being defined or 0
			DEF		4,"def$"
def$		VAL
			fdb		def$$

;@@@
;@@@ WORDS
;@@@

; >>> Begin defining a new word
			DEF		1,":"
			WORDS
			fdb		def$bgn							;compile word header
			fdb		complw							;compile JSR <words
			fcb		$9D,words$%256
			fdb		set_comp						;change to compile mode
			DONE

; >>> Compile header for a new word
def$bgn		WORDS
			fdb		def$can							;cancel any in-progress definition
			fdb		dp,pushlw,def$$,store			;save current DP as start of word in-progress
			fdb		rd$sym							;get name to DP
			fdb		comps$dp						;compile name at DP
			fdb		defs,compw						;compile link to current head
			fdb		def$,pushlw,defs$,store			;word is new defs head
			DONE

; >>> Cancel any current word definition
def$can		ldx		def$$
			beq		@next							;if no definition in progress
			clr		def$$+0
			clr		def$$+1							;not defining a word
			jmp		forgetx$						;remove the in-progress word
;
@next		NEXT

; >>> Add string at DP to dictionary
comps$dp	ldx		dp$								;X = DP = start of string
			ldb		,x+								;B = string length, X = content start
			abx										;X = addr after string
			jsr		<set$dp							;update DP & check for mem full
			NEXT

; >>> Finish defining a new word
			DEF		1+_EXE,";"
			WORDS
			fdb		complw,done$					;compile return from word list
			fdb		def$end							;finish up definition
			fdb		set_exec						;back to execute mode
			DONE

; >>> Finish up current definition
def$end		WORDS
			fdb		def$clr							;no in-progress def
			DONE
;
def$clr		clr		def$$+0
			clr		def$$+1							;not defining a word
			NEXT

; >>> Allocate space at current DP
; CONSUMES: 0,PP = number of bytes to allocate
			DEF		6,"alloc$"
alloc$		leax	dp$,pcr
			puls	d								;D = byte count
			addd	,x								;update DP
			std		,x
			jsr		<mem$check						;check for memory full
			NEXT

; >>> Compile byte on top of stack
			DEF		1,"."
compb		puls	d								;B = byte from stack
@compb		ldx		dp$								;store byte in dict
			stb		,x+
			stx		dp$
			NEXT

; >>> Compile word on top of stack.
; CONSUMES: 0,PP = word value
			DEF		1,"\,"
compw		puls	d								;D = word from stack
@compw		ldx		dp$								;store word in dict
			std		,x++
			stx		dp$
			NEXT
;
; >>> Compile literal word (pointed to by IP)
			DEF		4,"lit\,"
complw		ldd		,y++							;D = next word in list
			bra		@compw							;store it in dict

; >>> Push literal word (at IP)
; PRODUCES: 0,PP = value of next word
pushlw		ldd		,y++							;Y = word value
			pshs	d
			NEXT

; >>> Start a code word definition
; ASM: <name> ... NEXT;
; Stays in execute mode for word definition.
; Does NOT add JSR <words$ at start of definition.
; Intended for raw assembly code words.
			DEF		4,"ASM:"
			WORDS
			fdb		def$bgn
			DONE

; >>> Ends a code word definition
; Assumes we're defining a code word.
; Compiles NEXT and finishes up the definition.
			DEF		5,"NEXT;"
			WORDS
			fdb		complw,OP_NEXT					;compile NEXT (JMP [,Y++])
			fdb		def$end							;finalize definition
			DONE

; >>> Find word's definition or bail
; find <name> -> <addr>
			DEF		4,"find"
find		WORDS
			fdb		rd$sym							;get name to DP
			fdb		dp,defq,jf$,@nf					;find in dictionary, jump if not found
			DONE
;
@nf			fdb		bail$							;not found
			fcb		11,'N','o','t',' ','f','o','u','n','d','!',BEL

; >>> Forget (delete) definitions
; forget <word>
			DEF		6,"forget"
			WORDS
			fdb		find							;push addr of word or bail
			fdb		@forget							;forget to that point
			DONE
;
@forget		puls	x								;X = start of word
forgetx$	cmpx	dp$
			bhs		@fail							;if it's not in RAM DP space
;
			jsr		<set$dp							;else is new end of dict space
			ldb		,x+								;skip to link addr
			abx
			ldd		,x								;link is new head of defs
			std		defs$
			NEXT
;
@fail		ldx		#@msg
			jmp		bailx_
@msg		fcb		15,'U','n','f','o','r','g','e','t','t','a','b','l','e','!',BEL
