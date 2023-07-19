; "repl.asm" - Read, Eval, Print Loop for Alf
; 2022-nov-18 dww  created

;@@@
;@@@ Read, Eval, Print Loop
;@@@

; >>> Initialize interpreter
repl		ldx		#@greet							;X = greeting message
bailx_		lds		#ALF_PS_TOP						;initialize stacks
			ldu		#ALF_RS_TOP
			clr		comp$							;executing, not compiling
			pshs	x								;0,PP = greeting string
			leay	repl$,pcr						;start the REPL!
			NEXT
;
@greet		fcb		9,'a','l','f',' ','v',ALF_VER+'0','.',ALF_REV+'0',NL

; >>> Initialize and start Read, Eval, Print Loop (REPL)
; Assumes we enter with a message to display on the top of the Param Stack.
repl$		fdb		prs								;display message
			fdb		def$can							;cancel any in-progress definition
			fdb		rdl$can							;cancel current line buffer
			fdb		eof								;terminate loading
;
@lp			fdb		stk$check						;check for stack overflow
			fdb		rd$tok							;read symbol or string
			fdb		jt$,@str						;if it's a string
;
; Look for the symbol at DP in the list of defined words.
; Execute it if we're not compiling or if the word should always be executed.
; Otherwise, compile it's code address at DP and continue.
;
			fdb		dp,defq							;look for it's definition
			fdb		jf$,@num?						;if it's not there
;
			fdb		def_tick						;get precedence, code address
			fdb		jt$,@exec						;if must execute it
			fdb		compq,jt$,@comp					;if compiling
;
@exec		fdb		go$								;execute it
			fdb		jp$,@lp							;get next
;
@comp		fdb		compw							;compile it
			fdb		jp$,@lp							;get next
;
; Check if it's a numeric constant and compile or push it if so
;
@num?		fdb		dp,numq							;try for number
			fdb		jf$,@undef						;if not a valid number
;
			fdb		compq,jf$,@lp					;leave it on stack if executing
			fdb		complw,pushlw,compw				;else compile code to push it
			fdb		jp$,@lp							;and process next
;
; Process a quoted string (in s$)
;
@str		fdb		compq,jt$,@comps				;if compiling
			fdb		str$							;else push addr of transient string
			fdb		jp$,@lp							;and process next
;
@comps		fdb		complw,pushls$					;compile push literal string word
			fdb		str$,comps						;then compile string
			fdb		jp$,@lp							;and process next
;
; Undefined word
;
@undef		fdb		dp,prs							;word is undefined
			fdb		bail$
			fcb		3,'?',BEL,NL

; >>> Bail out. Display message and restart the REPL.
; CONSUMES: IP = address of bail message string
			DEF		5,"bail$"
bail$		tfr		y,x								;X = message
			jmp		bailx_

; >>> Default prompt string (none)
alf$prompt	equ		0

; >>> Search for definition of a word
; CONSUMES: 0,PP = address of word name string
; PRODUCES: 0,PP = 0 if not found else 0,PP = 1, 2,PP = address of word's header
			DEF		4,"def?"
defq		pshs	y
;
			leay	defs$,pcr						;Y = addr of head of defs list
@clp		ldy		,y								;Y = next def in chain
			beq		@fail							;if end of chain
;
			pshs	y								;save start of list word def
			ldb		,y+
			andb	#$7F							;B = length of list word name
			ldx		4,s								;X = target name
			cmpb	,x+
			bne		@skip							;if lengths don't match
;
			bra		@dec							;first byte (lengths) already matched
@nlp		lda		,x+
			cmpa	,y+
			bne		@skip							;if doesn't match
@dec		decb
			bpl		@nlp							;if not end of name yet
;
			puls	d,y								;restore IP, D = def addr
			std		,s								;overwrite target name with word addr
			bra		push1							;and a 1 because found it
;
@skip		leas	2,s								;drop start of word
			leay	b,y								;Y = word's link addr
			bra		@clp							;try next
;
@fail		puls	y								;restore IP
			leas	2,s								;drop target name addr
			bra		push0							;push 0 becuase not found

; >>> Push constant 0
; PRODUCES: 0,PP = 0
			DEF		1,"0"
push0		clrb									;B = 0
@push		clra									;D = value
			pshs	d
			NEXT
;
; >>> Push constant 1
; PRODUCES: 0,PP = 1
			DEF		1,"1"
push1		ldb		#1								;B = 1
			bra		@push

; >>> Get info about a definition for a word
; CONSUMES: 0,PP = address of word header (name)
; PRODUCES: 0,PP = 0 if normal else 1 if always execute; 2,PP = code address
			DEF		4,"def\'"
def_tick	puls	x								;X = word address
			ldb		,x+								;B = word's name length + precedence
			tfr		b,a
;
			andb	#$7F							;B = name length
			leax	b,x
			leax	2,x								;X = word's code address
			pshs	x
;
			tsta
			bmi		push1							;if always execute
			bra		push0							;else normal word

; >>> Try to convert string to a number
; CONSUMES: 0,PP = addr of string
; PRODUCES: 0,PP = 0 if not a number else 0,PP = 1, 2,PP = value
			DEF		4,"num?"
numq		puls	x								;X = string addr
			ldb		,x+								;X = char addr, B = count
			beq		push0							;if empty string
			pshs	y								;save IP
			bsr		@neg?							;CS if failed else D = value
			puls	y
			bcs		push0							;if failed
			pshs	d								;else succeeded
			bra		push1
;
; Handle negative numbers
@neg?		lda		,x								;A = first char
			cmpa	#'-'
			bne		@base?							;if not negative number
			leax	1,x								;else eat the sign
			decb
			bsr		@base?							;try to convert
			bcs		@ret							;if failed
			coma
			comb
			addd	#1								;D = -D
@succ		andcc	#~CC_C							;return CC
@ret		rts
;
; Check for number base prefix (ala C: 0x = 16, 0b = 2, else 10)
@base?		cmpb	#2								;long enough for base prefix?
			blo		@dec							;nope
			lda		,x								;A = next char
			cmpa	#'0'
			bne		@dec							;if def not base prefix
			lda		1,x								;A = 2nd char
			cmpa	#'x'
			beq		@hex							;if want hex base
			cmpa	#'b'
			bne		@dec							;if not binary base
;
			lda		#2								;use base 2
			bra		@base
;
@hex		lda		#16								;use base 16
@base		leax	2,x								;eat the prefix
			subb	#2
			bra		@digs
;
@dec		lda		#10								;use base 10
;
; Collect digits of number
@digs		pshs	a,b								;0,s = base, 1,s = count
			tstb
			beq		@fail							;if no digits at all
;
			ldy		#0								;Y = value
@diglp		lda		,x+								;A = next digit
			bsr		@dig							;convert to digit value
			cmpa	,s
			bhs		@fail							;if it's not valid for our base
;
			pshs	a								;0,s = digit
			tfr		y,d
			lda		1,s								;A = base, B = low byte of value
			mul
			exg		d,y
			ldb		1,s								;A = high byte, B = base
			mul										;(could check for overflow here)
			exg		a,b
			puls	b
			leay	d,y								;Y = Y * base + digit
;
			dec		1,s
			bne		@diglp							;if there's more
;
; Valid number
			leas	2,s								;drop base, count
			tfr		y,d								;D = number
			bra		@succ							;return CC
;
; Not a valid number
@fail		leas	2,s								;drop base, count
			orcc	#CC_C							;return CS
			rts
;
; Convert char in A to digit value
; Returns known invalid digit (MSB set) if not a digit
@dig		cmpa	#'9'
			bhi		@let?							;if not decimal digit
			suba	#'0'
			bra		@ret							;valid digit or MSB set for bad one
;
@let?		anda	#$DF							;force lower case
			suba	#'A'
			blo		@ret							;if not valid letter return MSB set
			adda	#10								;A-Z -> 10-25 for bases up to 26
			cmpa	#10+'Z'-'A'
			bls		@ret							;if it's a valid letter digit
			lda		#$FF							;else return invalid digit
			rts

; >>> Read Symbol
; Reads next (whitespace delimited) symbol to DP, bails if they provide a string
rd$sym		WORDS
			fdb		rd$tok							;get next token
			fdb		jt$,@bail						;bail if got a string instead
			DONE
;
@bail		fdb		bail$							;string not allowed
			fcb		20,'S','t','r','i','n','g',' ','n','o','t',' ','a','l','l','o','w','e','d','!',BEL

; >>> Read Token
; PRODUCES: 0,PP = 0 if got symbol (at DP) else 1 if got quoted string (at s$)
rd$tok		bsr		@rdnc							;skip CTL chars
			cmpb	#' '
			beq		rd$tok							;and spaces
;
			cmpb	#DQUOT
			beq		@str							;if it's a quoted string
;
; Assume it's a symbol delimited by whitespace (max len 127 bytes)
; Copy it to DP.
;
			ldx		dp$								;stash it at DP
			clr		,x								;first byte is length
@symlp		inc		,x								;bump length
			bmi		@symtl							;if too long
			lda		,x
			stb		a,x								;stash char
			jsr		rdc_							;next char
			cmpb	#$20
			bhi		@symlp							;if not end
			jmp		push0							;push 0 for symbol
;
@symtl		leax	@symtlm,pcr						;bail on symbol too long
@bail		jmp		bailx_
@symtlm		fcb		17,'S','y','m','b','o','l',' ','t','o','o',' ','l','o','n','g','!',BEL
;
; It's a quoted string (max len 255)
; Copy it to s$ transient string buffer.
;
@str		pshs	y								;preserve IP
			leay	str$buf+1,pcr					;stash it in transient string buffer
			clr		,y								;init to empty string
@strlp		bsr		@rdnc							;next non-CTL char
			cmpb	#DQUOT
			beq		@eos							;if end of string
			cmpb	#BSLASH
			beq		@esc							;if escape prefix
;
@adds		inc		,y								;add B to string
			beq		@strtl							;if string > 255 bytes
			lda		,y
			cmpa	-1,y
			bhi		@strtl							;if dest is full
			tfr		y,x								;else store char
			exg		a,b								;A = char, B = UNSIGNED index
			abx
			sta		,x
			bra		@strlp							;get next
;
@esc		bsr		@rdnc							;B = escaped char
			leax	@escs,pcr						;X = list of special escapes (0 ends)
@esclp		tst		,x
			beq		@adds							;if not in list add it as-is
			cmpb	,x+
			beq		@escsub							;if it's a special escape
			leax	1,x
			bra		@esclp							;else try next
@escsub		ldb		,x								;B = special substitute char
			bra		@adds
;
@rdnc		jsr		rdc_							;read next char
			cmpb	#$1F
			bls		@rdnc							;ignoring controls
			rts
;
@eos		puls	y								;restore IP
			jmp		push1							;push 1 for string
;
@escs		fcb		'a',BEL,'b',BS,'e',ESC,'f',FF,'n',LF,'r',CR,'t',HT,'v',VT,'0',0
;
@strtl		leax	@strtlm,pcr						;string too long
			bra		@bail
@strtlm		fcb		17,'S','t','r','i','n','g',' ','t','o','o',' ','l','o','n','g','!',BEL

; >>> Are we compiling?
; PRODUCES: 0,PP = 0 if executing else 1 if compiling
			DEF		5,"comp?"
compq		ldb		comp$
			clra
			pshs	d
			NEXT

; >>> Set mode to executing
			DEF		_EXE+1,"{"
set_exec	clr		comp$
			NEXT

; >>> Set mode to compiling
			DEF		1,"}"
set_comp	ldb		#1
			stb		comp$
			NEXT

; >>> Execute a word
; CONSUMES: 0,PP = a word's code address
			DEF		3,"go$"
go$			rts											;jump to addr on stack

; >>> Marks currently being defined word (if any) as always execute.
; CONSUMES: 0,PP = word's header address
			DEF		_EXE+6,"<exec!"
			ldx		def$$								;X = word being defined
			beq		@next								;if not defining a word
			ldb		#_EXE								;turn on execute flag
			orb		,x
			stb		,x
@next		NEXT

; >>> Bail if not compiling (compile or die)
			DEF		_EXE+6,"comp?!"
comp_only	tst		comp$
			beq		@bail								;if not compiling
			NEXT
;
@bail		leax	@msg,pcr
			jmp		bailx_
@msg		fcb		15,'c','o','m','p','i','l','e',' ','o','n','l','y','!',BEL,NL

; >>> Inline comment (can span multiple lines)
; ( <comment> )
; NOTE: Also stops at EOT.
			DEF		_EXE+1,"("
			lda		#')'								;terminator
			bra		@comm
;
; >>> Single line comment, ends at NL or EOT
; (( <comment> NL
			DEF		_EXE+2,"(("
			lda		#NL
@comm		pshs	a									;save delimiter
@commlp		jsr		rdc_								;read next char
			cmpb	,s
			beq		@done								;if it's the delimiter
			cmpb	#EOT
			bne		@commlp								;if it's not EOF char
;
@done		leas	1,s
			NEXT

; >>> Bail with memory full error
mem$full	ldx		#@msg
			jmp		bailx_
@msg		fcb		13,'M','e','m','o','r','y',' ','f','u','l','l','!',BEL

; >>> Bail with stack overflow error

; >>> Check for stack overflow (hit DP)
; Bails if PP <= DP
			DEF		9,"stk$check"
stk$check	cmps	dp$
			bls	stk$over							;bail if stack overflowed
			NEXT
;
stk$over	ldx		#@msg
			jmp		bailx_
@msg		fcb		16,'S','t','a','c','k',' ','o','v','e','r','f','l','o','w','!',BEL
