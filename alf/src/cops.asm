; "cops.asm" - control operations for Alf
; NOTE: Control structures can only be used in compiled words, not in execute mode.
; They typically use the Return Stack both during compilation and sometimes during
; execution to hold control data such as jump addresses. Mucking with the RS while
; in a control structure is a recipe for disaster.
; 2022-nov-18 dww  created

;@@@
;@@@ BASIC CONTROL WORDS
;@@@

; >>> Unconditional jump
			DEF		3,"jp$"
jp$			ldy		,y
			NEXT

; >>> Jump if false (top of stack == 0)
; CONSUMES: 0,PP = condition
			DEF		3,"jf$"
jf$			ldd		,s++
			beq		jp$								;if false
@skip		leay	2,y								;else skip addr
			NEXT
;
; >>> Jump if true (top of stack != 0)
; CONSUMES: 0,PP = condition
			DEF		3,"jt$"
jt$			ldd		,s++
			bne		jp$								;if true
			bra		@skip

;@@@
;@@@ IF STATEMENTS
;@@@ <expr> if <words> then
;@@@ <expr> if <words> else <words> then
;@@@ opt <expr> do <words> or <expr> do <words> else <words> then
;@@@ While compiling: RP+0 = prev jump false operand, RP+2 = chain to end of statement
;@@@

; >>> 'if' statement (compile only)
; PRODUCES: 0,RP = head of end of statement chain = 0; 2,RP = jf$ operand addr
; NOTE: if/else/then doesn't really need both but we maintain them so else/then
; will word for opt statements.
			DEF		_EXE+2,"if"
			WORDS
			fdb		comp_only,rpop					;bail if not compiling, save rtn addr
			fdb		complw,jf$						;compile jump to false code
			fdb		dp,rpush,complw,0				;2,PP = addr of jf$ operand for later
			fdb		rpush0							;0,RP = 0 = head of end of statement chain
			fdb		rpush							;restore rtn addr
			DONE

; >>> 'else' part of 'if' statement
; CONSUMES: 0,RP = head of end of statement chain; 2,RP = prev jf$ operand addr
; PRODUCES: 0,RP = head of end of statement chain; 2,RP = 0
			DEF		_EXE+4,"else"
			WORDS
			fdb		comp_only,rpop					;bail if not compiling, save rtn addr
			fdb		complw,jp$,dp,rpop,compw		;compile jump to end of statement
			fdb		dp$chain						;fill in jf$ operand to here
			fdb		rpush,rpush0					;2,RP = end chain; 0,RP = no jf$ operand
			fdb		rpush							;restore return addr
			DONE

; >>> 'then' part of 'if' statement
; CONSUMES: 0,RP = head of end of statement chain; 2,RP = prev jf$ operand addr
			DEF		_EXE+4,"then"
			WORDS
			fdb		comp_only,rpop					;bail if not compiling, save rtn addr
			fdb		dp$chain,dp$chain				;fill in end chain, prev jump operand
			fdb		rpush
			DONE

; >>> 'opt' start of multi-way if statement
; PRODUCES: 0,RP = head of end statement chain = 0; 2,RP = jf$ operand addr = 0
			DEF		_EXE+3,"opt"
			WORDS
			fdb		comp_only,rpop					;bail if not compiling, save rtn addr
			fdb		rpush0,rpush0					;no prev jump, end statement chains yet
			fdb		rpush
			DONE

; >>> 'do' part of multi-way if statement
; PRODUCES: 0,RP = unchanged, 2,RP = addr of jf$ operand
			DEF		_EXE+2,"do"
			WORDS
			fdb		comp_only,rpop					;bail if not compiling, save rtn addr
			fdb		complw,jf$,dp,push0,compw		;compile jump if false to next 'do'
			fdb		rswap,rdrop,rpush,rswap			;replace prev jf$ addr with ours
			fdb		rpush
			DONE

; >>> 'or' part of multi-way if statement
; CONSUMES: 2,RP = backfills prev jf$ to here if needed
; PRODUCES: 0,RP = added a jp$ addr to end chain, 2,RP = 0
			DEF		_EXE+2,"or"
			WORDS
			fdb		comp_only,rpop					;bail if not compiling, save rtn addr
			fdb		complw,jp$,dp,rpop,compw		;compile jump to end
			fdb		dp$chain,rpush0,rpush			;fill in prev jf$ to here, clear it
			fdb		rpush
			DONE

;@@@
;@@@ LOOP STATEMENTS
;@@@ loop {<expr> while} <words> {<expr> stopif} <words> (again | <expr> until)
;@@@ <count> times <words> {<expr> stopif} <words> ( next | <expr> +next )
;@@@ <from> <to> for <words> {<expr> stopif} <words> ( next | <expr> +next )
;@@@

; >>> Start of a loop
			DEF		_EXE+4,"loop"
			WORDS
			fdb		comp_only						;bail if not compiling
			fdb		rpop
			fdb		rpush0,dp,rpush					;2,RP = end of loop chain (0), 0,RP = top of loop
			fdb		rpush
			DONE

; >>> Conditional start of loop
			DEF		_EXE+5,"while"
			WORDS
			fdb		rpop,rpop						;save return, top of loop
			fdb		pushlw,jf$						;compiling jump if false to end of loop
@while		fdb		comp_only						;bail if not compiling
			fdb		compw,dp,rpop,compw,rpush		;compile jump, add operand addr to chain
			fdb		rpush,rpush						;restore return, top of loop
			DONE
;
; >>> Conditional stop loop
			DEF		_EXE+6,"stopif"
			WORDS
			fdb		rpop,rpop						;save return, top of loop
			fdb		pushlw,jt$						;compiling jump if true to end of loop
			fdb		jp$,@while						;finish same as while

; >>> Unconditional end of loop
			DEF		_EXE+5,"again"
			WORDS
			fdb		comp_only						;bail if not compiling
			fdb		rpop
			fdb		complw,jp$,rpop,compw			;compile jump back to top
			fdb		dp$chain						;backfill end of loop refs
			fdb		rpush
			DONE

; >>> Conditional end of loop
			DEF		_EXE+5,"until"
			WORDS
			fdb		comp_only						;bail if not compiling
			fdb		rpop
			fdb		complw,jf$,rpop,compw			;compile jump if false back to top
			fdb		dp$chain						;backfill end of loop refs
			fdb		rpush
			DONE

; >>> Fill in backward chain of refs to current DP
; CONSUMES: 0,RP = 0 or addr of last link in chain
dp$chain	ldd		dp$								;D = value to fill with
			pulu	x								;X = head of chain
			pshs	y								;preserve IP
;
@lp			cmpx	#0
			beq		@next							;if hit end of chain
			ldy		,x								;next link in chain
			std		,x								;fill in value
			tfr		y,x								;follow the chain
			bra		@lp
;
@next		puls	y
			NEXT

; >>> Loop N times
; CONSUMES: 0,PP = number of times to repeat loop
			DEF		_EXE+5,"times"
			WORDS
			fdb		complw,sub1,complw,rpush		;runtine: push count - 1 to RP
			fdb		complw,rpush0					;runtime: push 0 to RP
			fdb		jp$,for$						;continue same as 'for'
;
; >>> Loop from start to end value
; NOTE: Assumes values are signed
			DEF		_EXE+3,"for"
			WORDS
			fdb		complw,rpush,complw,rpush		;runtime: 0,RP = lower, 2,RP = upper
for$		fdb		comp_only						;bail if not compiling
			fdb		rpop
			fdb		dp								;remember top of loop
			fdb		complw,for$_					;runtime: compares limits and jumps
			fdb		dp,push0,compw					;fill in loop end later
			fdb		rpush,rpush						;0,RP = top of loop, 2,RP = chain for end
			fdb		rpush
			DONE

; >>> Runtime top of for loop: compare limits and jump if done
for$_		ldd		,u								;U = loop var value
			cmpd	2,u
			bgt		@done							;if time to end loop
			leay	2,y								;else skip end address
			NEXT									;and continue
;
@done		ldy		,y								;jump to end of loop
			NEXT

; >>> Next 'for' iteration (increment by 1)
			DEF		_EXE+4,"next"
			WORDS
			fdb		complw,next$1_					;compile increment by 1 word
			fdb		jp$,@next						;rest same as 'next+'
;
; >>> Next 'for' iteration, arbitrary increment
			DEF		_EXE+5,"+next"
			WORDS
			fdb		complw,next$_					;compile increment by N word
@next		fdb		comp_only						;bail if not compiling
			fdb		rpop
			fdb		rpop,compw						;target addr is top of loop
			fdb		dp$chain						;backfill end of loop to here
			fdb		complw,rdrop,complw,rdrop		;runtime: drop loop limits
			fdb		rpush
			DONE

; >>> Runtime next 'for' loop iteration
; Increments limit and jumps back to the top
next$_		puls	d								;D = increment
			bra		@next
next$1_		ldd		#1								;D = default increment
@next		addd	,u								;add to 0,RP = loop var
			std		,u
			ldy		,y								;jump to top of loop
			NEXT

; >>> Push innermost 'for' loop's current var
; PRODUCES: 0,PP = loop value
			DEF		1,"i"
			ldd		,u								;D = innermost loop value
@push		pshs	d
			NEXT
;
; Push innermost loop's maximum value
; PRODUCES: 0,PP = loop max value
			DEF		4,"imax"
			ldd		2,u								;D = innermost loop max value
			bra		@push
;
; >>> Push 2nd innermost 'for' loop's current var
; PRODUCES: 0,PP = loop value
			DEF		1,"j"
			ldd		4,u								;D = 2nd innermost loop value
			bra		@push
;
; Push 2nd innermost loop's maximum value
; PRODUCES: 0,PP = loop max value
			DEF		4,"jmax"
			ldd		6,u								;D = 2nd innermost loop max value
			bra		@push
;
; >>> Push 3rd innermost 'for' loop's current var
; PRODUCES: 0,PP = loop value
			DEF		1,"k"
			ldd		8,u								;D = 3rd innermost loop value
			pshs	d
			NEXT
;
; Push 3rd innermost loop's maximum value
; PRODUCES: 0,PP = loop max value
			DEF		4,"kmax"
			ldd		10,u							;D = 3rd innermost loop max value
			bra		@push
