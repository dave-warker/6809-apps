; "ker.asm" - base of kernel code for Alf
; 2022-dec-13 dww  created
;
; This contains the start of the read/only kernel code.
; The first 256 bytes are accessible via the DP register.
; It starts with a header for use by the Demon debugger.

;@@@
;@@@ HEADER FOR USE BY THE DEMON DEBUGGER
;@@@

			jmp		cold_start						;$00: cold start entry
			jmp		warm_start						;$03: warm start entry

;@@@
;@@@ DP ACCESSIBLE CODE (MUST RESIDE IN DIRECT PAGE!)
;@@@

; >>> Begin executing a new word list
; CONSUMES:	0,PP = address of new list of words
words$		sty		,--u							;push old IP to Return Stack
			puls	y								;pop new one from Param Stack
next$		NEXT									;execute first word in new list

; >>> Return to previous word list
; CONSUMES:	0,RP = word list address to resume execution
done$		pulu	y								;restore IP from Return Stack
			NEXT									;continue execution

; >>> Push constant value (also used to push variable address)
; PRODUCES: 0,PP = word at address on stack (return address of JSR)
const$		ldd		[,s]							;D = value at return address
@store		std		,s								;replaces address
			NEXT
var$		equ		const$							;also used for variable address
;
; >>> Push value of a variable
; PRODUCES: 0,PP = word at address given by word on stack (return address of JSR)
val$		ldx		[,s]							;X = address of value
			ldd		,x								;D = value
			bra		@store
;
; >>> Push address of string's length byte
; CONSUMES: 0,PP = address of string max length
; PRODUCES: 0,PP = address of string cur length byte (max + 1)
chars$		ldx		,s								;X = addr of string max length
			leax	1,x								;X = addr of string cur length
			stx		,s
			NEXT

; >>> Subroutine to set DP w/memory full check
; PASSED:  X = new DP value
; Bails if DP > PP
set$dp		stx		dp$								;set new DP
mem$check	cmps	dp$
			lbls	mem$full						;bail if memory is full
			rts

;@@@
;@@@ INITIALIZATION
;@@@

cold_start	clr		warm$							;force cold start
warm_start	lds		#ALF_PS_TOP						;ensure valid stack
			lda		#ALF_DP_ORG/256					;and direct page
			tfr		a,dp
			jsr		con$init						;initialize console I/O
;
			ldd		#ALF_WARM_ID
			cmpd	warm$
			beq		@warm							;if can try warm start
			std		warm$							;else do a cold start
			ldd		#alf_dp$						;init start of dict
			std		dp$
			ldd		#alf_defs$						;and head of defs chain
			std		defs$
			ldd		#0
			std		def$$							;no current definition
;
@warm		lda		#STR_MAX_LEN					;ensure proper buffer sizes
			sta		str$buf
			lda		#RBC_BUF_LEN
			sta		rdc$buf
			jmp		repl							;start up the loop
