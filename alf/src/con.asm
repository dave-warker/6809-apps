; "con.asm" - console I/O for Alf
; 2022-nov-18 dww  created

;@@@
;@@@ CORE CONSOLE I/O
;@@@

; >>> Start loading a file from the console
; Sets the loading flag which causes us to emit XOFF/XONs to the console
; as needed to keep it from sending charactets faster than we can process them.
; Basically we keep the console paused until we need a line of input.
			DEF		4,"load"
			lda		#1								;set flag that we're loading
			sta		loading$
			jsr		con$pause						;pause console until we need data
			NEXT

; >>> Stop loading (end of file)
			DEF		3,"eof"
eof			bsr		eof_							;stop loading
			NEXT
;
eof_		tst		loading$						;are we loading?
			beq		@ret							;nope
			clr		loading$						;yep, turn off loading state flag
			jsr		con$resume						;make sure console isn't paused
@ret		rts

; >>> Read next char from the console buffer
; rdc -> <char>
			DEF		3,"rdc"
rdc			bsr		rdc_							;B = read char
			clra
			pshs	d
			NEXT
;
rdc_		pshs	x
@try		ldx		#rdc$buf+1						;X = addr of line buffer cur length
			lda		rdc$next						;A = index for next char
			bmi		@rdl							;if need to read a line
			inca									;bump to actual index (and next char)
			beq		@eol							;if wrap then must be end of line
			cmpa	,x
			bhi		@eol							;if hit current end of buffer
			ldb		a,x								;B = char
@gotc		sta		rdc$next						;update index
			clra									;D = char
			puls	x
			rts
;
@rdl		bsr		rdl_							;read line to X
			clr		rdc$next						;buffer not empty now
			bra		@try
;
@eol		ldd		#RBC_BUF_EMPTY*256+NL			;mark buf empty, return NL
			bra		@gotc

; >>> Cancel current line buffer contents
rdl$can		ldb		#RBC_BUF_EMPTY
			stb		rdc$next
			NEXT

; >>> Read a line from the console with optional prompt
; <buf> rdl
			DEF		3,"rdl"
			puls	x								;X = string buffer address
			bsr		rdl_
			NEXT
;
; PASSED:  X = addr of buffer (-1,x = max len; 0,X = current len, 1,X = start of contents)
rdl_		clr		,x								;char count = 0
			tst		loading$
			beq		@lp								;if not loading from console
			jsr		con$resume						;else resume console input
@lp			jsr		con$rdcq						;wait for next (unbuffered) input char
			beq		@lp
			cmpb	#$20
			blo		@ctls							;if it's a control char
			cmpb	#$7E
			bhi		@lp								;ignore non-printables
;
			lda		,x								;A = current count
			inca									;want to add one more
			beq		@lp								;can't if wrap (sanity check)
			cmpa	-1,x
			bhs		@lp								;or if it's full
			sta		,x								;update current length
			stb		a,x								;store char in buffer
			tst		loading$
			bne		@lp								;don't echo if loading
			bsr		prc_							;echo it
			bra		@lp								;and get next
;
@ctls		cmpb	#BS
			bne		@eol?							;if not backspace
			tst		,x
			beq		@lp								;ignore if at start of buffer
			dec		,x								;one less char
			bsr		prb_							;erase prev char
			ldb		#' '
			bsr		prc_
			bsr		prb_
			bra		@lp								;and try for another
;
@eol?		cmpb	#CR
			beq		@eol							;CR = end of line
			cmpb	#LF
			bne		@lp								;LF also = end of line
@eol		tst		loading$
			lbne	con$pause						;else pause input while we process the line
			bra		nl_

; >>> Output a character
; CONSUMES: 0,PP = character to output
			DEF		3,"prc"
prc			puls	d								;B = char to output
			bsr		prc_
			NEXT
;
prb_		ldb		#BS
prc_		cmpb	#NL								;NL outputs CR,LF
			bne		con$prc							;normal char
nl_			ldb		#CR
			bsr		con$prc
			ldb		#LF
			bra		con$prc

; >>> Print N spaces
			DEF		3,"sps"
			puls	x
@lp			cmpx	#0
			beq		@done
			ldb		#' '
			bsr		prc_
			leax	-1,x
			bra		@lp
;
@done		next

; >>> Print (display) a space
			DEF		2,"sp"
			ldb		#' '
			bsr		prc_
			NEXT

; >>> Print (display) a newline
			DEF		2,"nl"
			bsr		nl_
			NEXT

; >>> Print a string to the console
; CONSUMES: 0,PP = addr of string
			DEF		3,"prs"
prs			puls	x								;X = string addr
			bsr		prs_
			NEXT
;
; >>> Subroutine to print a string to the console
; PASSED:  X = string to print
prs_		ldb		,x+								;X = start
			pshs	b								;,S = length
			bra		@done?
;
@lp			ldb		,x+								;display next char
			jsr		prc_
			dec		,s
@done?		bne		@lp								;if more to display
;
			leas	1,s
			rts

;@@@
;@@@ RAW (HARDWARE) CONSOLE I/O
;@@@

	IFDEF PG09

; >>> Console initialization (for @thorpej's 6809 Playground)
; The Playground kernel has already done this for us; no need
; to do anything else.
con$init
	rts

; >>> Poll for raw console input (for @thorpej's 6809 Playground)
; We can use the "cons_pollc" System Subroutine for this.  The Zero
; flag is used in the same way by the Playground kernel, but it
; leaves the value in A undefined if not character is available.
; If Zero is clear, then the character is returned in A.
; RETURNS: NE,B = input char else EQ,B = 0 if none avail
con$rdcq
	pshs	A		; Save A
	jsr	[SysSubr_cons_pollc] ; Call kernel's cons_pollc
	bne	@charavail	; Z clear -> character in A
	clrb			; sets Z
	puls	A,PC		; Restore and return
@charavail
	tfr	A,B		; return character in B
	puls	A,PC		; Restore and return

; >>> Pause console input (set RTS=1)
; There is no mechanism for this on the Playground.  The console UART
; has an auto-/RTS circuit, so at least overflows won't occur.
con$pause
	rts

; >>> Resume console input (set RTS=0)
; See above.
con$resume
	rts

; >>> @thorpej's 6809 Playground: Display character
; We can use the "cons_putc" System Subroutine for this.
; PASSED:  B = character to display
con$prc
	pshs	A		; Save A
	tfr	B,A		; Kernel wants character to display in A
	jsr	[SysSubr_cons_putc] ; Call kernel's cons_putc
	puls	A,PC		; Restore and return

	ELSE

; >>> Console initialization (for Grant's 6 Chip Computer h/w)
; 115200 baud, 8N1, RTS=0, RX INT enabled (in case wired for h/w handshake)
con$init	lda		#ACIA_CTL_CDS_MRESET
			sta		G6CC_ACIA_CTL
			lda		#ACIA_CTL_CDS_BY_16+ACIA_CTL_WS_8N1+ACIA_CTL_TC_RTS0_TXI0+ACIA_CTL_RXI_1
			sta		G6CC_ACIA_CTL
			rts

; >>> Poll for raw console input (for Grant's 6 Chip Computer h/w)
; RETURNS: NE,B = input char else EQ,B = 0 if none avail
con$rdcq	ldb		G6CC_ACIA_STAT
			andb	#ACIA_STAT_RDRF_MASK
			beq		@ret
			ldb		G6CC_ACIA_DATA
@ret		rts

; >>> Pause console input (set RTS=1)
con$pause	lda		#ACIA_CTL_CDS_BY_16+ACIA_CTL_WS_8N1+ACIA_CTL_TC_RTS1_TXI0+ACIA_CTL_RXI_1
			sta		G6CC_ACIA_CTL
			rts

; >>> Resume console input (set RTS=0)
con$resume	lda		#ACIA_CTL_CDS_BY_16+ACIA_CTL_WS_8N1+ACIA_CTL_TC_RTS0_TXI0+ACIA_CTL_RXI_1
			sta		G6CC_ACIA_CTL
			rts

; >>> Grant's 6 Chip Computer: Display character
; PASSED:  B = character to display
con$prc		lda		#ACIA_STAT_TDRE_MASK
@wait		bita	G6CC_ACIA_STAT
			beq		@wait
			stb		G6CC_ACIA_DATA
			rts

	ENDIF
