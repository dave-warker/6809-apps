; "con.asm" - console support for Syl
; 2023-feb-06 dww  created

;@@@
;@@@ CONSOLE INPUT
;@@@

; >>> Start loading from console
con$load	lda		#1
			sta		<con_loading							;set flag
			jmp		con$pause								;pause input until next line input

; >>> Stop loading from console
con$loaded	tst		<con_loading							;were we loading?
			beq		@ret									;nope
			clr		<con_loading							;not any more
			jmp		con$resume
;
; >>> Get next non-blank character (skips SPACES, TABs, NLs, etc)
con$getnb	bsr		con$getc
			cmpb	#$20
			bls		con$getnb								;skip whitespace chars
@ret		rts

; >>> Read next buffered input character
; RETURNS: B = char, will be NL at end of line
; ALTERS NO OTHER REGISTERS!
con$getc	pshs	x,a
@getc		ldx		<con_buf_next							;offset for next char
			bmi		@getl									;if need to fill it
			cmpx	<con_buf_len
			bhs		@eol									;if hit end of line
			ldb		con_buf,x								;B = next char
			leax	1,x										;bump to next
@gotc		stx		<con_buf_next
			puls	x,a
			rts
;
@eol		ldb		#NL										;return newline at end of line
			ldx		#CON_BUF_EMPTY							;and set flag to refill buffer
			stx		<lex$tok$start							;also invalidate last token
			bra		@gotc
;
@getl		bsr		con$getl								;get next line of input
			bra		@getc									;and try again

; >>> Undo last con$getc (if possible)
; ALTERS NO REGISTERS
con$ungetc	pshs	d
			ldd		<con_buf_next
			beq		@done									;can't undo past start of buffer
			blt		@end									;if empty then go back to end
			subd	#1										;else backup one char
			std		<con_buf_next
@done		puls	d
			rts
;
@end		ldd		<con_buf_len
			bra		@done

; >>> Cancel remaining chars in input buffer
con$canl	ldd		#CON_BUF_EMPTY
			std		<con_buf_next
			rts

; >>> Read a line of input to the buffer
; ALTERS:  B,X
con$getl	tst		<con_loading
			beq		@prep									;if not loading from console
			jsr		con$resume								;else resume input
;
@prep		bsr		con$prompt								;display prompt if needed
			ldx		#0										;offset to start of buffer
			stx		<con_buf_next							;will be next input char
;
@lp			lbsr	con$gotc								;wait for next input char
			beq		@lp
			cmpb	#$20
			blo		@ctl									;if it's a control char
			cmpb	#$7E
			bhi		@lp										;ignore non-printables (?)
;
			cmpx	#CON_BUF_SIZE							;is there room?
			bhs		@lp										;nope, ignore it
			stb		con_buf,x								;store it
			leax	1,x										;and advance count
@echo		bsr		con$putc								;echo it
			bra		@lp										;try for another
;
@ctl		cmpb	#BS
			bne		@eol?									;if not backspace
			cmpx	#0
			beq		@lp										;if at start of line
			leax	-1,x									;drop last char
			bsr		con$putc								;erase from console
			ldb		#' '
			bsr		con$putc
			ldb		#BS
			bra		@echo									;and try again
;
@eol?		cmpb	#CR
			beq		@eol									;if it's end of line
			cmpb	#LF
			bne		@lp										;else ignore it
@eol		stx		<con_buf_len							;save length of buffer
;
			tst		<con_loading
			beq		@done									;if not loading
			jsr		con$pause								;else pause input
@done:
			; fall through into ...
;
; >>> Display newline
con$nl		ldb		#CR
			bsr		con$putc
			ldb		#LF
			bra		con$putc
;
; >>> Display a character
; PASSED:  B = char to display
; NOTE: Converts NLs to CRLFs
con$putc	cmpb	#NL
			lbne	con$outc								;if not a newline
			ldb		#CR										;else output CR+LF
			lbsr	con$outc
			ldb		#LF
			lbra	con$outc

; >>> Display line input prompt if needed.
con$prompt:
			tst		<con_loading
			bne		@ret									;not needed if loading
			lda		<sexp$par$depth
			beq		@ret									;or if no unbalanced parens
;
@lp			ldb		#' '									;display 2 spaces/level
			bsr		con$putc
			bsr		con$putc
			deca
			bne		@lp
;
@ret		rts

;@@@
;@@@ CONSOLE OUTPUT
;@@@

; >>> Display message
; PASSED:  X = address of message, first byte is length
con$msg		pshs	d
			lda		,x+										;A = length
			bra		@end?
;
@lp			ldb		,x+										;display next char
			bsr		con$putc
			deca
@end?		bne		@lp										;if not done
;
			puls	d
			rts

; >>> Display 16 bit unsigned hex integer
; PASSED:  D = number to display
con$disp$hex$word:
			pshs	d
			tfr		a,b
			bsr		con$disp$hex$byte						;display high byte
			puls	d
	; fall through into ...
;
; >>> Display 8 bit unsigned hex integer
; PASSED: B = byte to display
con$disp$hex$byte:
			pshs	b
			lsrb
			lsrb
			lsrb
			lsrb
			bsr		@disp$nib								;display high nibble
			puls	b
;
@disp$nib	pshs	b										;display low nibble of B
			andb	#$0f
			addb	#'0'
			cmpb	#'9'
			bls		@disp$dig								;if it's 0-9
			addb	#'A'-'0'-10								;else it's A-F
@disp$dig	bsr		con$outc
			puls	b
			rts

; >>> Display signed 16 bit integer value
; PASSED:  D = integer value
;
con$disp$int$word:
			tsta
			bpl		con$disp$ord$word						;display positive numbers as-is
;
			pshs	b										;else display sign
			ldb		#'-'
			jsr		con$outc
			puls	b
;
			pshs	d
			coma											;n = -n
			comb
			addd	#1
			jsr		con$disp$ord$word
			puls	d
			rts

; >>> Display unsigned 16 bit integer value
; PASSED:  D = integer value
;
con$disp$ord$word:
			pshs	x,d
			bsr		@disp
			puls	d,x
			rts
;
@disp		cmpd	#10
			blo		@putd									;if it's the highest digit
			ldx		#10										;else generate next digit
			jsr		mod16
			pshs	b										;save it
			tfr		x,d
			bsr		@disp									;show the higher digits
			puls	b
@putd		addb	#'0'									;display this digit
			bra		con$outc

;@@@
;@@@ RAW (HARDWARE) CONSOLE I/O
;@@@

; >>> Console initialization (for Grant's 6 Chip Computer h/w)
; 115200 baud, 8N1, RTS=0, RX INT enabled (in case wired for h/w handshake)
con$init	lda		#ACIA_CTL_CDS_MRESET
			sta		G6CC_ACIA_CTL
			lda		#ACIA_CTL_CDS_BY_16+ACIA_CTL_WS_8N1+ACIA_CTL_TC_RTS0_TXI0+ACIA_CTL_RXI_1
			sta		G6CC_ACIA_CTL
			clr		<con_loading
			bra		con$resume

; >>> Poll for raw console input (for Grant's 6 Chip Computer h/w)
; RETURNS: NE,B = input char else EQ,B = 0 if none avail
con$gotc	ldb		G6CC_ACIA_STAT
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

; >>> Grant's 6 Chip Computer: Output character
; PASSED:  B = character to display
con$outc	pshs	a
			lda		#ACIA_STAT_TDRE_MASK
@wait		bita	G6CC_ACIA_STAT
			beq		@wait
			stb		G6CC_ACIA_DATA
			puls	a
			rts
