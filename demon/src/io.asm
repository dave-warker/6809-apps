; "io.asm" - Basic I/O for DeMon
; 2022-aug-22 dww  Created.

	; @@@ Common

; ==> Initialize I/O devices.
io_init:
		lda			#ACIA_CTL_CDS_MRESET
		sta			G6CC_ACIA_CTL
; 115200baud, 8N1, RTS=0, RX INT enabled (same as CoCo BASIC)
		lda			#ACIA_CTL_CDS_BY_16+ACIA_CTL_WS_8N1+ACIA_CTL_TC_RTS0_TXI0+ACIA_CTL_RXI_1
		sta			G6CC_ACIA_CTL
		rts

	; @@@ Console

; ==> Test if console has an input character.
; RETURNS: NE if there is a character waiting else EQ if not.
io_has_ch:
		pshs		a
		lda			G6CC_ACIA_STAT
		anda		#ACIA_STAT_RDRF_MASK
		puls		a
		rts

; ==> Return next console input character (waits if necessary.)
; RETURNS: A = input character
io_get_ch:
lp$		lda			G6CC_ACIA_STAT
		anda		#ACIA_STAT_RDRF_MASK
		beq			lp$
;
		lda			G6CC_ACIA_DATA
		rts

; ==> Return next console input char with timeout.
; PASSED:  B = 1/10ths of a sec to wait (max 2.55 secs)
; RETURNS: CC, A = char or CS if timedout
io_get_ch_w_to:
		pshs		x
;
@olp	ldx			#(G6CC_CPU_CYC_PER_SEC/10)/17						;gives 1/10th sec for inner loop
@ilp	lda			G6CC_ACIA_STAT										;(04)
		anda		#ACIA_STAT_RDRF_MASK								;(02)
		bne			@gotch												;(03) if got a character
		leax		-1,x												;(05) decrement counter
		bne			@ilp												;(03) and try again
		decb															;= 17 cycles total for inner loop
		bne			@olp												;if not out of time
		orcc		#CC_C												;else set carry
		bra			@done												;and exit
;
@gotch	lda			G6CC_ACIA_DATA										;get character
		andcc		#~CC_C												;clear carry
;
@done	puls		x
		rts

; ==> Test if console is ready for the next output character.
; RETURNS: NE if it's ready else EQ if previous char still being sent.
io_can_put_ch:
		pshs		a
		lda			G6CC_ACIA_STAT
		anda		#ACIA_STAT_TDRE_MASK
		puls		a
		rts

; ==> Send a character to the console device (waits if necessary.)
; PASSED:  A = character to send
io_put_equal:
		lda			#'='
		bra			io_put_ch
io_put_space:
		lda			#SPACE
io_put_ch:
		pshs		a
lp$		lda			G6CC_ACIA_STAT
		anda		#ACIA_STAT_TDRE_MASK
		beq			lp$
		puls		a
		sta			G6CC_ACIA_DATA
		rts
