; "ln.asm" - line input support for demon.
; 2022-sep-01 dww  created.

; ==> Set line buffer error handler data
; PASSED:  A = prompt length, X = error handler address
; Assumes current stack level should also be restored
; Negative prompt length means do a NL first
ln_set_err_handler:
		sta			<ln_err_indent									;indent from start of line
		stx			<ln_err_handler									;error handler vector
		leax		2,s
		stx			<ln_err_stack									;stack pointer
		rts

; ==> Signal an error in line buffer
; Uses previously set handler info to display error indicator
ln_err:	lds			<ln_err_stack									;restore stack
		lda			<ln_err_indent
		bpl			@ind											;if no NL needed
;
		jsr			nl
		nega
;
@ind	adda		<ln_next+1										;calc indent for error indicator
		suba		#ln_buf&$FF
		deca
		jsr			spaces
		jsr			msg
		fcc	"^",BEL,CR,LF,EOS										;try to point to error
		jmp			[ln_err_handler]								;off to the handler

; ==> Get a line of input into ln_buf from the keyboard.
ln_fill_buf:
		pshs		x
		ldx			#ln_buf											;reset to start of buffer
		stx			<ln_next										;reset next to start
;
@lp		jsr			io_get_ch										;next input char
		cmpa		#$20
		blo			@ctl											;if it's a control char
		cmpa		#$7F
		bhs			@err											;ignore non-printables
;
		cmpx		#ln_buf+LN_BUF_SIZE-1
		bhs			@err											;if buffer is full
		sta			,x+												;add to buffer
		clr			0,x												;add terminator
@echo	jsr			io_put_ch										;echo it
		bra			@lp
;
@err	lda			#BEL											;error, ring bell
		bra			@echo
;
; Control characters
;
@ctl:	cmpa		#BS
		bne			tab?											;if not backspace
		cmpx		#ln_buf
		beq			@err											;if at start
		jsr			msg												;echo to screen
		fcb	BS,SPACE,BS,EOS
@back	leax		-1,x											;backup pointer
		clr			,x												;add NUL
		bra			@lp
;
tab?	cmpa		#HT
		bne			rcl?											;if not TAB
		cmpx		#ln_buf
		bne			@err											;only valid at start of buffer
		bra			@done											;repeat previous buffer
;
rcl?	cmpa		#DC2
		bne			cr?
		cmpx		#ln_buf
		bne			@err											;only valid at start of buffer
		jsr			msgx											;echo to screen, advance over it
		bra			@back											;backup to NUL
;
cr?		cmpa		#CR
		bne			esc?											;if not CR then error
@end	clr			0,x												;NUL at end
@done	puls		x
		rts
;
esc?	cmpa		#ESC
		bne			@err											;if not ESC to cancel
		ldx			#ln_buf											;else return empty buffer
		bra			@end

; ==> Check if we're at the end of the current command (NUL or ';')
; RETURNS: EQ if at end of command else NE
ln_at_eoc:
		pshs		a
		lda			[ln_next]
		beq			@done											;if at end of line
		cmpa		#EOC											;set EQ if at end of command
@done	puls		a
		rts

; ==> Skip spaces
ln_skip_spaces:
@lp		bsr			ln_peek_for_space
		beq			@lp
		rts

; ==> Gext next char from line buffer.
; RETURNS:	CS if at end of buffer
;			else CC & A = next char
ln_get_ch:
		pshs		x
		ldx			<ln_next
		lda			,x+												;next char
lgc_tst	beq			lgc_end											;if end of buffer
		stx			<ln_next										;advance
		puls		x
lgc_cc	andcc		#~CC_C											;clear carry
		rts
;
lgc_end	puls		x												;don't advance
lgc_cs	orcc		#CC_C											;set carry
		rts

; ==> Get next command char from line buffer.
; RETURNS:	CS if at end of buffer or command sep (;)
;			else CC & A = next char
ln_get_cmd_ch:
		pshs		x
		ldx			<ln_next
		lda			,x+												;next char
		beq			lgc_end											;if end of buffer
		cmpa		#EOC
		bra			lgc_tst											;finish up

; ==> Peek at next line buffer char without advancing.
; RETURNS:	CS if at end of buffer or command sep (;)
;			else CC & A = peeked char
ln_peek_cmd_ch:
		lda			[ln_next]
		beq			lgc_cs											;if end of buffer
		cmpa		#EOC
		beq			lgc_cs											;if end of command
		bra			lgc_cc											;else we're cool

; ==> Skip next char (assumes not at end of buffer)
ln_skip_ch:
		pshs		x
		ldx			<ln_next
		leax		1,x
@done	stx			<ln_next
		puls		x
		rts
;
; ==> Unskip char (assumes not at start of buffer)
ln_unskip_ch:
		pshs		x
		ldx			<ln_next
		leax		-1,x
		bra			@done

; ==> Require a specific char as the next buffer char.
; PASSED:  A = char to expect
; NOTE: Doesn't return if there is an error
ln_expect_space:
		lda			#SPACE
ln_expect_ch:
		bsr			ln_peek_for_ch									;is it there?
		lbne		cmd_err											;nope, fail
		rts

; ==> Look for and consume a specific char as the next buffer char.
; PASSED:  A = char to expect (assumed to not be NUL or EOC)
; RETURNS: EQ if char was found and consumed else NE if not
ln_peek_for_comma:
		lda			#','
		bra			ln_peek_for_ch
ln_peek_for_minus:
		lda			#'-'
		bra			ln_peek_for_ch
ln_peek_for_plus:
		lda			#'+'
		bra			ln_peek_for_ch
ln_peek_for_space:
		lda			#SPACE
ln_peek_for_ch:
		cmpa		[ln_next]
		bne			@ret											;if not what we expected
		bsr			ln_skip_ch										;skip over it
		orcc		#CC_Z											;ensure EQ is returned
@ret	rts

; ==> Look for and skip a specific letter
; PASSED:  A = alpha char to look for (can be either case)
; RETURNS: EQ if found (and skipped) it else NE
ln_peek_for_letter:
		bsr			ln_peek_for_ch									;match with passed case?
		beq			@ret											;yep
		eora		#$20											;else flip case
		bsr			ln_peek_for_ch
@ret	rts

; ==> Look for and skip an upper case alpha char
; RETURNS: CS if end of command or not alpha else CC, A = alpha char converted to upper case
ln_peek_for_uc_alpha:
		bsr			ln_peek_cmd_ch
		bcs			@nope											;if no char present
		jsr			to_upper_ch										;convert to upper case
		cmpa		#'A'
		blo			@nope											;if not alpha
		cmpa		#'Z'
		bls			@yep											;if it is alpha
@nope	orcc		#CC_C											;set carry
		rts
;
@yep	bsr			ln_skip_ch										;eat the alpha
		andcc		#~CC_C											;clear carry
		rts
;
; ==> Look for and skip decimal digit
; RETURNS: CS if end of command or non digit else CC, A = digit character
ln_peek_for_decimal_digit:
		bsr			ln_peek_cmd_ch
		bcs			@nope											;if no char present
		cmpa		#'0'
		bcs			@nope											;if not digit
		cmpa		#'9'
		bls			@yep											;if it is a digit
		bra			@nope

; ==> Peek for string ignoring case
; PASSED:  X = address of NUL terminated string (any letters should be UC)
; RETURNS: EQ if the string matched (and was eaten) else NE and position unchanged
ln_peek_for_string:
		pshs		d,y
		ldy			<ln_next										;so we can restore
;
@lp		tst			,x
		beq			@ret											;if end of string then matched
		lbsr		ln_get_cmd_ch
		bcs			@nope											;if end of buffer then fail
		jsr			to_upper_ch
		cmpa		,x+
		beq			@lp												;if still matching
;
@nope	sty			<ln_next										;restore position
		andcc		#~CC_Z											;return NE
;
@ret	puls		y,d
		rts

; ==> Get uppercased word (w/MSB set on last letter)
; PASSED:  Y = pointer to word buffer, B = buffer length (!= 0)
; RETURNS: CC if found a word else CS if no letters were found or was too long
ln_get_uc_word:
		bsr			ln_peek_for_uc_alpha
		bcs			@err											;if we didn't find at least one
;
@lp		sta			,y+												;store this one
		bsr			ln_peek_for_uc_alpha							;get next
		bcc			@yep											;if it's a letter
		bsr			ln_peek_for_decimal_digit
		bcs			@done											;if it's not a digit
@yep	decb
		bne			@lp												;if there's room
;
@err	orcc		#CC_C											;return error
		rts
;
@done	lda			-1,y											;set MSB of last
		ora			#$80
		sta			-1,y
		andcc		#~CC_C											;return CC
		rts		

; ==> Get a register index
; RETURNS: CS if not found else CC, A = register index
ln_get_register:
		pshs		b,y
		ldd			<ln_next										;save position in case we fail
		pshs		d
;
		bsr			ln_peek_for_uc_alpha							;1st letter (required)
		bcs			@nope											;error if none
		tfr			a,b
		bsr			ln_peek_for_uc_alpha							;2nd (optional)
		bcs			@no2nd											;if no second
		pshs		a
		bsr			ln_peek_for_uc_alpha
		puls		a
		bcs			@lkup											;if no 3rd
;
@nope	puls		d												;restore position
		std			<ln_next
		orcc		#CC_C											;return CS if error
@ret	puls		y,b
		rts
;
@no2nd	clra														;no 2nd letter
@lkup	exg			a,b												;A = 1st, B = 2nd
		ldy			#ln_reg_table									;Y = register list ptr
@lklp	cmpd		,y++
		beq			@yep											;if got a match
		cmpy		#ln_end_reg_table
		bne			@lklp											;if not end of table
		bra			@nope											;else not found
;
@yep	leay		-ln_reg_table-2,y								;Y = index * 2
		tfr			y,a
		lsra														;A = register index
		leas		2,s
		bra			@ret											;return CC from shift

; ==> List of register names.
; Order matches the defines at top of demon.asm (REG_A ...)
ln_reg_table:
		fcb		'A',0,'B',0,'D',0,'D','P','X',0,'Y',0,'U',0,'C','C','P','C','S',0
ln_end_reg_table equ *
