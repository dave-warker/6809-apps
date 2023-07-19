; "cmds.asm" - debugger commands
; 2022-sep-08 dww  created.

; @@@
; @@@ Command dispatch table
; @@@

cmd_tbl	fdb			cmd_err											; A = 
		fdb			cmd_err											; B = 
		fdb			cmd_comp_mem									; C = Compare/CRC memory range
		fdb			cmd_disp_mem									; D = Display Memory
		fdb			cmd_eval										; E = Evaluate expression
		fdb			cmd_fill_mem									; F = Fill Memory
		fdb			cmd_go											; G = Go command
		fdb			cmd_err											; H = 
		fdb			cmd_err											; I = 
		fdb			cmd_err											; J = 
		fdb			cmd_err											; K = 
		fdb			cmd_loop										; L = Loop command line
		fdb			cmd_move_mem									; M = Move memory
		fdb			cmd_err											; N = 
		fdb			cmd_err											; O = 
		fdb			cmd_err											; P = 
		fdb			cmd_query_mem									; Q = Query (search) memory
		fdb			cmd_read										; R = Read data
		fdb			cmd_sub_mem										; S = Substitute into memory
		fdb			cmd_trace										; T = Trace
		fdb			cmd_err											; U = 
		fdb			cmd_err											; V = 
		fdb			cmd_write										; W = Write data
		fdb			cmd_examine_regs								; X = Examine registers
		fdb			cmd_disassm										; Y = Disassembler
		fdb			cmd_assm										; Z = Assembler

; @@@
; @@@ Commands
; @@@

; ==> Compare/Calc CRC for memory range
; C<range>{' '<dest>}
cmd_comp_mem:
		jsr			ln_get_range1									;get source range
		jsr			ln_at_eoc
		bne			@comp											;if comparing memory
;
; Display CRC-16 for memory range
;
		jsr			crc16											;calculate CRC
		jsr			disp_hex_word
		jmp			nl
;
; Compare memory range to destination block
;
@comp	pshs		d												;X = source addr, D = (non-zero) count
		jsr			ln_expect_space
		jsr			ln_get_expr
		tfr			d,y												;Y = destination address
		puls		d
;
@lp		pshs		d												;compare next byte
		lda			,x+
		cmpa		,y+
		beq			@next											;if they match
;
		tfr			x,d												;show source addr
		subd		#1
		jsr			disp_hex_word
		jsr			io_put_equal
		lda			-1,x											; show source byte
		jsr			disp_hex_byte
		jsr			io_put_space
		tfr			y,d												;show dest addr
		subd		#1
		jsr			disp_hex_word
		jsr			io_put_equal
		lda			-1,y											; show source byte
		jsr			disp_hex_byte
		jsr			nl
;
@next	puls		d
		subd		#1
		bne			@lp												;if more to do
		rts

; ==> DIPLAY MEMORY
; D<range>
cmd_disp_mem:
		ldd			#DISP_MEM_BPL									;get range to display
		jsr			ln_get_range
		jsr			ln_expect_eoc									;should be end of command
		tfr			d,y												;X = address, Y = remaining count
;
@lnlp	tfr			y,d												;B = count for this line
		cmpd		#DISP_MEM_BPL
		blo			@dspln											;if less than a full line use B as-is
		ldb			#DISP_MEM_BPL									;else limit count
@dspln	bsr			disp_hex_ln										;display one line
		negb														;update count
		leay		b,y
		bne			@lnlp											;if more to do
;
		rts
;
disp_hex_ln:
		pshs		b												;save byte count
		tfr			x,d												;display start address
		jsr			disp_hex_word
		jsr			io_put_space
;
		pshs		x												;display in hex
		ldb			2,s												;B = byte count
hexlp$	jsr			io_put_space
		lda			,x+
		jsr			disp_hex_byte
		decb
		bne			hexlp$
		puls		x
;
		lda			#DISP_MEM_BPL									;pad gap to ASCII with spaces
		suba		0,s												;(BPL - count)*3 + 2
		pshs		a
		adda		0,s
		adda		,s+
		adda		#2
		jsr			spaces
;
		ldb			0,s												;display ASCII, B = byte count
asclp$	lda			,x+
		jsr			to_printable_ch									;make sure it's printable
		jsr			io_put_ch
		decb
		bne			asclp$
;
		jsr			nl
		puls		b												;return count in B again
		jmp			cmd_pause										;chance to pause or abort

; ==> Evaluate expression
; E{&}<expr>
cmd_eval:
		lda			#'&'
		jsr			ln_peek_for_ch									;& means display in decimal
		pshs		cc
		jsr			ln_skip_spaces									;skip spaces
		jsr			ln_get_expr										;D = value
		puls		cc
		bsr			@disp
		jmp			nl
;
@disp	lbeq		disp_dec_sword									;display as signed decimal
		jmp			disp_hex_word									;else hex

; ==> Fill Memory
; F<range> <byte>
cmd_fill_mem:
		ldd			#0
		jsr			ln_get_range									;range to fill
		tfr			d,y												;X = start, Y = (non-zero) count
		jsr			ln_expect_space
		jsr			ln_get_expr										;B = value to fill with
		jsr			ln_expect_eoc
;
@filllp	stb			,x+												;fill next byte
		leay		-1,y
		bne			@filllp											;do next
;
		rts

; ==> Go command
; G{<pc> {<bkpt>}} | GT<bkpt>
cmd_go:
		ldx			#BKPT_ADDR_NONE									;assume no breakpoint
		lda			#'T'
		jsr			ln_peek_for_letter
		beq			@bkpt											;if breakpoint supplied
;
		jsr			ln_at_eoc
		beq			@go												;if no PC or breakpoint
		jsr			ln_get_expr										;else get new PC
		std			<cpu_reg_pc
		jsr			ln_at_eoc
		beq			@go												;if no breakpoint
		jsr			ln_expect_space									;else better be space separator
;
@bkpt	jsr			ln_get_expr										;get breakpoint address
		tfr			d,x
;
@go		stx			<bkpt_addr										;breakpoint address (if any)
		jsr			ln_expect_eoc									;better be end of command
		jmp			restore_cpu_state								;here we go
;
bkpt_reentry:
		jsr			disp_all_regs
		jmp			rep_lp											;continue with command line

; ==> Loop command line
; L
cmd_loop:
		jsr			cmd_pause										;chance to bail out
		ldx			#ln_buf											;reset line pointer to start
		stx			<ln_next
		rts

; ==> Move memory
; M<range> <dest>
cmd_move_mem:
		jsr			ln_get_range1									;source range
		pshs		d
		jsr			ln_expect_space
		jsr			ln_get_expr
		jsr			ln_expect_eoc
		tfr			d,y
		puls		d												;X = source, Y = dest, D = count
;
		pshs	y
		cmpx	,s++												;source < dest?
		blo		@dsc												;yep, have to copy descending
;
		lsra														;convert byte count to word
		rorb
		tfr		d,u													;U = vword count
		bcc		@asclp												;if count was even
		lda		,x+													;else copy odd byte
		std		,y+
;
@asclp	cmpu	#0
		beq		@done												;if all done
		ldd		,x++												;else copy a word
		std		,y++
		leau	-1,u
		bra		@asclp
;
@dsc	leax	d,x													;copy descending addresses
		leay	d,y
;
		lsra														;convert byte count to word
		rorb
		tfr		d,u													;U = word count
		bcc		@dsclp												;if byte count was even
		lda		,-x													;else copy odd byte
		sta		,-y
;
@dsclp	cmpu	#0
		beq		@done												;if all done
		ldd		,--x												;copy a word
		std		,--y
		leau	-1,u
		bra		@dsclp
;
@done	rts

; ==> Query (search) memory range (max 8 bytes)
; Q<range> <bytes>+
; Stack frame:
;  0,u = remaining range count, 2,u = query string length, 3,u = query string mask, 4,u = query string (8 bytes max)
cmd_query_mem:
		leas		-12,s											;allocate stack frame
		tfr			s,u
;
		jsr			ln_get_range1									;X = range address
		std			0,u												;0,u = range count
;
; Build the query string
;
		clr			2,u												;init search byte counter
		clr			3,u												; and mask
		leay		4,u												;Y = search byte pointer
;
@qslp	jsr			ln_expect_space									;next query byte
		lsl			3,u												;mask bit == 0 to match any
		lda			#ANY
		jsr			ln_peek_for_ch
		beq			@qsnxt											;if it can match any
		inc			3,u												;mask bit == 1 to require match
		jsr			ln_get_expr
		stb			,y
@qsnxt	leay		1,y												;bump string pointer
		inc			2,u												;and byte count
		jsr			ln_at_eoc
		beq			@qspad											;if end of command
		lda			2,u
		cmpa		#8
		blo			@qslp											;if there's room for another
		jsr			ln_expect_eoc									;else should be end of command
;
@qspad	lda			#8												;left align bit mask
		suba		2,u
@qsplp	beq			@qulp											;if we're done
		lsl			3,u
		deca
		bra			@qsplp											;if need to shift more
;
; Run the query
;  X = start address, 0,u = range count, 2,u = query string len, 3,u = query string mask, 4,u = 8 byte query string buffer
;
@qulp	bsr			@qmatch											;check for a match
		bne			@qunxt											;if none
		bsr			@qshow											;else display it
@qunxt	leax		1,x												;bump to next address
		ldd			#-1
		addd		0,u
		std			0,u												;decrement count
		bne			@qulp											;next address
;
@done	leas		12,s											;dealloc stack frame
		rts
;
; Check for match at current address (return EQ if it matches)
;
@qmatch:
		pshs		x
		leay		4,u												;Y = query string pointer
		ldd			2,u												;A = query string count, B = mask
;
@qmlp	tsta
		beq			@qmret											;matches if query length exhausted
		lslb			
		bcc			@qmnxt											;if this byte doesn't have to match
		pshs		a
		lda			,x
		cmpa		,y
		puls		a
		bne			@qmret											;if doesn't match
@qmnxt	leax		1,x												;bump pointers
		leay		1,y
		deca														;decrement count
		bra			@qmlp											;try next
;
@qmret	puls		x
		rts
;
; Display match at current address
;
@qshow:
		pshs		x
		ldb			2,u												;B = length of string
		jsr			disp_hex_ln										;display the match
		puls		x
		rts

; ==> Substitute into memory.
; S<addr> <byte>+
cmd_sub_mem:
		jsr			ln_get_expr
		tfr			d,x												;X = starting address
;
@sublp	jsr			ln_peek_for_space								;another byte?
		bne			@ret											;nope, we're done
;
		lda			#DQUOT
		jsr			ln_peek_for_ch
		bne			@byte											;if not a string
@strlp	jsr			ln_get_cmd_ch									;next string char
		bcs			@ret											;string always ends at EOL or EOC
		cmpa		#DQUOT
		beq			@ret											;or at ending quote
		cmpa		#BSLASH
		bne			@strch											;if not an escape
		jsr			ln_get_ch										;always take next char
		bcs			@ret											;except at end of buffer
@strch	sta			,x+												;store in memory
		bra			@strlp
;
@byte	jsr			ln_get_expr										;yep, get byte value
		stb			,x+												;store in memory
		bra			@sublp
;
@ret	rts

; ==> examine registers
; X{<reg>{'='<expr>}}*
cmd_examine_regs:
		jsr			ln_at_eoc
		beq			disp_all_regs									;if no args then show all
;
; Set individual registers
;
@reglp	jsr			ln_expect_reg_name								;X = register index
		lda			#'='
		jsr			ln_expect_ch
		jsr			ln_get_expr										;D = new value
		jsr			reg_set_value
		jsr			ln_peek_for_space								;another register?
		beq			@reglp											;yep
		jsr			ln_expect_eoc									;nope, better be end of command
;
; Show all registers
;
disp_all_regs:
		ldx			#REG_PC											;PC
		bsr			@show_reg_space
		ldx			cpu_reg_pc
		jsr			msg
		fcc			": ",EOS
		jsr			inst_disp_instr
		jsr			nl
;
		jsr			io_put_space
		ldx			#REG_A											;A
		bsr			@show_reg_space
		ldx			#REG_B											;B
		bsr			@show_reg_space
		ldx			#REG_CC											;CC
		bsr			@show_reg_space
		ldx			#REG_X											;X
		bsr			@show_reg_space
		ldx			#REG_Y											;Y
		bsr			@show_reg_space
		ldx			#REG_U											;U
		bsr			@show_reg_space
		ldx			#REG_S											;S
		bsr			@show_reg_space
		ldx			#REG_DP											;DP
		bsr			@show_reg
		jmp			nl
;
; Show a register (X = index)
;
@show_reg_space:
		bsr			@show_reg
		jmp			io_put_space
;
@show_reg:
		jsr			disp_reg_name									;display register name
		jsr			io_put_equal
		jsr			reg_get_value									;D = register value
		pshs		d
		lda			reg_infos,x										;A = info byte
		bita		#REG_INFO_CC
		bne			@showcc											;if it's condition codes
;
		bita		#REG_INFO_WORD
		puls		d
		lbne		disp_hex_word									;if it's a word
		tfr			b,a
		jmp			disp_hex_byte									;else it's a byte
;
@showcc	puls		d												;B = condition codes
		ldy			#@cc_chs
@cclp	lda			,y+												;A = bit letter (lower case)
		beq			@ccend											;if end of list
		rolb
		bcc			@putcc											;if bit not set
		jsr			to_upper_ch										;else set, show as upper case
@putcc	jsr			io_put_ch										;display flag
		bra			@cclp											;back for another
@ccend	rts
;
@cc_chs	fcc	"efhinzvc",0

; ==> Read (Intel Hex or Motorola S Record) data
; R{<offset>}
; Stack frame:
;  0,u = rec checksum; 1,u = rec byte count; 2,u = addr offset; 4,u = low water mark, 6,u = high
cmd_read:
		leas		-8,s											;allocate stack frame
		tfr			s,u
;
		ldx			#0
		stx			2,u												;offset = 0
		stx			6,u												;high water mark = 0
		leax		-1,x
		stx			4,u												;low water mark = $FFFF
;
		jsr			ln_peek_cmd_ch									;use their offset if provided
		bcs			@eoc
		jsr			ln_get_expr
		std			2,u
@eoc	jsr			ln_expect_eoc									;better be end of command
;
		lda			#BEL											;DING! we're ready
		jsr			io_put_ch
;
; Receive record loop
;
@reclp	clr			0,u												;init checksum to 0
		jsr			@rxch											;wait for next record
		cmpa		#'S'
		beq			@msrec											;if it's a Motorola S rec
		cmpa		#':'
		lbeq		@ihrec											;if it's an Intel Hex rec
		cmpa		#ESC
		bne			@reclp											;if not aborting
;
@done	ldd			4,u												;display read range
		cmpd		#$FFFF
		beq			@ret											;assume nothing was read
		jsr			disp_hex_word									;else show low water mark
		jsr			io_put_space
		ldd			6,u
		jsr			disp_hex_word									;show high water mark
		jsr			nl
;
@ret	leas		8,s												;dealloc frame
		bsr			@wait											;wait til final CR, LF sent
		rts
;
@wait	ldb			#10												;wait til done sending
		jsr			io_get_ch_w_to									;(no chars for 1 second)
		bcc			@wait
		rts
;
@err	jsr			msg												;bail on error
		fcc	"error!",CR,LF,EOS
@errlp	bsr			@wait											;wait til done sending
		jmp			rep_lp											;bail
;
; Receive Motorola S record
;
@msrec	jsr			@rxhxch											;get record type
		pshs		a
		bsr			@rxcnt											;get byte count
		lda			1,u												;subtract address, checksum
		suba		#3
		sta			1,u
		bcs			@err											;error if count < 3
		bsr			@rxadr											;X = address (adjusted for offset)
		puls		a
;
		cmpa		#1												;handle 16bit address data record
		bne			@mstyp9
		ldb			1,u												;B = byte count
@msdalp	tstb
		beq			@msdacs											;if done with data
		bsr			@rxhxby											;get next data byte
		sta			,x+												;store in memory
		decb
		bra			@msdalp											;get next
@msdacs	lda			0,u												;validate checksum
		coma
@mschk	bsr			@rxcsum
		bra			@reclp											;and wait for next record
;
@mstyp9	cmpa		#9
		bne			@msign											;if not end of file record
@msend	bsr			@rxhxby											;ignore checksum
		bra			@done											;and we're all done!
;
@msign	tstb														;ignore other record types
		beq			@msdacs											;if end of data
		bsr			@rxhxby											;read and ignore byte
		decb
		bra			@msign
;
; Utils
;
@rxcnt	bsr			@rxhxby											;receive byte count
		sta			1,u
		rts
;
@rxadr	bsr			@rxhxby											;receive address
		tfr			a,b
		bsr			@rxhxby
		exg			a,b
		addd		2,u												;add offset
		tfr			d,x												;X = address
;
		tst			1,u
		beq			@rxaret											;if no data don't update water marks
;
		cmpd		4,u												;update low water mark
		bhs			@updhi											;if not new low water mark
		std			4,u
;
@updhi	pshs		x												;calc end of record addr
		clra
		ldb			1,u
		leax		d,x
		leax		-1,x
		cmpx		6,u
		bls			@rxadrx											;if not > current high water
		stx			6,u
@rxadrx	puls		x
@rxaret	rts
;
@rxcsum	sta			,-s												;save expected value
		bsr			@rxhxby											;get actual checksum
		cmpa		,s+
		lbne		@err											;if doesn't match
		rts
;
@rxhxby	bsr			@rxhxch											;receive hex byte
		lsla														;high nibble
		lsla
		lsla
		lsla
		sta			,-s
		bsr			@rxhxch											;low nibble
		ora			,s+												;A = byte value
		pshs		a												;update checksum
		adda		0,u
		sta			0,u
		puls		a
		rts
;
@rxhxch	bsr			@rxch											;receive hex character
		jsr			decode_hex_ch									;convert to 0-15
		lbcs		@err											;if not a valid hex char
		rts
;
@rxch	jmp			io_get_ch										;receive a character
;
; Receive Intel Hex record
;
@ihrec	bsr			@rxcnt											;get byte count
		bsr			@rxadr											;get address
		bsr			@rxhxby											;A = record type
;
		tsta
		bne			@ihtyp1											;if not data record
		ldb			1,u												;B = byte count
@ihdalp	tstb
		beq			@ihdacs											;if end of data
		bsr			@rxhxby											;get next data byte
		sta			,x+												;store in memory
		decb
		bra			@ihdalp											;next byte
@ihdacs	lda			0,u												;validate checksum
		nega
		jmp			@mschk											;finish like Motorola S rec
;
@ihtyp1	deca
		lbeq		@msend											;if type 1 (EOF) finish like Motorola S rec
		jmp			@err											;else bail if any other record type

; ==> Write (Intel Hex or Motorola S Record) data
; W{I}<range>
cmd_write:
		ldy			#write_mots_rec									;assume using Motorola S format
		lda			#'I'
		jsr			ln_peek_for_letter
		bne			@rng											;if that's what we're using
		ldy			#write_ihex_rec									;else using Intel Hex format
;
@rng	jsr			ln_get_range1									;get range to write
		pshs		d												;0,s = count remaining, X = addr
;
@reclp	ldd			0,s												;get remaining count
		beq			@end											;if we're done
		cmpd		#WRITE_BPR_MAX
		bls			@upcnt											;if short record
		ldd			#WRITE_BPR_MAX									;else limit to max allowed
@upcnt	pshs		d
		ldd			2,s												;update count
		subd		0,s
		std			2,s
		puls		d												;D = rec len, X = address
		jsr			,y												;write the record (updates X)
		jsr			nl
		bra			@reclp											;next record
;
@end	jsr			,y												;write EOF record(s)
		puls		d
		jmp			nl

; ==> Write an Intex Hex record
; PASSED:  X = address, B = length
; RETURNS: X = address after rec's data
write_ihex_rec:
		clr			,-s												;0,s = checksum
		lda			#':'											;start of record
		jsr			io_put_ch
;
		tfr			b,a												;write rec len
		bsr			@wbyt
;
		pshs		b												;write address
		tfr			x,d												;(big endian order)
		puls		b
		bsr			@wbyt
		tfr			x,a
		bsr			@wbyt
;
		clra														;rec type = 00 (data)
		tstb
		bne			@len
		inca														;else = 01 (eof)
@len	bsr			@wbyt
;
@ilp	decb														;write data bytes
		bmi			@icsum											;if done (assumes len <= 128)
		lda			,x+
		bsr			@wbyt
		bra			@ilp
;
@icsum	lda			0,s												;write checksum
		nega
		bsr			@wbyt
;
		leas		1,s												;done
		rts
;
@wbyt	jsr			disp_hex_byte									;write byte in hex
		adda		2,s												;update checksum
		sta			2,s
		rts
;
; ==> Write a Motorola S record
; PASSED:  X = address, B = length
; RETURNS: X = address after rec's data
write_mots_rec:
		clr			,-s												;0,s = checksum
		lda			#'S'											;start of record
		jsr			io_put_ch
;
		lda			#'1'											;assume data record
		tstb
		bne			@rtyp
		lda			#'9'											;else EOF record
		ldx			#0												;force address = 0
@rtyp	jsr			io_put_ch
;
		tfr			b,a												;byte count
		adda		#3												;including addr & checksum
		bsr			@wbyt
;
		pshs		b												;write address
		tfr			x,d												;(big endian order)
		puls		b
		bsr			@wbyt
		tfr			x,a
		bsr			@wbyt
;
@mlp	decb														;write data bytes
		bmi			@mcsum											;if done (assumes len <= 128)
		lda			,x+
		bsr			@wbyt
		bra			@mlp
;
@mcsum	lda			0,s												;write checksum
		coma
		bsr			@wbyt
;
		leas		1,s												;done
		rts

; ==> Disassemble memory range
; Y<range>
cmd_disassm:
		jsr			ln_get_range1									;X = start addr, D = byte count
		leay		d,x
		pshs		y												;0,s = end addr
;
@dislp	bsr			disassm1										;disasemble next instruction
		cmpx		0,s
		blo			@dislp											;if not done
;
		leas		2,s
		rts
;
; Disassemble one instruction
; PASSED:  X = address of the instruction
; RETURNS: X = address immediately after the instruction
disassm1:
		pshs		x												;save start address
		lda			#MAX_INSTR_BYTES*3+7							;leave space for hex
		jsr			spaces
		jsr			inst_disp_instr									;disassemble one (X = next instr)
;
		lda			#CR												;display address and bytes
		jsr			io_put_ch
		puls		y												;start address
		tfr			y,d
		jsr			disp_hex_word
		jsr			io_put_space
;
		pshs		x												;end address
@hexlp	jsr			io_put_space
		lda			,y+												;display a byte
		jsr			disp_hex_byte
		cmpy		0,s
		blo			@hexlp											;if not done
		leas		2,s
		jmp			nl												;next line

; ==> Assembler
; Z<addr>
cmd_assm:
		jsr			ln_at_eoc
		beq			@asmlp											;if no address specified
		jsr			ln_get_expr
		std			<asm_addr										;target address
;
@asmlp	lda			#-6												;prompt length (need NL)
		ldx			#@asmlp											;error handler
		jsr			ln_set_err_handler
		ldx			<asm_addr										;X = target address
;
		tfr			x,d
		jsr			disp_hex_word
		lda			#2
		jsr			spaces
;
		pshs		x												;save start address
		jsr			ln_fill_buf										;get instruction text
		tst			ln_buf
		beq			@done											;empty line stops
		ldx			,s
		jsr			inst_asm_ln										;assemble the instruction in the buffer
		puls		x
;
		lda			#CR												;display the instruction
		jsr			io_put_ch
		bsr			disassm1
		stx			<asm_addr
		bra			@asmlp
;
@done	leas		2,s
		jsr			nl
		jmp			rep_lp											;prompt for new line

; ==> Trace
; T{O}{<count>} | TT<addr>
cmd_trace:
;
; TT<addr> - Trace Til
;
		lda			#'T'
		jsr			ln_peek_for_letter
		bne			@tsteps									;if not trace til
;
		jsr			ln_get_expr
		tfr			d,y										;Y = target address
		bsr			@ttil									;trace til PC == target address
@done	lbra		disp_all_regs							;display registers
;
@ttlp
	IF TRACE_POLLING
		bsr			@poll									;chance to abort
	ENDIF
		bsr			@trace1									;trace til PC == Y
@ttil	cmpy		<cpu_reg_pc
		bne			@ttlp
@ret	rts
;
	IF TRACE_POLLING
@poll	jsr			io_has_ch
		beq			@ret									;if no char waiting
		jsr			io_get_ch
		cmpa		#SPACE
		lbeq		disp_all_regs							;if they want to see all registers
		cmpa		#ESC
		lbeq		rep_lp									;if they want to bail
		rts
	ENDIF
;
; T{<count>} - Trace into calls
;
@tsteps	lda			#'O'
		jsr			ln_peek_for_letter
		beq			@tover									;if tracing over calls
		bsr			@getcnt									;Y = count
;
@tinto	cmpy		#0
		beq			@done									;if done all steps
		bsr			@trace1									;else next step
		leay		-1,y
		bra			@tinto
;
; TO{<count>} - Trace over calls
;
@tover	bsr			@getcnt									;Y = count
;
@tovlp	cmpy		#0
		beq			@done									;if done all steps
		bsr			@trace1									;next step (D = addr after)
		bne			@tovnxt									;if it wasn't a call
;
		pshs		y										;else save remaining count
		tfr			d,y										;Y = address after call
		bsr			@ttil									;trace till it returns
		puls		y
;
@tovnxt	leay		-1,y
		bra			@tovlp									;next step
;
; Get trace count
; RETURNS: Y = count
;
@getcnt	ldy			#1										;Y = default count
		jsr			ln_at_eoc
		beq			@eoc									;if that's what we're using
		jsr			ln_get_expr
		tfr			d,y										;Y = explicit count
@eoc	jmp			ln_expect_eoc
;
; Trace next instruction, preserve Y
@trace1	pshs		y
		jsr			trace1									;trace next instruction
		puls		y
		bcc			@ret									;if not invalid instr
;
		jsr			msg										;invalid instruction
		fcc			"Undef instr!",CR,LF,BEL,EOS
		jsr			disp_all_regs
		jmp			rep_lp									;bail
