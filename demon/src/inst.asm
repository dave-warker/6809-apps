; "inst.asm" - 6809 instruction encoding/decoding for Demon

; ==> Instruction operand formats (can be used as direct jmp table offset):
OPN_UND		equ		0*2				;undefined
OPN_NON		equ		1*2				;no operand
OPN_IMB		equ		2*2				;immediate byte (#$nn)
OPN_IMW		equ		3*2				;immediate word (#$nnnn)
OPN_DIR		equ		4*2				;direct address (<$nn)
OPN_EXT		equ		5*2				;extended address (>$nnnn)
OPN_IDX		equ		6*2				;indexed (index byte follows)
OPN_RGP		equ		7*2				;register pair (r0,r1)
OPN_RLS		equ		8*2				;register list w/S (r0,r1,...)
OPN_RLU		equ		9*2				;register list w/U (r0,r1,...)
OPN_RAB		equ		10*2			;relative byte address
OPN_RAW		equ		11*2			;relative word address

;@@@
;@@@ Disassembler
;@@@

; ==> Disassemble (& display) one instruction
; PASSED:  X = address of the instruction
; RETURNS: X = address after the instruction (must advance)
; Assumes instruction won't wrap past $FFFF.
inst_disp_instr:
		pshs	x								;in case we need to backtrack
		jsr		inst_get_opcode_index			;D = opcode index
		pshs	d
		bsr		inst_disp_mne					;display mnemonic
		puls	d
		bcs		@und							;if it's not a valid instruction
;
		leas	2,s								;clean up the stack
		bra		inst_disp_operand				;display the operand
;
@und	puls	x								;invalid, display it as an FCB
		ldb		#FCB_MNE_INDEX
		bsr		inst_disp_mne_by_index
		jmp		inst_disp_hex_byte

; ==> Display mnemonic padded to fixed width
; PASSED:  D = mnemonic index
inst_disp_mne:
		jsr		inst_get_mne_for_opcode			;B = mnemonic index
		bcs		@ret							;if it's not valid return CS
;
inst_disp_mne_by_index:
		bsr		inst_get_mne_str				;Y = address of mnemonic
;
		ldb		#DISP_MNE_WIDTH					;overall width
@mnelp	lda		,y
		anda	#$7F
		jsr		io_put_ch
		decb
		tst		,y+
		bpl		@mnelp
;
@padlp	jsr		io_put_space					;pad to proper width
		decb
		bne		@padlp
;
		andcc	#~CC_C							;return CC
@ret	rts
;
; ==> Get address of mnemonic string
; PASSED:  B = (assumed valid) mnemonic index (1-142)
; RETURNS: Y = address of mnemonic string, last byte has MSB set.
inst_get_mne_str:
		ldy		#inst_mne						;Y = start of list
@lp		decb
		beq		@ret							;if at corrent mnemonic
@skip	tst		,y+
		bpl		@skip
		bra		@lp

; ==> Display operand.
; PASSED:  X = address of operand, D = (assumed valid) opcode index
; RETURNS: X = address after operand
inst_disp_operand:
		jsr		inst_get_opn_for_opcode			;D = operand format (OPN_NON, etc.)
		tfr		d,y
		jmp		[@opnds,y]						;assume we'll never get an invalid opcode
;
@opnds	fdb		@ret							;OPN_UND: undefined (should never get)
		fdb		@ret							;OPN_NON: none
		fdb		inst_disp_opn_imb				;OPN_IMB: immediate byte
		fdb		inst_disp_opn_imw				;OPN_IMW: immediate word
		fdb		inst_disp_opn_dir				;OPN_DIR: direct address (byte)
		fdb		inst_disp_opn_ext				;OPN_EXT: extended address (word)
		fdb		inst_disp_opn_idx				;OPN_IDX: indexed operand
		fdb		inst_disp_opn_rgp				;OPN_RGP: register pair
		fdb		inst_disp_opn_rls				;OPN_RLS: register list w/S
		fdb		inst_disp_opn_rlu				;OPN_RLU: register list w/U
		fdb		inst_disp_opn_rab				;OPN_RAB: relative address byte
		fdb		inst_disp_opn_raw				;OPN_RAW: relative address word
;
; ==> #$nn
;
inst_disp_opn_imb:
		lda		#'#'
		jsr		io_put_ch
		bra		inst_disp_hex_byte
;
; ==> <$nn
;
inst_disp_opn_dir:
		lda		#'<'
		jsr		io_put_ch
inst_disp_hex_byte:
		lda		#'$'
		jsr		io_put_ch
		lda		,x+
@dhb	jmp		disp_hex_byte
;
; ==> #$nnnn
;
inst_disp_opn_imw:								;#$nnnn
		bsr		inst_disp_opn_imb
		lda		,x+
		bra		@dhb
;
; ==> >$nnnn
;
inst_disp_opn_ext:
		bsr		inst_disp_hex_byte
		lda		,x+
		bra		@dhb
;
; r0,r1
;
inst_disp_opn_rgp:
		ldb		,x
		lsrb
		lsrb
		lsrb
		lsrb
		bsr		@reg
		lda		#','
		jsr		io_put_ch
		ldb		,x+
@reg	andb	#$0F
		clra
		tfr		d,y
		lda		@pregs,y						;map to our register indicies
		jmp		disp_reg						;and display it
;
@pregs:	fcb		REG_D,REG_X,REG_Y,REG_U,REG_S,REG_PC,0,0,REG_A,REG_B,REG_CC,REG_DP,0,0,0,0
;
; r0,r1,... (w/S)
;
inst_disp_opn_rls:
		ldy		#@rls							;list includes S register
		bra		@rl
;
@rls	fcb		REG_CC,REG_A,REG_B,REG_DP,REG_X,REG_Y,REG_S,REG_PC
@rlu	fcb		REG_CC,REG_A,REG_B,REG_DP,REG_X,REG_Y,REG_U,REG_PC
;
; r0,r1,... (w/U)
;
inst_disp_opn_rlu:
		ldy		#@rlu							;list includes U register
;
@rl		ldb		,x+								;B = register list
@rllp	lda		,y+								;A = next register
		lsrb
		bcs		@rldis							;if need to display it
		bne		@rllp							;if not done
@ret	rts
;
@rldis	cmpa	#REG_A
		bne		@dr								;if not reg A
		bitb	#1								;reg B also set?
		beq		@dr								;nope
;
		lsrb									;yep, eat it
		leay	1,y
		lda		#REG_D							;and display both as D
;
@dr		jsr		disp_reg						;display register
		tstb
		beq		@ret							;if all done
		lda		#','							;else need a comma sep
		jsr		io_put_ch
		bra		@rllp
;
; $nnnn
;
inst_disp_opn_rab:
		ldb		,x+
		sex
		bra		@ra
;
; $nnnn
;
inst_disp_opn_raw:
		ldd		,x++
@ra		leay	d,x								;Y = target address
		lda		#'$'
		jsr		io_put_ch
		tfr		y,d
		jmp		disp_hex_word
;
; Indexed
; 
inst_disp_opn_idx:
		ldb		,x+							;B = U = index format byte
		tfr		b,u
		tstb
		bmi		@ind						;if not 5 bit relative
;
; n,r w/5 bit offset
;
		andb	#%00011111					;isolate offset
		bitb	#%00010000
		beq		@off8						;if no need to sign extend
		orb		#%11100000					;else sign extend to 8 bits
;
@off8	sex									;sign extend to 16 bits
@off16	jsr		disp_dec_sword				;display as 16 bit signed offset
@dcomr	bsr		@dcom						;and a comma
@dreg	tfr		u,a							;and the register (U = index byte)
		anda	#%01100000
		lsra
		lsra
		lsra
		lsra
		lsra
		ldy		#@regs
		lda		a,y
		bra		@putc
;
@regs:	fcb		'X','Y','U','S'
;
@dcom	lda		#','
		bra		@putc
;
; [x] indirection
;
@ind	bitb	#%00010000					;indirection set?
		beq		@etc						;nope
		lda		#'['
		bsr		@putc
		bsr		@etc
		lda		#']'
@putc	jmp		io_put_ch
;
; The rest
;
@etc	andb	#%00001111					;dispatch on bits 3-0
		lslb
		ldy		#@inds
		jmp		[b,y]
;
@inds:	fdb		@pi1						; 0: ,R+
		fdb		@pi2						; 1: ,R++
		fdb		@pd1						; 2: ,-R
		fdb		@pd2						; 3: ,--R
		fdb		@comr						; 4: ,R
		fdb		@boff						; 5: B,R
		fdb		@aoff						; 6: A,R
		fdb		@und						; 7: undefined
		fdb		@s8off						; 8: nn,R (8 bits)
		fdb		@s16off						; 9: nnnnR (16 bits)
		fdb		@und						;10: undefined
		fdb		@doff						;11: D,R
		fdb		@pcr8						;12: 8 bit PC relative
		fdb		@pcr16						;13: 16 bit PC relative
		fdb		@und						;14: undefined
		fdb		@ext						;15: $nnnn
;
@pi1	bsr		@dcom						;,R+
		bsr		@dreg
@dplus	lda		#'+'
		bra		@putc
;
@pi2	bsr		@pi1						;,R++
		bra		@dplus
;
@pd1	bsr		@dcom						;,-R
@pdm	bsr		@dmin
		bra		@dreg
;
@pd2	bsr		@dcom						;,--R
		bsr		@dmin
		bra		@pdm
;
@dmin	lda		#'-'
		bra		@putc
;
@boff	lda		#'B'						;B,R
@roff	bsr		@putc
@comr	bsr		@dcom
		bra		@dreg
;
@aoff	lda		#'A'						;A,R
		bra		@roff
;
@doff	lda		#'D'						;D,R
		bra		@roff
;
@s16off	ldd		,x++						;nnnn,R
@woff	jsr		disp_dec_sword
		bra		@comr
;
@s8off	ldb		,x+							;nn,R
		sex
		bra		@woff
;
@pcr16	ldd		,x++						;nnnn,PCR
@pcr	pshs	x
		addd	,s++						;D = actual address
		jsr		disp_hex_word
		jsr		msg
		fcc	",PCR",EOS
		rts
;
@pcr8	ldb		,x+							;nn,PCR
		sex
		bra		@pcr
;
@ext	jsr		inst_disp_hex_byte			;$nnnn
		lda		,x+
		jmp		disp_hex_byte
;
@und	jsr		msg							;undefined
		fcc	"???",EOS
		rts

;@@@
;@@@ Assembler
;@@@

; ==> Assemble the instruction in the line buffer.
; PASSED:  X = address where instruction should be placed
; RETURNS: X = address after the instruction
; NOTE: Invokes ln_err if there is an error in assembly
inst_asm_ln:
;
; Find the mnemonic index
;
		leas	-MAX_MNE_CHARS,s				;0,s = mnemonic buffer
		tfr		s,y
		ldb		#MAX_MNE_CHARS					;get the mnemonic
		jsr		ln_get_uc_word
		bcs		inst_asm_fail					;if none or it's too long
		tfr		s,y								;look up in mnemonic table
		jsr		inst_get_mne_index				;B = mnemonic index
		bcs		inst_asm_fail					;if not found
		jsr		ln_skip_spaces
		leas	MAX_MNE_CHARS,s
;
; Search through the opcode/mnemonic table looking for opcodes that
; have this mnemonic. Try each until we find one with the correct operand.
; This could be speeded up with a table mapping mnemonics to possible
; opcodes+operands but that would eat a fair amount of space and assembly
; is interactive, one instruction at a time so speed isn't an issue.
;
		pshs	b								;0,s = target mnemonic
		ldu		#0								;U = opcode row index (0-47)
;
; Scan rows of the opcode/mnemonic table
@opcrlp	ldb		inst_opc_mne_row_tbl,u			;B = table row index
		clra
		lslb
		lslb
		lslb
		rola
		lslb
		rola
		ldy		#inst_opc_mne
		leay	d,y								;Y = opcode row pointer
		clrb									;B = column index
;
; Scan columns of the opcode/mnemonic table
@opcclp	lda		,y+								;A = mnemonic index for this opcode
		cmpa	0,s
		beq		@tryopc							;if mnemonic matches
		incb									;else next opcode
@opcnxt	cmpb	#16
		bne		@opcclp
;
		leau	1,u								;next row
		cmpu	#$2F
		ble		@opcrlp							;if not end of opcodes
;
inst_asm_fail:
		jmp		ln_err							;bail
;
; Check if this opcode's operand matches what the user entered
@tryopc	pshs	b,x,y,u							;save state
		ldd		<ln_next						;including line buffer position
		pshs	d
		clra
		tfr		u,b								;D = 00000000.00rrrrrr
		lslb
		lslb
		rolb
		rola
		rolb
		rola									;D = 000000rr.rrrr0000
		orb		2,s								;D = 000000rr.rrrrcccc = opcode index
;
		tsta
		beq		@opc							;if no prefix
		adda	#$10-1
		sta		,x+								;store prefix
		suba	#$10-1
@opc	stb		,x+								;store opcode
;
		jsr		inst_get_opn_for_opcode			;D = operand format
		tfr		d,y
		jsr		[@opns,y]						;try to parse this operand format
		bcc		@ok								;if it was successful
;
		puls	d								;restore state
		std		<ln_next
		puls	b,x,y,u
		bra		@opcnxt							;and try next opcode
;
; Assembled successfully
@ok		leas	10,s							;clean up stack
		rts
;
@opns	fdb		inst_asm_opn_err				;OPN_UND (should never get this)
		fdb		inst_asm_opn_non				;OPN_NON
		fdb		inst_asm_opn_imb				;OPN_IMB
		fdb		inst_asm_opn_imw				;OPN_IMW
		fdb		inst_asm_opn_dir				;OPN_DIR
		fdb		inst_asm_opn_ext				;OPN_EXT
		fdb		inst_asm_opn_idx				;OPN_IDX
		fdb		inst_asm_opn_rgp				;OPN_RGP
		fdb		inst_asm_opn_rls				;OPN_RLS
		fdb		inst_asm_opn_rlu				;OPN_RLU
		fdb		inst_asm_opn_rab				;OPN_RAB
		fdb		inst_asm_opn_raw				;OPN_RAW

; ==> OPN_NON: No operand
inst_asm_opn_non:
inst_asm_opn_end:
		jsr		ln_at_eoc
		bne		inst_asm_opn_err				;if not end of command
;
inst_asm_opn_ok:
		andcc	#~CC_C							;success, return CC
		rts
;
inst_asm_opn_und:
inst_asm_opn_err:
		orcc	#CC_C							;failure, return CS
		rts

; ==> OPN_IMB: Immediate byte - #nn
inst_asm_opn_imb:
		lda		#'#'
inst_asm_opn_prefixed_byte:
		bsr		inst_asm_prefixed_expr			;get '#'<expr>
		bcs		inst_asm_opn_err				;if didn't'
inst_asm_gen_byte:
		jsr		is_byte_value
inst_asm_gen_byte2:
		bcs		inst_asm_fail					;fail if it won't fit in a byte
inst_asm_store_byte:
		stb		,x+								;store byte value
		bra		inst_asm_opn_end				;and ensure that's the end
;
; ==> OPN_IMW: Immediate word - #nnnn
inst_asm_opn_imw:
		lda		#'#'
		bsr		inst_asm_prefixed_expr			;get '#'<expr>
		bcs		inst_asm_opn_err				;if didn't'
inst_asm_store_word:
		std		,x++							;store word value
		bra		inst_asm_opn_end				;and ensure that's the end
;
inst_asm_prefixed_expr:
		jsr		ln_peek_for_ch
		bne		inst_asm_opn_err				;if prefix isn't present
inst_asm_need_expr:
		jsr		ln_try_expr
		bcs		inst_asm_fail					;fail if expression isn't present
		rts										;return expression value in D

; ==> OPN_DIR: Direct address - <nn
inst_asm_opn_dir:
		lda		#'<'
		bra		inst_asm_opn_prefixed_byte

; ==> OPN_EXT: Extended address - >nnnn | nnnn
inst_asm_opn_ext:
		lda		#'>'
		jsr		ln_peek_for_ch
		bne		@expr						;if no prefix
		bsr		inst_asm_need_expr			;else store expression value
		bra		inst_asm_store_word
;
@expr	jsr		ln_try_expr					;look for naked expression
		bcc		inst_asm_store_word			;and store it if found
		rts									;return CS

; ==> OPN_RGP: register pair - r0,r1
inst_asm_opn_rgp:
		bsr		@reg						;get r0
		bcs		@ret						;if none
;
		lslb
		lslb
		lslb
		lslb
		pshs	b							;0,s = r0r00000
;
		jsr		ln_peek_for_comma
		bne		@err						;if no comma separator
		bsr		@reg						;A = r1
		lbcs	inst_asm_fail				;fail if it's missing
;
		orb		,s+							;A = r0r0r1r1
		clra
		bra		inst_asm_gen_byte			;store it
;
@err	puls	a							;clear stack
		orcc	#CC_C						;return CS
@ret	rts
;
@reg	jsr		ln_get_register
		bcs		@ret						;if no register
		ldy		#@regs
		ldb		a,y							;remap to 6809 register pair index
		rts
@regs	fcb		%1000,%1001,%0000,%1011,%0001,%0010,%0011,%1010,%0101,%0100

; ==> OPN_RLS/OPN_RLU: register list - r,r,...
; NOTE: could handle the two cases seperately so we can fail on PSHS S or PULU U
; but frankly it's not worth it. Both will map to the same bit.
inst_asm_opn_rls:
inst_asm_opn_rlu:
		clrb								;B = register list mask
		ldy		#@regs						;Y = map our reg index to 6809 list bit
;
@lp		jsr		ln_get_register
		bcs		@end						;if no more registers
		orb		a,y							;else set bit for this register
		jsr		ln_peek_for_comma
		beq		@lp							;if more are coming
;
@end	clra
		tstb
		lbne	inst_asm_gen_byte			;if got at least one register
;
		orcc	#CC_C						;else return CS
		rts
;
@regs	fcb		%00000010,%00000100,%00000110,%00001000,%00010000,%00100000,%01000000,%00000001,%10000000,%01000000

; ==> OPN_RAB: relative addres byte - <expr> | *{[+|-]<expr>}
inst_asm_opn_rab:
		lda		#1							;get relative offset
		bsr		inst_asm_get_ra
		bcs		@ret						;if not found
		jsr		is_byte_offset
		jmp		inst_asm_gen_byte2			;else store it if it fits		
;
; ==> OPN_RAW: relative addres word - <expr> | *{[+|-]<expr>}
inst_asm_opn_raw:
		lda		#2							;get relative offset
		bsr		inst_asm_get_ra
		lbcc	inst_asm_store_word			;store it if present
@ret	rts									;else return CS

; ==> Get relative address
; PASSED:  A = size of the address (1 or 2 bytes)
; RETURNS: CS if none found else CC,D = relative address
inst_asm_get_ra:
		leay	a,x							;Y = origin for relative address
		lda		#'*'
		jsr		ln_peek_for_ch
		beq		@off						;if specified explicit offset
;
		jsr		ln_try_expr					;get target address
		bcs		@ret						;if none found
		pshs	y
		subd	,s++						;D = relative address
@ok		andcc	#~CC_C						;return CS
@ret	rts
;
@off	ldd		<asm_addr
		pshs	y
		subd	,s++						;D = relative address of start of inst
;
		jsr		ln_at_eoc
		beq		@ok							;return that as-is if no offset specified
		tfr		d,y							;Y = relative address of start of inst
		jsr		ln_peek_for_minus
		beq		@minus						;if -<expr>
		jsr		ln_peek_for_plus
		lbne	inst_asm_fail				;fail if not +<expr>
;
		jsr		inst_asm_need_expr
		leay	d,y
		tfr		y,d							;D = offset
		bra		@ok
;
@minus	jsr		inst_asm_need_expr
		pshs	d
		tfr		y,d
		subd	,s++						;D = offset
		bra		@ok

; ==> OPN_IDX: Indexed modes
;
inst_asm_opn_idx:
		lda		#'['
		jsr		ln_peek_for_ch
		beq		@xind						;if explicit indirection
;
		clrb								;B = initial index byte
		bsr		@rind?						;parse other indexed modes
		bcc		@end						;if it's valid
@ret	rts									;else return CS
;
@xind	ldb		#%00010000					;B = index byte with indirection
		bsr		@rind?						;parse other modes
		bcs		@ret						;if not indexed mode
		lda		#']'
		jsr		ln_peek_for_ch				;eat optional closing bracket
@end	jmp		inst_asm_opn_end			;and ensure end of buffer
;
; ==> Indexed register indirect - ,R ,-R ,--R ,R+ ,R++
@rind?	jsr		ln_peek_for_comma
		bne		@aidx?						;if not register indirect
;
		jsr		ln_peek_for_minus
		bne		@pinc?						;if not pre-decrement
		orb		#%10000010					;assume ,-R
		jsr		ln_peek_for_minus
		bne		@pdec						;if that's what it is
		incb								;else ,--R
@pdec	jsr		@idxreg						;add index register
@failne	lbcs	inst_asm_fail				;fail if it's missing
@stidx	stb		,x+							;store index byte
		andcc	#~CC_C						;ensure CC
		rts
;
@pinc?	jsr		@idxreg						;look for index register
		bcs		@failne						;fail if it's not there
		orb		#%10000100					;assume ,R
		jsr		ln_peek_for_plus
		bne		@stidx						;if that's what it is
		eorb	#%00000100					;else flip to ,R+
		jsr		ln_peek_for_plus
		bne		@stidx						;if that's what it is
		incb								;else bump to ,R++
		bra		@stidx
;
; ==> Accumulator indexed - A,R B,R D,R
@aidx?	ldy		<ln_next					;save current line position
		jsr		ln_get_register
		bcs		@oidx?						;if not a register
		cmpa	#REG_D
		bhi		@oidx?						;if not accumulator
		pshs	a
		jsr		ln_peek_for_comma
		puls	a
		bne		@oidx?						;if no comma separator
;
		tfr		a,b
		clra
		pshs	d
		jsr		@idxreg						;add index register
		puls	y
		lbcs	inst_asm_fail				;if not present
		orb		@aidxs,y					;set to acc indexed mode
		bra		@stidx						;and store it
;
@aidxs	fcb		%10000110,%10000101,%10001011	;A,B,D
;
; ==> Offset indexed n,R n,PCR or Extended indexed [nnnn]
@oidx?	sty		<ln_next					;restore line position
		pshs	b							;save index byte
		jsr		ln_try_expr
		bcc		@off						;if got offset expression
		puls	b							;else not indexed operand
		rts									;return CS
;
@off	tfr		d,y							;Y = offset
		puls	b							;B = index byte
		jsr		ln_peek_for_comma
		bne		@iind?						;if not offset indexed
;
		pshs	x
		leax	@pcr,pcr
		jsr		ln_peek_for_string
		puls	x
		bne		@offr						;if not PC relative
;
		orb		#%10001100					;assume nn,PCR
		stb		,x+
		leay	-1,y						;Y = target address
		tfr		y,d
		pshs	x
		subd	,s++						;D = byte offset
		jsr		is_byte_offset
		bcs		@pcrw						;if doesn't fit in byte
;
@bsto	stb		,x+							;store byte offset
		andcc	#~CC_C						;return CC
		rts
;
@pcr	fcc		"PCR",EOS
;
@pcrw	inc		-1,x						;bump to word index
		subd	#1							;D = word offset
@wsto	std		,x++						;store word offset
@ok		andcc	#~CC_C						;return CC
		rts
;
@offr	bsr		@idxreg						;should be index register next
		lbcs	inst_asm_fail				;fail if not
		orb		#%10001001					;assume word offset
		stb		,x+							;store index byte
;
		tfr		y,d							;D = offset
		jsr		is_byte_offset
		bcs		@wsto						;if it is word offset
		dec		-1,x						;else assume byte offset
		cmpb	#-16
		blt		@bsto						;if need byte offset
		cmpb	#15
		bgt		@bsto						;if need byte offset
;
		lda		-1,x						;make sure Indirect isn't set
		bita	#%00010000
		bne		@bsto						;else can't use 5 bit offset
;
		andb	#%00011111					;else fits in index byte
		eorb	#%10001000
		eorb	-1,x
		stb		-1,x
		bra		@ok							;return CC
;
@iind?	bitb	#%00010000
		beq		@err						;if not explicit indirect then error
;
		orb		#%10001111					;set as extended indirect
		stb		,x+
		tfr		y,d							;D = extended address
		bra		@wsto						;store it
;
@err	orcc	#CC_C						;return CS
		rts
;
@idxreg	jsr		ln_get_register
		bcs		@ret2						;return CS
		pshs	b,y
		leay	@idxs,pcr
		ldb		a,y							;B = register mapped to index byte
		lbmi	inst_asm_fail				;if invalid index register
		orb		,s+							;merge into index byte
		puls	y
@ret2	rts									;return CC
;
@idxs	fcb	-1,-1,-1,-1,%00000000,%00100000,%01000000,-1,-1,%01100000	;X,Y,U,S
		
; ==> Get mnemonic index
; PASSED:  Y = address of mnemonic string (MSB set on last char)
; RETURNS: CS if not found else CC,B = mnemonic index (1-142)
inst_get_mne_index:
		pshs	x
		ldx		#inst_mne					;X = start of mnemonics
		clrb								;B = mnemonic index
;
@mnelp	incb								;next index
		pshs	y
@cmplp	lda		,y+
		cmpa	,x
		bne		@nope						;if doesn't match
		tst		,x+
		bpl		@cmplp						;if not end of mnemonic
;
		puls	x,y
		exg		x,y
		rts									;return CC, B = index
;
@nope	puls	y							;reset pointer
@mneskp	tst		,x+
		bmi		@mnelp						;if hit end of mnemonic
		bne		@mneskp						;if not end of table
;
		orcc	#CC_C						;return CS
		puls	x
		rts

;@@@
;@@@ Support
;@@@

; ==> Get opcode index for an instruction.
; PASSED:  X = address of the instruction
; RETURNS: D = opcode index ($000 - $2FF), X = address after opcode
; 			WHERE: $000-$0ff = unprefixex opcodes, $100-$1FF = $10 prefixed, $200-$2FF = $11 prefixed
inst_get_opcode_index:
		clra
		ldb		,x+							;D = first opcode byte
		cmpb	#$10
		beq		@pre10						;if it's a $10 prefix
		cmpb	#$11
		bne		@ret						;if it's not $11 prefixed
		inca								;bump by $200 for $11
@pre10	inca								;or by $100 for $10
		ldb		,x+							;B = prefixed opcode byte
@ret	rts

; ==> Get mnemonic index for opcode index.
; PASSED:  D = opcode index ($000 - $2FF)
; RETURNS: CS if invalid opcode or CC, B = mnemonic index (1 - 142)
; 1 byte/opcode, 16 bytes/row, 48 uncompressed rows total.
inst_get_mne_for_opcode:					;D = 000000rr.rrrrcccc (r = opcode row, c = column)
		rora
		rorb
		rora
		rorb
		rora
		rorb
		rora
		rorb
		rora								;D = ccccx000.00rrrrrr (x = don't care)
		ldy		#inst_opc_mne_row_tbl
		ldb		b,y							;D = ccccx000.000RRRRR (R = remapped row)
		bmi		@und						;if undefined row
		rola
		rolb
		rola
		rolb
		rola
		rolb
		rola
		rolb
		rola								;D = 0000000R.RRRRcccc
		leay	inst_opc_mne_row_tbl_size,y
		ldb		d,y							;B = mnemonic index
@und?	beq		@und						;if undefined
;
@ok		andcc	#~CC_C						;return CC, B = index
		rts
;
@und	orcc	#CC_C						;return CS
		rts
;
; ==> Get operand type for (assumed valid) opcode index.
; PASSED:  D = opcode index ($000 - $2FF)
; RETURNS: D = OPN_UND, etc
inst_get_opn_for_opcode:
		pshs	b							;D = 000000rr.rrrrcccc (r = row, c = column)
		lsra
		rorb
		lsra
		rorb
		lsrb
		lsrb								;D = 00000000.00rrrrrr
		leay	inst_opc_opn,pcr			;Y = base of operand type table
		ldb		b,y							;B = (provisional) operand type for entire row
		puls	a							;(A = rrrrcccc)
		bpl		@ret						;if entire row uses same operand type
;
		lslb								;B = offset from start of table for this row ($00-$7F)
		leay	b,y							;Y = address of row
		anda	#$0F						;A = offset into the row
		ldb		a,y							;B = actual operand type
;
@ret	clra
		rts

; ==> Maps opcodes to their operand formats.
; NOTE: operand format for undefined opcodes isn't guaranteed to be accurate.
; Entries with MSB set have individual rows at table offset: (entry & $7F) << 1
inst_opc_opn:
	fcb	OPN_DIR,$80+$18,OPN_RAB,$80+$20,OPN_NON,OPN_NON,OPN_IDX,OPN_EXT,$80+$28,OPN_DIR,OPN_IDX,OPN_EXT,$80+$30,OPN_DIR,OPN_IDX,OPN_EXT	;unprefixed
	fcb	OPN_UND,OPN_UND,OPN_RAW,OPN_NON,OPN_UND,OPN_UND,OPN_UND,OPN_UND,OPN_IMW,OPN_DIR,OPN_IDX,OPN_EXT,OPN_IMW,OPN_DIR,OPN_IDX,OPN_EXT	;$10 prefixed
	fcb	OPN_UND,OPN_UND,OPN_UND,OPN_NON,OPN_UND,OPN_UND,OPN_UND,OPN_UND,OPN_IMW,OPN_DIR,OPN_IDX,OPN_EXT,OPN_UND,OPN_UND,OPN_UND,OPN_UND	;$11 prefixed
	fcb	OPN_UND,OPN_UND,OPN_NON,OPN_NON,OPN_UND,OPN_UND,OPN_RAW,OPN_RAW,OPN_UND,OPN_NON,OPN_IMB,OPN_UND,OPN_IMB,OPN_NON,OPN_RGP,OPN_RGP	;$10-$1F
	fcb	OPN_IDX,OPN_IDX,OPN_IDX,OPN_IDX,OPN_RLU,OPN_RLU,OPN_RLS,OPN_RLS,OPN_UND,OPN_NON,OPN_NON,OPN_NON,OPN_IMB,OPN_NON,OPN_UND,OPN_NON	;$30-$3F
	fcb	OPN_IMB,OPN_IMB,OPN_IMB,OPN_IMW,OPN_IMB,OPN_IMB,OPN_IMB,OPN_UND,OPN_IMB,OPN_IMB,OPN_IMB,OPN_IMB,OPN_IMW,OPN_RAB,OPN_IMW,OPN_UND	;$80-$8F
	fcb	OPN_IMB,OPN_IMB,OPN_IMB,OPN_IMW,OPN_IMB,OPN_IMB,OPN_IMB,OPN_UND,OPN_IMB,OPN_IMB,OPN_IMB,OPN_IMB,OPN_IMW,OPN_UND,OPN_IMW,OPN_UND	;$C0-$CF

; ==> Table of mnemonics
inst_mne:
	fcs	"ABX"	;  1
	fcs	"ADCA"	;  2
	fcs	"ADCB"	;  3
	fcs	"ADDA"	;  4
	fcs	"ADDB"	;  5
	fcs	"ADDD"	;  6
	fcs	"ANDA"	;  7
	fcs	"ANDB"	;  8
	fcs	"ANDCC"	;  9
	fcs	"ASL"	; 10
	fcs	"ASLA"	; 11
	fcs	"ASLB"	; 12
	fcs	"ASR"	; 13
	fcs	"ASRA"	; 14
	fcs	"ASRB"	; 15
	fcs	"BCC"	; 16
	fcs	"BCS"	; 17
	fcs	"BEQ"	; 18
	fcs	"BGE"	; 19
	fcs	"BGT"	; 20
	fcs	"BHI"	; 21
	fcs	"BHS"	; 22
	fcs	"BITA"	; 23
	fcs	"BITB"	; 24
	fcs	"BLE"	; 25
	fcs	"BLO"	; 26
	fcs	"BLS"	; 27
	fcs	"BLT"	; 28
	fcs	"BMI"	; 29
	fcs	"BNE"	; 30
	fcs	"BPL"	; 31
	fcs	"BRA"	; 32
	fcs	"BRN"	; 33
	fcs	"BSR"	; 34
	fcs	"BVC"	; 35
	fcs	"BVS"	; 36
	fcs	"CLR"	; 37
	fcs	"CLRA"	; 38
	fcs	"CLRB"	; 39
	fcs	"CMPA"	; 40
	fcs	"CMPB"	; 41
	fcs	"CMPD"	; 42
	fcs	"CMPS"	; 43
	fcs	"CMPU"	; 44
	fcs	"CMPX"	; 45
	fcs	"CMPY"	; 46
	fcs	"COM"	; 47
	fcs	"COMA"	; 48
	fcs	"COMB"	; 49
	fcs	"CWAI"	; 50
	fcs	"DAA"	; 51
	fcs	"DEC"	; 52
	fcs	"DECA"	; 53
	fcs	"DECB"	; 54
	fcs	"EORA"	; 55
	fcs	"EORB"	; 56
	fcs	"EXG"	; 57
FCB_MNE_INDEX equ 58
	fcs	"FCB"	; 58
	fcs	"FCC"	; 59
	fcs	"FDB"	; 60
	fcs	"INC"	; 61
	fcs	"INCA"	; 62
	fcs	"INCB"	; 63
	fcs	"JMP"	; 64
	fcs	"JSR"	; 65
	fcs	"LBCC"	; 66
	fcs	"LBCS"	; 67
	fcs	"LBEQ"	; 68
	fcs	"LBGE"	; 69
	fcs	"LBGT"	; 70
	fcs	"LBHI"	; 71
	fcs	"LBHS"	; 72
	fcs	"LBLE"	; 73
	fcs	"LBLO"	; 74
	fcs	"LBLS"	; 75
	fcs	"LBLT"	; 76
	fcs	"LBMI"	; 77
	fcs	"LBNE"	; 78
	fcs	"LBPL"	; 79
	fcs	"LBRA"	; 80
	fcs	"LBRN"	; 81
	fcs	"LBSR"	; 82
	fcs	"LBVC"	; 83
	fcs	"LBVS"	; 84
	fcs	"LDA"	; 85
	fcs	"LDB"	; 86
	fcs	"LDD"	; 87
	fcs	"LDS"	; 88
	fcs	"LDU"	; 89
	fcs	"LDX"	; 90
	fcs	"LDY"	; 91
	fcs	"LEAS"	; 92
	fcs	"LEAU"	; 93
	fcs	"LEAX"	; 94
	fcs	"LEAY"	; 95
	fcs	"LSL"	; 96
	fcs	"LSLA"	; 97
	fcs	"LSLB"	; 98
	fcs	"LSR"	; 99
	fcs	"LSRA"	;100
	fcs	"LSRB"	;101
	fcs	"MUL"	;102
	fcs	"NEG"	;103
	fcs	"NEGA"	;104
	fcs	"NEGB"	;105
	fcs	"NOP"	;106
	fcs	"ORA"	;107
	fcs	"ORB"	;108
	fcs	"ORCC"	;109
	fcs	"PSHS"	;110
	fcs	"PSHU"	;111
	fcs	"PULS"	;112
	fcs	"PULU"	;113
	fcs	"ROL"	;114
	fcs	"ROLA"	;115
	fcs	"ROLB"	;116
	fcs	"ROR"	;117
	fcs	"RORA"	;118
	fcs	"RORB"	;119
	fcs	"RTI"	;120
	fcs	"RTS"	;121
	fcs	"SBCA"	;122
	fcs	"SBCB"	;123
	fcs	"SEX"	;124
	fcs	"STA"	;125
	fcs	"STB"	;126
	fcs	"STD"	;127
	fcs	"STS"	;128
	fcs	"STU"	;129
	fcs	"STX"	;130
	fcs	"STY"	;131
	fcs	"SUBA"	;132
	fcs	"SUBB"	;133
	fcs	"SUBD"	;134
	fcs	"SWI"	;135
	fcs	"SWI2"	;136
	fcs	"SWI3"	;137
	fcs	"SYNC"	;138
	fcs	"TFR"	;139
	fcs	"TST"	;140
	fcs	"TSTA"	;141
	fcs	"TSTB"	;142
	fcb	0

; ==> Map opcodes to mnemonics
; Mnemonic indicies run from 1 - 142 with 0 used for undefined opcodes.
; Opcode mapping: 000000rrrrrrcccc, where r = table row, c = column.
; The row table remaps r (0-47) so duplicate rows share the same entry
; and rows with no valid opcodes ($FF) don't occupy a row at all.
inst_opc_mne_row_tbl:
	fcb	$00,$01,$02,$03,$04,$05,$06,$06,$07,$08,$08,$08,$09,$0A,$0A,$0A	;$000-$0FF: unprefixed
	fcb	$FF,$FF,$0B,$0C,$FF,$FF,$FF,$FF,$0D,$0E,$0E,$0E,$0F,$10,$10,$10	;$100-$1FF: $10 prefixed
	fcb	$FF,$FF,$FF,$11,$FF,$FF,$FF,$FF,$12,$12,$12,$12,$FF,$FF,$FF,$FF	;$200-$2FF: $11 prefixed
inst_opc_mne_row_tbl_size equ *-inst_opc_mne_row_tbl
;
inst_opc_mne:
; $000-$0FF: Unprefixed opcodes
	fcb	103,0,0,47,99,0,114,13,96,114,52,0,61,140,64,37			;$00: $000-$00F
	fcb	0,0,106,138,0,0,80,82,0,51,109,0,9,124,57,139			;$01: $010-$01F
	fcb	32,33,21,27,16,17,30,18,35,36,31,29,19,28,20,25			;$02: $020-$02F
	fcb	94,95,92,93,110,112,111,113,0,121,1,120,50,102,0,135	;$03: $030-$03F
	fcb	104,0,0,48,100,0,118,14,97,115,53,0,62,141,0,38			;$04: $040-$04F
	fcb	105,0,0,49,101,0,119,15,98,116,54,0,63,142,0,39			;$05: $050-$05F
	fcb	103,0,0,47,99,0,114,13,96,114,52,0,61,140,64,37			;$06: $060-$07F *
	fcb	132,40,122,134,7,23,85,0,55,2,107,4,45,34,90,0			;$07: $080-$08F
	fcb	132,40,122,134,7,23,85,125,55,2,107,4,45,65,90,130		;$08: $090-$0BF *
	fcb	133,41,123,6,8,24,86,0,56,3,108,5,87,0,89,0				;$09: $0C0-$0CF
	fcb	133,41,123,6,8,24,86,126,56,3,108,5,87,127,89,129		;$0A: $0D0-$0FF *
; $100-$1FF: $10 prefixed opcodes
	fcb	0,81,71,75,66,74,78,68,83,84,79,77,69,76,70,73			;$0B: $120-$12F
	fcb	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,136						;$0C: $130-$13F
	fcb	0,0,0,42,0,0,0,0,0,0,0,0,46,0,91,0						;$0D: $180-$18F
	fcb	0,0,0,42,0,0,0,0,0,0,0,0,46,0,91,131					;$0E: $190-$1BF *
	fcb	0,0,0,0,0,0,0,0,0,0,0,0,0,0,88,0						;$0F: $1C0-$1CF
	fcb	0,0,0,0,0,0,0,0,0,0,0,0,0,0,88,128						;$10: $1D0-$1FF *
; $200-$2FF: $11 prefixed opcodes
	fcb	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,137						;$11: $230-$23F
	fcb	0,0,0,44,0,0,0,0,0,0,0,0,43,0,0,0						;$12: $280-$2BF *
