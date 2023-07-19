; "trace.asm" - Execution trace support

TOP_UND	equ		0*2		;undefined opcode
TOP_P10	equ		1*2		;$10 prefix
TOP_P11	equ		2*2		;$11 prefix
TOP_EX1	equ		3*2		;execute 1 byte instruction
TOP_EX2	equ		4*2		;execute 1 byte instruction
TOP_EX3	equ		5*2		;execute 1 byte instruction
TOP_EX4	equ		6*2		;execute 1 byte instruction
TOP_EX5	equ		7*2		;execute 1 byte instruction
TOP_RBU	equ		8*2		;unconditional relative branch (byte)
TOP_RWU	equ		9*2		;unconditional relative branch (word)
TOP_RBC	equ		10*2	;conditional relative branch (byte)
TOP_RWC	equ		11*2	;conditional relative branch (word)
TOP_RCB	equ		12*2	;relative call (byte)
TOP_RCW	equ		13*2	;relative call (word)
TOP_RTS	equ		14*2	;return from subroutine
TOP_RTI	equ		15*2	;return from interrupt
TOP_JPD	equ		16*2	;jump direct
TOP_JPE	equ		17*2	;jump extended
TOP_JSD	equ		18*2	;jump to sub direct
TOP_JSE	equ		19*2	;jump to sub extended
TOP_IDX	equ		20*2	;unprefixed indexed
TOP_RGP	equ		21*2	;register pair (EXG/TFR)
TOP_RGL	equ		22*2	;register list (PSH/PUL)
TOP_SWI	equ		23*2	;SWI
TOP_SWN	equ		24*2	;SWI2, SWI3
TOP_JPX	equ		25*2	;jmp indexed
TOP_JSX	equ		26*2	;jsr indexed

TOP		macro noexpand	;generates a row (16 bytes) of trace operations
		fcb		TOP_{1},TOP_{2},TOP_{3},TOP_{4},TOP_{5},TOP_{6},TOP_{7},TOP_{8}
		fcb		TOP_{9},TOP_{10},TOP_{11},TOP_{12},TOP_{13},TOP_{14},TOP_{15},TOP_{16}
		endm

; ==> Trace one instruction using current CPU state
; RETURNS: CS if was invalid instruction,
;		   EQ if was call and D = next addr after inst else NE
trace1:
		ldy		<cpu_reg_pc							;Y = current PC
		leax	top_do_unprefixed,pcr				;X = unprefixed opcode table
		ldb		,y+									;B = opcode
;
trace_opc:
		abx
		ldb		,x									;B = opcode trace action
		leax	trace_opc_actions,pcr
		abx
		jmp		[,x]								;handle trace action (passed B = action)
;
trace_opc_actions:
		fdb		top_do_undefined		;TOP_UND: undefined opcode
		fdb		top_do_10_prefix		;TOP_P10: $10 prefixed opcodes
		fdb		top_do_11_prefix		;TOP_P11: $11 prefixed opcodes
		fdb		top_do_exe				;TOP_EXE1: execute 1 byte inst
		fdb		top_do_exe				;TOP_EXE2: execute 1 byte inst
		fdb		top_do_exe				;TOP_EXE3: execute 1 byte inst
		fdb		top_do_exe				;TOP_EXE4: execute 1 byte inst
		fdb		top_do_exe				;TOP_EXE5: execute 1 byte inst
		fdb		top_do_rbu				;TOP_RBU: uncond rel branch byte
		fdb		top_do_rwu				;TOP_RWU: uncond rel branch word
		fdb		top_do_rbc				;TOP_RBC: cond rel branch byte
		fdb		top_do_rwc				;TOP_RWC: cond rel branch word
		fdb		top_do_rcb				;TOP_RCB: rel call byte
		fdb		top_do_rcw				;TOP_RCW: rel call word
		fdb		top_do_rts				;TOP_RTS: return from subroutine
		fdb		top_do_rti				;TOP_RTI: return from interrupt
		fdb		top_do_jpd				;TOP_JPD: jump direct
		fdb		top_do_jpe				;TOP_JPE: jump extended
		fdb		top_do_jsd				;TOP_JSD: jump to sub direct
		fdb		top_do_jse				;TOP_JSE: jump to sub extended
		fdb		top_do_idx				;TOP_IDX: unprefixed indexed (non-PC dest)
		fdb		top_do_rgp				;TOP_RGP: register pair (EXG/TFR)
		fdb		top_do_rgl				;TOP_RGL: register list (PSH/PUL)
		fdb		top_do_swi				;TOP_SWI: SWI
		fdb		top_do_swn				;TOP_SWN: SWI2, SWI3
		fdb		top_do_jpx				;TOP_JPX: jump indexed
		fdb		top_do_jsx				;TOP_JSX: jump to sub indexed

; ==> TRC_ACT_UND: undefined opcode
top_do_undefined:
		orcc	#CC_C								;return CS
		rts

; ==> TOP_P10: $10 prefixed opcodes
top_do_10_prefix:
		leax	top_do_0_prefixed,pcr				;dispatch $10 prefixed opcodes
		ldb		,y+									;B = prefixed opcode
		subb	#TOP_0_PREFIXED_1ST					;offset to first defined opcode row
		bhs		trace_opc							;if it's valid
		bra		top_do_undefined					;else undefined opcode

; ==> TOP_P11: $11 prefixed opcodes
top_do_11_prefix:
		leax	top_do_1_prefixed,pcr				;dispatch $11 prefixed opcodes
		ldb		,y+									;B = prefixed opcode
		subb	#TOP_1_PREFIXED_1ST					;offset to first defined opcode row
		blo		top_do_undefined					;if it's below that it's undefined
		cmpb	#TOP_1_PREFIXED_LAST-TOP_1_PREFIXED_1ST
		bls		trace_opc							;if it's in valid range
		bra		top_do_undefined					;else undefined

; ==> TRC_ACT_EXE1-TRC_ACT_EXE5: execute instruction
top_do_exe:
		subb	#TOP_EX1
		lsrb										;B = byte count - 1 (0-4)
;
top_do_exe_copy:
		ldy		<cpu_reg_pc							;Y = instruction
		ldx		#trace_instr_buf					;X = inst buffer
@cpylp	lda		,y+									;copy instruction to buffer
		sta		,x+
		decb
		bpl		@cpylp
;
top_do_exe_inst:
		sty		<cpu_reg_pc							;Y = our new PC
top_do_exe_buf:
		sts		<trace_sp							;save our stack
;
		lda		#$7E								;append a jump back here
		sta		,x+
		leay	@cont,pcr
		sty		,x++
;
		leau	cpu_reg_cc,pcr						;restore registers
		pulu	cc,d,dp,x,y,s						;(may enable interrupts)
		leau	[cpu_reg_u,pcr]
		jmp		>trace_instr_buf					;execute it
;
@cont	pshs	cc									;because the STU will change them!
		stu		>cpu_reg_u							;save CPU state
		leau	cpu_reg_y+2,pcr
		pshu	y,x,dp,d
		lda		#OUR_DATA_DP
		tfr		a,dp
		puls	a
		sta		<cpu_reg_cc
		sts		<cpu_reg_s
		lds		<trace_sp							;restore our stack
;
top_ok_not_call:
		andcc	#~(CC_C+CC_Z)						;return CC, NE
		rts

; ==> TOP_RBU: unconditional rel branch byte
top_do_rbu:
		ldb		,y+
		sex											;D = 16 bit relative address
		bra		@rwu
;
; ==> TOP_RWU: unconditional rel branch word
top_do_rwu:
		ldd		,y++								;D = 16 bit relative address
@rwu	leay	d,y									;Y = new PC
top_do_jump:
		sty		<cpu_reg_pc
		bra		top_ok_not_call						;return CC, NE

; ==> TOP_RBC: conditional rel branch byte
top_do_rbc:
		bsr		@tcond								;test condition
		beq		top_do_rbu							;it it's true
		leay	1,y									;else skip over inst
		bra		top_do_jump							;update PC and return
;
; ==> TOP_RWC: conditional rel branch byte
top_do_rwc:
		bsr		@tcond								;test condition
		beq		top_do_rwu							;if it's true
		leay	2,y									;else skip over inst
		bra		top_do_jump
;
@tcond	ldx		#trace_instr_buf					;build Bxx *+3; INCA; RTS
		lda		-1,y								;Bxx or LBxx opcode
		sta		,x+									;(LBxx == Bxx w/o $10 prefix)
		ldd		#$014C
		std		,x++								;*+3; INCA
		lda		#$39								;RTS
		sta		,x
		clra										;A = 0
		ldb		<cpu_reg_cc
		tfr		b,cc								;restore CC
		jsr		<trace_instr_buf
		tsta										;EQ if true, NE if false
		rts

; ==> TOP_RCB: relative call (byte)
top_do_rcb:
		ldb		,y+
		sex											;D = 16 bit offset, Y = return address
		bra		@rcw
;
; ==> TOP_RCW: relative call (word)
top_do_rcw:
		ldd		,y++								;D = 16 bit offset
;
@rcw	exg		d,y									;D = return address
		leay	d,y									;Y = new PC
top_do_call:
		sty		<cpu_reg_pc
;
		ldx		<cpu_reg_s							;push return address
		std		,--x
		stx		<cpu_reg_s
;
		andcc	#~CC_C								;return CC,EQ
		orcc	#CC_Z
		rts

; ==> TOP_RTS: return from subroutine
top_do_rts:
		ldx		<cpu_reg_s							;pop return address
@pc		ldy		,x++
		stx		<cpu_reg_s
		bra		top_do_jump							;set as new PC
;
; ==> TOP_RTI: return from interrupt
top_do_rti:
		ldx		<cpu_reg_s							;pop state from stack
		lda		,x+									;"restore" CC
		sta		<cpu_reg_cc
		bpl		@pc									;if only CC,PC were stacked
;
		ldu		#cpu_reg_a							;"restore" CC,D,DP,X,Y
		ldb		#7
@reslp	lda		,x+
		sta		,u+
		decb
		bne		@reslp
		ldd		,x++								;"restore" U
		std		<cpu_reg_u
		bra		@pc									;set new PC

; ==> TOP_JPD: jump direct
top_do_jpd:
		ldb		,y+
		lda		<cpu_reg_dp
		tfr		d,y									;Y = new PC
		bra		top_do_jump

; ==> TOP_JPE: jump extended
top_do_jpe:
		ldy		,y++								;Y = new PC
		bra		top_do_jump

; ==> TOP_JSD: jump to sub direct
top_do_jsd:
		ldb		,y+
		lda		<cpu_reg_dp
@jsd	exg		d,y									;Y = new PC, D = return address
		bra		top_do_call
;
; ==> TOP_JSE: jump to sub extended
top_do_jse:
		ldd		,y++								;D = new PC
		bra		@jsd

; ==> TOP_IDX:  non-PC indexed
top_do_idx:
		pshs	y									;copy opcode byte(s) to buffer
		ldy		<cpu_reg_pc
		ldx		#trace_instr_buf
@opclp	lda		,y+
		sta		,x+
		cmpy	,s
		bne		@opclp
		puls	y
;
		bsr		top_append_idx						;copy index bytes (adjusting PCR as needed)
		jmp		top_do_exe_inst						;update PC and execute it
;
; Append index byte and argument to the trace buffer.
; PCR modes are adjusted for address difference between source and trace buffer.
; PASSED:  Y = addr of source index byte for inst, X = buff addr where it goes
; RETURNS: Y = addr after the source index bytes, X = after the buffer bytes
top_append_idx:
		ldb		,y									;B = index byte
		andb	#%10001110
		cmpb	#%10001100
		beq		@pcr								;if it's a PCR mode
;
; Non-PCR modes
;
		ldb		,y									;B = index byte
		bmi		@idxb								;if not 5 bit offset
		ldb		#1									;else use byte count of 1
		bra		@idxlp
;
@idxb	andb	#$0F								;get byte count from table
		pshs	x
		leax	@idxs,pcr
		ldb		b,x									;B = byte count for this mode
		puls	x
;
@idxlp	lda		,y+									;copy index bytes
		sta		,x+									;to trace buffer
		decb
		bne		@idxlp
		rts
;
@idxs	fcb		1,1,1,1,1,1,1,1,2,3,1,1,2,3,1,3
;
; PCR modes
;
@pcr	lda		,y									;copy PCR index byte
		ora		#%00000001							; forcing a 16 bit offset
		sta		,x+
;
		lda		,y+									;A = original index byte
		lsra
		bcs		@pcrw								;if it's already 16 bits
		ldb		,y+
		sex											;D = offset
		bra		@adj
@pcrw	ldd		,y++								;D = offset
;
; Adjust the original PCR offset so it points to the same address
; as the source even though we're executing from the trace buffer.
; new off = source off + addr source end - addr trace buffer end
; (D = offset, Y = source end, X = addr to store adjusted offset)
;
@adj	pshs	y									;,S = src end
		addd	,s++								;D = src off + src end
		pshs	x									;,S = buf end - 2
		subd	,s++
		subd	#2									;D = src off + src end - buf end
		std		,x++								;store adjusted offset
		rts

; ==> TOP_RGP: register pair (EXG/TFR)
top_do_rgp:
		lda		,y+									;A = register pair
		eora	#%01010101							;PC,PC (PC == %0101)
		bita	#%11110000
		beq		@pc									;if r0 == pc
		bita	#%00001111
		beq		@pc									;if r1 == pc
;
top_do_exe_copy2:
		ldb		#2-1								;no PC reg, execute it as-is
		jmp		top_do_exe_copy
;
@pc		sty		<cpu_reg_pc							;update PC for below
;
		bsr		@r0
		bsr		@getr
		tfr		x,u									;U = r0 value
		ldb		-1,y
		bsr		@getr								;X = r1 value
;
		exg		x,u									;X = r0 value, U = r1 value
		ldb		-1,y
		bsr		@setr								;r1 = r0 value
;
		lda		#$01
		bita	-2,y								;test bit 0 of opcode
		bne		@done								;we're done if it's TFR
;
		bsr		@r0									;else set r0 = r1 value
		tfr		u,x
		bsr		@setr
;
@done	jmp		top_ok_not_call						;and we're done
;
@r0		ldb		-1,y
		lsrb
		lsrb
		lsrb
		lsrb										;B = r0 index
		rts
;
@getr	bsr		@ourr								;D = our register index
		tfr		d,x
		jsr		reg_get_value						;D = register value
		tst		@bytes,x
		beq		@retx								;if no special handling needed
		lda		@bytes,x							;8 bit regs use $FF for high byte
		bmi		@retx
		tfr		b,a									;except for CC, DP (of course)
@retx	tfr		d,x									;X = register value
		rts
@bytes	fcb		$FF,$FF,0,1,0,0,0,1,0,0				;A,B,D,DP,X,Y,U,CC,PC,S
;
@setr	bsr		@ourr
		exg		d,x									;X = our reg index, D = new value
		jmp		reg_set_value						;set new value
;
@ourr	andb	#$0F								;B = 0000rrrr
		pshs	x
		leax	@regs,pcr
		ldb		b,x
		puls	x
		clra										;D = our register index
		rts
@regs	fcb		REG_D,REG_X,REG_Y,REG_U,REG_S,REG_PC,0,0,REG_A,REG_B,REG_CC,REG_DP,0,0,0,0

; ==> TOP_RGL: register list (PSH/PUL)
top_do_rgl:
		ldb		,y+									;B = register list
		bpl		top_do_exe_copy2					;execute if PC not included
;
		ldx		#trace_instr_buf					;copy instr to buffer
		ldd		-2,y
		andb	#$7F								;w/o PC
		std		,x++								;Y = after src instr, X = after buffer inst
;
		ldu		#cpu_reg_s							;U = ptr to S reg value
		bita	#%00000010							;A = opcode
		beq		@do									;if it is S
		leau	cpu_reg_u-cpu_reg_s,u				;else it's reg U
;
@do		lsra
		bcc		@psh								;if it's PSHS/PSHU
;
		pshs	u									;else it's PULS/PULU
		jsr		top_do_exe_inst						;PUL the rest (passed X, Y)
		puls	u
		ldx		,u									;X = stack reg value
		ldd		,x++								;pull new PC value
		std		<cpu_reg_pc
		stx		,u									;update stack reg
@done	jmp		top_ok_not_call						;and we're done
;
@psh	pshs	x
		ldx		,u									;X = stack reg value
		sty		,--x								;push new PC first
		stx		,u									;update stack register
		puls	x
		jmp		top_do_exe_inst						;push the rest

; ==> TOP_SWI: SWI
top_do_swi:
		bsr		@save								;save machine state on S
		lda		<cpu_reg_cc							;set F,I flags
		ora		#CC_F+CC_I
		sta		<cpu_reg_cc
		ldx		#SWI_VECTOR							;X = addr of vector
;
@setpc	ldd		,x									;D = vector
		std		<cpu_reg_pc
		andcc	#~CC_C								;return CC
		orcc	#CC_Z								;and EQ for call
		rts
;
; ==> TOP_SWN: SWI2, SWI3
top_do_swn:
		bsr		@save								;save machine state on S
		ldx		#SWI2_VECTOR						;assume it's SWI2
		lda		-2,y								;A = prefix byte
		cmpa	#$10
		beq		@setpc								;if it is SWI2
		ldx		#SWI3_VECTOR						;else must be SWI3
		bra		@setpc
;
@save	lda		<cpu_reg_cc							;set E flag
		ora		#CC_E
		sta		<cpu_reg_cc
;
		ldu		<cpu_reg_s							;push return PC
		sty		,--u
		ldd		<cpu_reg_u							;push U
		std		,--u
;
		ldx		#cpu_reg_y+2						;push the rest
@savelp	ldd		,--x
		std		,--u
		cmpx	#cpu_reg_cc
		bhi		@savelp
;
		stu		<cpu_reg_s							;update s
		rts

; ==> TOP_JPX: jump indexed
top_do_jpx:
		bsr		top_get_idx_addr					;Y = effective addr
		jmp		top_do_jump
;
; ==> TOP_JSX: jump to sub indexed
top_do_jsx:
		bsr		top_get_idx_addr					;Y = effective addr, D = addr after inst
		jmp		top_do_call
;
; Get effective address for a source index mode
; PASSED:  Y = address of source index byte and possible arg
; RETURNS: Y = effective address for the mode, D = addr after index bytes
top_get_idx_addr:
		ldx		#trace_instr_buf					;X = trace buffer
		lda		#$30								;LEAX opcode
		sta		,x+
		jsr		top_append_idx						;append index mode bytes
;
		pshs	y									;save addr after source instr
		ldy		<cpu_reg_x							;and cpu state that LEAX will bash
		lda		<cpu_reg_cc
		pshs	a,y
;
		jsr		top_do_exe_buf						;execute the LEAX instr
;
		puls	a,x									;restore CPU state
		sta		<cpu_reg_cc
		ldy		<cpu_reg_x							;Y = effective address
		stx		<cpu_reg_x
		puls	d									;D = addr after source instr
		rts

; ==> Unprefixed opcode trace operation table
top_do_unprefixed:
		TOP		EX2,UND,UND,EX2,EX2,UND,EX2,EX2,EX2,EX2,EX2,UND,EX2,EX2,JPD,EX2		;$00-$0F
		TOP		P10,P11,EX1,EX1,UND,UND,RWU,RCW,UND,EX1,EX2,UND,EX2,EX1,RGP,RGP		;$10-$1F
		TOP		RBU,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC,RBC		;$20-$2F
		TOP		IDX,IDX,IDX,IDX,RGL,RGL,RGL,RGL,UND,RTS,EX1,RTI,EX2,EX1,UND,SWI		;$30-$3F
		TOP		EX1,UND,UND,EX1,EX1,UND,EX1,EX1,EX1,EX1,EX1,UND,EX1,EX1,UND,EX1		;$40-$4F
		TOP		EX1,UND,UND,EX1,EX1,UND,EX1,EX1,EX1,EX1,EX1,UND,EX1,EX1,UND,EX1		;$50-$5F
		TOP		IDX,UND,UND,IDX,IDX,UND,IDX,IDX,IDX,IDX,IDX,UND,IDX,IDX,JPX,IDX		;$60-$6F
		TOP		EX3,UND,UND,EX3,EX3,UND,EX3,EX3,EX3,EX3,EX3,UND,EX3,EX3,JPE,EX3		;$70-$7F
		TOP		EX2,EX2,EX2,EX3,EX2,EX2,EX2,UND,EX2,EX2,EX2,EX2,EX3,RCB,EX3,UND		;$80-$8F
		TOP		EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,JSD,EX2,EX2		;$90-$9F
		TOP		IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,JSX,IDX,IDX		;$A0-$AF
		TOP		EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,JSE,EX3,EX3		;$B0-$BF
		TOP		EX2,EX2,EX2,EX3,EX2,EX2,EX2,UND,EX2,EX2,EX2,EX2,EX3,UND,EX3,UND		;$C0-$CF
		TOP		EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2,EX2		;$D0-$DF
		TOP		IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX,IDX		;$E0-$EF
		TOP		EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3,EX3		;$F0-$FF

; ==> $10 prefixed opcodes
TOP_0_PREFIXED_1ST	equ	$20
top_do_0_prefixed:
		TOP		UND,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC,RWC		;$20-$2F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,SWN		;$30-$3F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$40-$4F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$50-$5F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$60-$6F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$70-$7F
		TOP		UND,UND,UND,EX4,UND,UND,UND,UND,UND,UND,UND,UND,EX4,UND,EX4,UND		;$80-$8F
		TOP		UND,UND,UND,EX3,UND,UND,UND,UND,UND,UND,UND,UND,EX3,UND,EX3,EX3		;$90-$9F
		TOP		UND,UND,UND,IDX,UND,UND,UND,UND,UND,UND,UND,UND,IDX,UND,IDX,IDX		;$A0-$AF
		TOP		UND,UND,UND,EX4,UND,UND,UND,UND,UND,UND,UND,UND,EX4,UND,EX4,EX4		;$B0-$BF
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,EX4,UND		;$C0-$CF
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,EX3,EX3		;$D0-$DF
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,IDX,IDX		;$E0-$EF
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,EX4,EX4		;$F0-$FF

; ==> $11 prefixed opcodes
TOP_1_PREFIXED_1ST	equ	$30
TOP_1_PREFIXED_LAST	equ	$BF
top_do_1_prefixed:
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,SWN		;$30-$3F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$40-$4F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$50-$5F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$60-$6F
		TOP		UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$70-$7F
		TOP		UND,UND,UND,EX4,UND,UND,UND,UND,UND,UND,UND,UND,EX4,UND,UND,UND		;$80-$8F
		TOP		UND,UND,UND,EX3,UND,UND,UND,UND,UND,UND,UND,UND,EX3,UND,UND,UND		;$90-$9F
		TOP		UND,UND,UND,IDX,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND,UND		;$A0-$AF
		TOP		UND,UND,UND,EX4,UND,UND,UND,UND,UND,UND,UND,UND,EX4,UND,UND,UND		;$B0-$BF
