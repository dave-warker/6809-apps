; "cpu.asm" - Processor related code for Demon
; 2022-sep-09 dww  created.

; @@@
; @@@ CPU State
; @@@

; ==> Software Interrupt (breakpoint) entry.
; Save all CPU state then jump back into command processing loop.
; NOTE: the STU instruction ACTUALLY UPDATES CC FLAGS so we have to use the copy ion the stack.
swi_entry_vector:
		stu		>cpu_reg_u										;save some registers
		leau	cpu_reg_y+2,pcr
		pshu	y,x,dp,d
		lda		#OUR_DATA_DP									;restore our DP
		tfr		a,dp
		lda		,s												;save CC
		sta		<cpu_reg_cc
		leas	10,s											;drop SWI reg saves
		puls	x
		sts		<cpu_reg_s										;save S
		leax	-1,x
		stx		<cpu_reg_pc										;save PC
		lds		#our_s_stack_top								;restore our S
;
		bsr		cpu_clear_bkpt
		jmp		bkpt_reentry									;continue breakpoint processing

; ==> Try to remove existing breakpoint
; Attempts to ensure it's a legit breakpoint before mucking with memory.
cpu_clear_bkpt:
		ldx		<bkpt_addr
		cmpx	#BKPT_ADDR_NONE
		beq		@ret											;if no breakpoint set
		lda		,x
		cmpa	#BKPT_OP
		bne		@nope											;oops, op is wrong, don't try to restore
		lda		<bkpt_save_byte									;restore original byte
		sta		,x
@nope	ldd		#BKPT_ADDR_NONE									;clear breakpoint flag
		std		<bkpt_addr
@ret	rts

; ==> Restore CPU state and continue execution.
restore_cpu_state:
		ldx		<bkpt_addr
		cmpx	#BKPT_ADDR_NONE
		beq		@res											;if no breakpoint set
		lda		0,x												;save current byte
		sta		<bkpt_save_byte
		lda		#BKPT_OP										;set breakpoint instruction (SWI)
		sta		0,x
;
@res
 IF INT_SAFE_REST	;interrupt safe CPU state restore
		lds		<cpu_reg_s
		ldx		<cpu_reg_pc
		lda		<cpu_reg_cc
		anda	#~CC_E
		pshs	a,x												;CC,PC restored by RTI
		leau	cpu_reg_a,pcr									;restore other registers
		pulu	d,dp,x,y
		ldu		>cpu_reg_u
		rti														;and off we go
 ELSE				;non-interrupt safe (but smaller/faster) version
		leau	cpu_reg_cc,pcr
		pulu	cc,d,dp,x,y,s									;(could get interrupt after this)
		leau	[cpu_reg_u,pcr]									;(doesn't alter flags)
		jmp		[cpu_reg_pc]
 ENDIF

; @@@
; @@@ Registers
; @@@

; ==> Tables of valid register names (in register index order)
reg_names_1st:
		fcb		'A'												;REG_A
		fcb		'B'												;REG_B
		fcb		'D'												;REG_D
		fcb		'D'												;REG_DP
		fcb		'X'												;REG_X
		fcb		'Y'												;REG_Y
		fcb		'U'												;REG_U
		fcb		'C'												;REG_CC
		fcb		'P'												;REG_PC
		fcb		'S'												;REG_S
;
reg_names_2nd:
		fcb		0												;REG_A
		fcb		0												;REG_B
		fcb		0												;REG_D
		fcb		'P'												;REG_DP
		fcb		0												;REG_X
		fcb		0												;REG_Y
		fcb		0												;REG_U
		fcb		'C'												;REG_CC
		fcb		'C'												;REG_PC
		fcb		0												;REG_S

; ==> Table of info about each register
reg_infos:
		fcb		(cpu_reg_a-cpu_state)							;REG_A
		fcb		(cpu_reg_b-cpu_state)							;REG_B
		fcb		(cpu_reg_d-cpu_state)+REG_INFO_WORD				;REG_D
		fcb		(cpu_reg_dp-cpu_state)							;REG_DP
		fcb		(cpu_reg_x-cpu_state)+REG_INFO_WORD				;REG_X
		fcb		(cpu_reg_y-cpu_state)+REG_INFO_WORD				;REG_Y
		fcb		(cpu_reg_u-cpu_state)+REG_INFO_WORD				;REG_U
		fcb		(cpu_reg_cc-cpu_state)+REG_INFO_CC				;REG_CC
		fcb		(cpu_reg_pc-cpu_state)+REG_INFO_WORD			;REG_PC
		fcb		(cpu_reg_s-cpu_state)+REG_INFO_WORD				;REG_S

; ==> Look for (1 or 2 character) register name in buffer.
; RETURNS: X = register index of (consumed) name else bails if no valid name found
ln_expect_reg_name:
		jsr		ln_peek_for_uc_alpha							;1st letter is required
		bcs		@err
		tfr		a,b
		jsr		ln_peek_for_uc_alpha							;2nd is optional
		bcc		no3@rd
		clra
no3@rd	pshs	a												;3rd is forbidden
		jsr		ln_peek_for_uc_alpha
		puls	a
		bcc		@err
;
		ldx		#0												;find it in the name table(s)
@lp		cmpb	reg_names_1st,x
		bne		@next											;if 1st letter doesn't match
		cmpa	reg_names_2nd,x
		beq		@done											;if 2nd matches then we're good
@next	leax	1,x
		cmpx	#N_REGS
		blo		@lp												;if not end of table
;
@err	jmp		cmd_err											;bail with error
;
@done	rts

; ==> Display register name
; PASSED:  X = (assumed valid) register index
; RETURNS: EQ if was a one letter name else NE if was 2
disp_reg_name:
		lda		reg_names_1st,x									;always display 1st char
		jsr		io_put_ch
		lda		reg_names_2nd,x
		beq		@done											;if no 2nd then return EQ
		jsr		io_put_ch										;else display 2nd
		andcc	#~CC_Z											;and return NE
@done	rts

; ==> Get register's current value
; PASSED:  X = (assumed valid) register index
; RETURNS: D = register value
reg_get_value:
		pshs	y
		bsr		reg_get_info									;A = info, Y = address in cpu_state
		ldb		,y+												;B = first byte
		anda	#REG_INFO_WORD
		beq		@done											;if it's a one byte value (A = 0)
		tfr		b,a												;else it's two bytes
		ldb		,y
@done	puls	y
		rts

; ==> Set register's current value
; PASSED: X = (assumed value register index), D = new register value
reg_set_value:
		pshs	d,y
		bsr		reg_get_info									;A = info, Y = address in cpu_state
		anda	#REG_INFO_WORD
		puls	d
		bne		@two											;if two byte value
;
		stb		0,y												;set one byte value
		bra		@done
;
@two	std		0,y												;set two byte value
;
@done	puls	y
		rts

; ==> get registerinfo from register index
; PASSED:  X = (assumed valid) register index
; RETURNS: A = register info, Y = address of register data in cpu_state
reg_get_info:
		lda		reg_infos,x										;A = info byte
		pshs	a
		anda	#REG_INFO_OFF_M									;A = offset into cpu_state
		ldy		#cpu_state
		leay	a,y												;Y = address of value
		puls	a
		rts
