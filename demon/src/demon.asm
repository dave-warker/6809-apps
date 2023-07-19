; "demon.asm" - Dave's Debug Monitor for the 6809
; Based on my Z80 versions.
; 2022-aug-22 dww  Created by Dave Warker

		pragma		6809,cescapes,m80ext

		include		"g6cc.lib"
		include		"ascii.lib"

; @@@
; @@@ Constants
; @@@

DEMON_VER		equ		2		;major version
DEMON_REV		equ		4		;minor version
INT_SAFE_REST	equ		0		;1 = interrupt safe CPU state restore, 0 = not interrupt safe
TRACE_POLLING	equ		0		;1 = trace watches for ESC/SPACE, 0 = doesn't

CODE_MAX_SIZE	equ		$2000	;code space
DATA_MAX_SIZE	equ		256		;RAM space

	IFNDEF DEBUG
CODE_ORG		equ		G6CC_ROM_ORG+G6CC_ROM_SIZE-CODE_MAX_SIZE	;EPROM build
DATA_ORG		equ		G6CC_RAM_ORG+G6CC_RAM_SIZE-DATA_MAX_SIZE
	ELSE
CODE_ORG		equ		$1000										;RAM build
DATA_ORG		equ		CODE_ORG-DATA_MAX_SIZE
	ENDC

OUR_DATA_DP		equ		DATA_ORG/256								;our direct page register
DEF_SYS_STACK	equ		DATA_ORG									;default system stack pointer for apps
S_STACK_SIZE	equ		64											;size of our system stack
LN_BUF_SIZE		equ		64											;size of line input buffer incl NUL

EOS				equ		NUL											;end of string marker
EOC				equ		SEMIC										;';' command separator
ANY				equ		UND											;'_' match any in Query Mem

MAX_MNE_CHARS	equ		5											;longest mnemonic
MAX_INSTR_BYTES	equ		5											;longest instruction
BKPT_OP			equ		$3F											;breakpoint instruction (SWI)
DISP_MEM_BPL	equ		16											;display mem bytes/line
DISP_MNE_WIDTH	equ		7											;Width of mnemonic field
WRITE_BPR_MAX	equ		32											;Write maxc bytes/record

SWI_VECTOR		equ		$FFFA										;SWI vector address
SWI2_VECTOR		equ		$FFF4										; SWI2
SWI3_VECTOR		equ		$FFF2										; SWI3

MAX_SYM_LENGTH	equ		8											;longest possible symbol
ALF_SYM_VALUE	equ		$C000										;where we should find ALF
DEMON_SYM_VALUE	equ		CODE_ORG									;where we will find this copy of Demon
SYL_SYM_VALUE	equ		$D000										;where we should find SYL

; @@@
; @@@ Storage
; @@@

		org			DATA_ORG

warm_flag		rmb		1											;helps detect warm boot
WARM_FLAG_VALUE	equ		$DA											;value when we've warm booted

ln_buf			rmb		LN_BUF_SIZE									;line buffer
ln_next			rmb		2											;line buffer next char ptr
ln_err_indent	rmb		1											;indent for error indicator
ln_err_handler	rmb		2											;where to go after error
ln_err_stack	rmb		2											;stack pointer to restore

cpu_state		equ		*											;processor state
; { IN PSHU/PULU STACK ORDER:
cpu_reg_cc		rmb		1											; Condition Codes
cpu_reg_a		rmb		1											; A
cpu_reg_b		rmb		1											; B
cpu_reg_d		equ		cpu_reg_a									; D = A + B
cpu_reg_dp		rmb		1											; Direct Page
cpu_reg_x		rmb		2											; X
cpu_reg_y		rmb		2											; Y
cpu_reg_s		rmb		2											; S
; }
cpu_reg_u		rmb		2											; U
cpu_reg_pc		rmb		2											; PC
CPU_STATE_SIZE	equ		*-cpu_state
;
REG_A			equ		0											;Register indicies
REG_B			equ		1
REG_D			equ		2
REG_DP			equ		3
REG_X			equ		4
REG_Y			equ		5
REG_U			equ		6
REG_CC			equ		7
REG_PC			equ		8
REG_S			equ		9
N_REGS			equ		10
;
REG_INFO_WORD	equ		$80											;register is 2 bytes
REG_INFO_CC		equ		$40											;register is condition codes
REG_INFO_OFF_M	equ		$3F											;mask for offset into cpu_state
;
CC_C			equ		$01											;Carry flag in condition code register
CC_V			equ		$02											;oVerflow flag
CC_Z			equ		$04											;Zero flag
CC_N			equ		$08											;Negative flag
CC_I			equ		$10											;Interrupt flag
CC_H			equ		$20											;Half carry flag
CC_F			equ		$40											;Fast interrupt flag
CC_E			equ		$80											;Entire register set flag

bkpt_addr		rmb		2											;address where it's set
BKPT_ADDR_NONE	equ		$FFFF										; address when no breakpoint set
bkpt_save_byte	rmb		1											;original byte at breakpoint address
;
asm_addr		rmb		2											;address to assemble at
;
trace_instr_buf	rmb		MAX_INSTR_BYTES+3							;instruction being traced + JMP
trace_sp		rmb		2											;save for our stack
;
our_s_stack		rmb		S_STACK_SIZE								;our system stack
our_s_stack_top	equ		*

DATA_END		equ		*

; @@@
; @@@ Code
; @@@

		org			CODE_ORG

demon_reset:
;
; Reset initialization
;
		lds			#our_s_stack_top								;registers
		lda			#OUR_DATA_DP
		tfr			a,dp
		setdp		OUR_DATA_DP
;
		lda			#WARM_FLAG_VALUE								;warm start?
		cmpa		<warm_flag
		bne			@clear											;nope
		jsr			cpu_clear_bkpt									;try to clear leftover breakpoint
;
@clear	ldx			#DATA_ORG										;initialize data area
@clrlp	clr			,x+
		cmpx		#DATA_END
		blo			@clrlp
;
		lda			#WARM_FLAG_VALUE								;warm start
		sta			<warm_flag
		ldx			#DEF_SYS_STACK									;ensure valid stack
		stx			<cpu_reg_s
		ldx			#BKPT_ADDR_NONE									;no breakpoint set
		stx			<bkpt_addr
;
		jsr			io_init
		jsr			msg
	IFDEF DEBUG
		fcc			"DEBUG "
	ENDC
		fcc			"Demon 6809 v",DEMON_VER+'0','.',DEMON_REV+'0',CR,LF,EOS
;
; Command processing loop
;
rep_lp:
		lda			#2												;prompt length
		ldx			#rep_lp											;handler address
		jsr			ln_set_err_handler
;
		lds			#our_s_stack_top
		jsr			msg
	IFNDEF DEBUG
		fcn			"> "
	ELSE
		fcn			"$ "
	ENDC
		jsr			ln_fill_buf										;get a line of input
		jsr			nl
;
cmd_lp:
		jsr			ln_get_ch										;get command char
		bcs			rep_lp											;if end of line
		cmpa		#$20
		bls			cmd_lp											;ignore spaces and ctl chars
		cmpa		#EOC
		beq			cmd_lp											;and command seps
;
		jsr			to_upper_ch										;dispatch to command
		suba		#'A'												;convert to table index
		blo			cmd_err											;if not in valid range
		cmpa		#'Z'-'A'
		bhi			cmd_err											;ditto
		asla
		bsr			@go
		bra			cmd_lp											;look for next
;
@go		ldx			#cmd_tbl
		jmp			[a,x]
;
cmd_err	jmp			ln_err

; ==> Expect end of command else error
ln_expect_eoc:
		jsr			ln_at_eoc
		bne			cmd_err											;if not end of command
		rts

; ==> Check for command pause (SPACE) or abort (ESC)
cmd_pause:
		jsr			io_has_ch
		beq			@done											;if no key pressed
		jsr			io_get_ch
;
		cmpa		#ESC
		beq			rep_lp											;if wants to cancel
;
		cmpa		#XOFF											;XOFF pauses
		beq			@pause
		cmpa		#SPACE
		bne			cmd_pause										;if not SPACE to pause
;
@pause	jsr			io_get_ch										;wait for a key
		cmpa		#ESC
		beq			rep_lp											;if wants to cancel now
@done	rts

; @@@
; @@@ Etc
; @@@

		include		"expr.asm"
		include		"ln.asm"
		include		"util.asm"
		include		"io.asm"
		include		"cpu.asm"
		include		"cmds.asm"
		include		"inst.asm"
		include		"trace.asm"

; @@@
; @@@ Vectors
; @@@

		org			CODE_ORG+CODE_MAX_SIZE-$0E
		fdb			$0000											;$FFF2 - SWI3
		fdb			$0000											;$FFF4 - SWI2
		fdb			$0000											;$FFF6 - FIRQ
		fdb			$0000											;$FFF8 - IRQ
		fdb			swi_entry_vector								;$FFFA - SWI - breakpoint
		fdb			$0000											;$FFFC - NMI
		fdb			demon_reset										;$FFFE - RES - reset entry

; @@@

	END
