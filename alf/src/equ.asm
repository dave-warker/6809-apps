; "equ.asm" - equates for Alf
; 2022-nov-18 dww  created

;@@@
;@@@ CONSTANTS
;@@@

ALF_VER			equ		1							;major version
ALF_REV			equ		6							;minor version
ALF_WARM_ID		equ		$DA5E						;Alf's warm boot flag
;
	IFDEF DEBUG
ALF_RO_ORG		equ		$5000						;read/only area
ALF_RO_SIZE		equ		$2000
ALF_RW_ORG		equ		$0000						;read/write area
ALF_RW_SIZE		equ		$4FFF
	ELSE
ALF_RO_ORG		equ		$C000						;read/only area
ALF_RO_SIZE		equ		$2000
ALF_RW_ORG		equ		$0000						;read/write area
ALF_RW_SIZE		equ		$7F00
	ENDIF
;
ALF_KER_ORG		equ		ALF_RO_ORG					;start of kernel
ALF_DP_ORG		equ		ALF_KER_ORG					;start of DP accessible code
ALF_SKV_ORG		equ		ALF_RW_ORG					;start of static kernel vars
;
ALF_RS_SIZE		equ		$0100						;size of the return stack
ALF_RS_TOP		equ		ALF_RW_ORG+ALF_RW_SIZE		;top/initial return stack pointer
ALF_RS_BOT		equ		ALF_RS_TOP-ALF_RS_SIZE		;bottom/origin of return stack
;
ALF_PS_TOP		equ		ALF_RS_BOT					;top/initial param stack pointer
;
STR_MAX_LEN		equ		255							;longest string allowed
RBC_BUF_LEN		equ		127							;console input buffer size
RBC_BUF_EMPTY	equ		$FF							;console input buffer empty flag

NL				equ		LF							;newline character
CC_C			equ		$01							;6809 Condition Code Carry flag bit
OP_JSR_DP		equ		$9D00						;opcode word for JSR <addr
OP_NEXT			equ		$6EB1						;opcode word for NEXT: JMP [,Y++]

;@@@
;@@@ MACROS:
;@@@

_head		set		0								;head of the nucleus' word list
DEF			macro noexpand							;define a visible word's header
@me			set		*
			fcb		\1								;name length in bytes (MSB set = always execute)
			fcc		\2								;name
			fdb		_head
_head		set		@me
			endm
_EXE		equ		$80								;add to length to force execution

NEXT		macro noexpand							;advance to next word
			jmp		[,y++]
			endm

WORDS		macro noexpand							;start a new word list
			jsr		<words$
			endm

DONE		macro noexpand							;done with current word list
			fdb		done$
			endm

CONST		macro noexpand							;push constant value
			jsr		<const$
			endm

VAR			macro noexpand							;push variable address (same as const)
			jsr		<var$
			endm

VAL			macro noexpand							;push variable value
			jsr		<val$
			endm
