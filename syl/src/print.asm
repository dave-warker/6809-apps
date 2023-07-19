; "print.asm" - Print an S-Expression for Syl
; 2023-feb-20 dww  created

; >>> Evaluate an S-Expression
; PASSED:  X = root pointer of the S-Expression
; RETURNS: X = root pointer of the result
print:
			jsr		ref$is$int
			beq		@int							;if it's an integer
			jsr		ref$is$atom
			beq		@atom							;if it's an ATOM
			jsr		ref$is$cons
			beq		@cons							;if it's a CONS cell
			jsr		ref$is$bis
			beq		@bifn							;if it's a built-in function
;
; UNKNOWN
;
			ldb		#'?'
@putc		jsr		con$putc
			tfr		x,d
			jmp		con$disp$hex$word
;
; INTEGER
;
@int		tfr		x,d
			bita	#$40
			bne		@disp$int						;negative number is OK as-is
			anda	#CCS_INT_MASK/256				;else clear MSB for positive
@disp$int	jmp		con$disp$int$word				;display as signed decimal
;
; ATOM
;
@atom		jmp		con$msg							;ref is name addr
;
; CONS
;
@cons		ldb		#'('							;open paren
			jsr		con$putc
;
@conslp		pshs	x								;display CAR
			ldx		CC_CAR_OFF,x
			bsr		print
			puls	x
;
			ldx		CC_CDR_OFF,x
			beq		@consend						;we're done if it's NIL
			jsr		ref$is$cons
			bne		@dotted							;if it's a dotted pair
;
			ldb		#' '							;else it's a list
			jsr		con$putc
			bra		@conslp
;
@dotted		ldb		#'.'							;display as dotted pair
			jsr		con$putc
			bsr		print
;
@consend	ldb		#')'							;close paren
			jmp		con$putc
;
; Built-in Function
;
@bifn		leax	bis$base-CCS_REF_BIV_FIRST,x	;X = actual address
			ldb		#'@'
			bra		@putc							;rest same as unknown
