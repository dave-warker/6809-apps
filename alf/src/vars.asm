; "vars.asm" - variables for Alf
; 2022-dec-22 dww created.

; >>> Define a word that pushes a constant (16 bit) value
; <expr> val: <name>
			DEF		4,"VAL:"
			WORDS
			fdb		def$bgn							;compile word header
			fdb		complw,OP_JSR_DP+const$%256		;compile JSR <const$
			fdb		compw							;compile value from stack
			fdb		def$end							;finalize it
			DONE

; Define a variable that can hold a 16 bit word (and pushes it's address)
; var: <name>
			DEF		4,"VAR:"
			WORDS
			fdb		def$bgn							;compile word header
			fdb		complw,OP_JSR_DP+next$%256		;compile JSR <next$ (pushes addr)
			fdb		push0,compw						;compile 0 (initial value)
			fdb		def$end							;finalize it
			DONE

; Define a variable that can hold an 8 bit byte (and pushes it's address)
; var: <name>
			DEF		5,"BYTE:"
			WORDS
			fdb		def$bgn							;compile word header
			fdb		complw,OP_JSR_DP+next$%256		;compile JSR <next$ (pushes addr)
			fdb		push0,compb						;compile 0 (initial value)
			fdb		def$end							;finalize it
			DONE

; Define a variable that can hold N bytes (and pushes addr of start)
; <count> bytes: <name>
			DEF		6,"BYTES:"
			WORDS
			fdb		def$bgn							;compile word header
			fdb		complw,OP_JSR_DP+next$%256		;compile JSR <next$ (pushes addr)
			fdb		alloc$							;allocate space
			fdb		def$end							;finalize it
			DONE
