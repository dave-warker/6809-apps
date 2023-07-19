; "strs.asm" - string words for Alf
; 2022-nov-25 dww  created

; >>> Maximum string capacity constant
			DEF		4,"SCAP"
MAXSTR		CONST
			fdb		STR_MAX_LEN

; >>> [VAR] Temp string buffer
			DEF		2,"s$"
str$		VAR
			fdb		str$buf+1						;push address of current length

; >>> Allocate max length string
			DEF		4,"STR:"
			WORDS
			fdb		MAXSTR
			fdb		def$chars
			DONE

; >>> Allocate string of specific max length
; <len> chars: <name>
			DEF		6,"CHARS:"
def$chars	WORDS
			fdb		MAXSTR,umin						;limit length to max allowed
			fdb		def$bgn
			fdb		complw,OP_JSR_DP+chars$%256		;compile JSR <chars$
			fdb		dup,compb,push0,compb			;compile max length, cur length (0)
			fdb		alloc$							;allocate space for string
			fdb		def$end							;finish up definition
			DONE

; >>> Compile a string
; CONSUMES: 0,PP = address of the string
comps		puls	x								;X = source string addr
			pshs	y								;preserve IP
			ldy		dp$								;Y = dest addr (assume there's enough space)
			clr		,y+								;compile max length of 0
			ldb		,x								;B = count
			incb									;including length
;
@lp			lda		,x+								;copy a byte
			sta		,y+
			decb
			bne		@lp
;
			sty		dp$								;update DP
			jsr		<mem$check						;check for memory full
			puls	y								;restore IP
			NEXT

; >>> Push literal string at current IP
; PRODUCES: 0,PP = addr of string, IP = addr after it
pushls$		leay	1,y								;Y = address of cur length
			pshs	y								;push it
			ldb		,y+								;advance IP over it
			clra
			leay	d,y
			NEXT

; >>> Push max capacity of string
; CONSUMES: 0,PP = address of string cur length
; PRODUCES: 0,PP = max capacity of string (byte @ addr - 1)
			DEF		4,"scap"
scap		ldx		,s								;X = addr of string cur len
			ldb		-1,x							;B = max capacity
@pushb		clra
			std		,s								;return B
			NEXT
;
; >> Push current length of string
; CONSUMES: 0,PP = address of string cur len
; PRODUCES: 0,PP = string's current length
			DEF		4,"slen"
slen		ldx		,s								;X = addr of strung cur len
			ldb		,x								;B = cur length
			bra		@pushb

; >>> Push address of first data char of string
; CONSUMES: 0,PP = address of string
; PRODUCES: 0,PP = address of first data char of string
			DEF		4,"sdat"
sdat		ldx		,s								;X = addr of cur length
			leax	1,x								;X = addr of first char
			stx		,s
			NEXT

; >>> Push char at string index (or -1 if out of bounds)
; <str> <off> c@ --> <char> or -1
			DEF		2,"c@"
			WORDS
			fdb		push1,ss						;sanitize offset
			fdb		@cat
			DONE
;
@cat		ldd		,s++							;D = 0 if NG else 1
			beq		@fail							;if should return -1
			puls	d,x								;X = string, D = index
			leax	1,x								;skip length
			ldb		d,x
			clra									;D = char
			pshs	d
			NEXT
;
@fail		leas	2,s								;clean up stack
			ldd		#-1								;error result
			std		,s
			NEXT

; >>> Set char at index (ignored if invalid index)
; <char> <str> <off> c=
			DEF		2,"c="
			puls	d,x								;D = off, X = string
;
			tsta
			bpl		@val							;if not relative to end
			pshs	d
			ldb		,x
			clra									;D = cur length
			addd	,s++							;D = offset rel to start
;
@val		tsta
			bne		@fail							;if def not a valid index
			cmpb	-1,x
			bhs		@fail							;if not within capacity
			cmpb	,x+
			blo		@set							;if is in current length
			bne		@fail							;if not appending to end
			inc		-1,x							;else adding char to end
;
@set		leax	d,x								;X = addr for store
			puls	d								;B = char
			stb		,x
			NEXT
;
@fail		leas	2,s								;drop char
			NEXT

; >>> Sanitize sub-string spec
; <str> <off> <len> ss -> <str> <off> <len>
; Pins sub-string within actual contents.
; Accepts negative offset as relative to end.
			DEF		2,"ss"
ss			ldx		4,s								;X = string cur len address
			clra
			ldb		,x
			pshs	d
;
; 0,s = cur length, 2,s = req length, 4,s = req offset, 6,s = str addr
;
			ldd		4,s								;D = requested offset
			bpl		@offl							;if not relative to end
			addd	,s								;else convert to actual offset
;
; Pin offset to 0 <= off < cur len, adjust req len as needed
;
@offl		tsta									;ensure off >= 0
			bpl		@offh
			addd	2,s								;adjust req length
			std		2,s
			clra									;and use offset of 0
			clrb
@offh		cmpd	,s								;ensure off < cur len
			blo		@uoff
			ldd		,s								;force to end of string
			clr		2,s								;and set length to 0
			clr		3,s
@uoff		std		4,s								;update offset to actual
;
; Pin length to 0 <= len <= cur len - off
;
			ldd		2,s								;D = requested length
			bpl		@lenh							;if not negative
			clra
			clrb									;else set length to 0
@lenh		pshs	d
			ldd		2,s								;D = cur length
			subd	6,s								;D = avail length
			cmpd	,s								;req len <= avail?
			bhs		@ulen							;yep
			std		,s								;else adjust req len
@ulen		puls	d								;D = actual len
			std		2,s								;update req len to actual
			leas	2,s								;drop cur len
;
			NEXT

; >>> Copy a string
; CONSUMES: 2,PP = source string, 0,PP = destination string
			DEF		4,"scpy"
			WORDS
			fdb		rpush							;save dest string
			fdb		dup,slen,push0,swap
			fdb		rpop							;<src> 0 <len> <dest>
			fdb		jp$,sscpy$						;rest same as substring copy

; >>> Append a string
; CONSUMES: 2,PP = source string, 0,PP = destination string
			DEF		4,"scat"
			WORDS
			fdb		rpush							;save dest string
			fdb		dup,slen,push0,swap
			fdb		rpop							;<src> 0 <len> <dest>
			fdb		jp$,sscat$						;rest same as substring append

; >>> Copy a substring
; <src> <off> <len> <dest> sscpy
			DEF		5,"sscpy"
			WORDS
sscpy$		fdb		push0,over,bstore				;empty dest string
			fdb		jp$,sscat$						;rest same as substring append

; >>> Append a substring
; <src> <off> <len> <dest> sscat
; This is the core string copy/append function
			DEF		5,"sscat"
sscat		WORDS
sscat$		fdb		rpush,ss,rpop					;sanitize source substring
			fdb		@sscat$							;rest is machine code
			DONE
;
@sscat$		pshu	y								;save IP
			puls	y								;Y = destination string
;
; 0,s = req len, 2,s = req off, 4,s = source str, Y = dest string
; Assumes substring off, len have already been sanitized.
;
			ldb		-1,y							;B = dest capacity
			subb	,y								; - cur dest len
			bcc		@avail							;if normal string
			clrb									;else r/o string, force avail to 0
@avail		clra									;D = available space
;
			cmpd	,s								;pin length to available
			bhs		@mty?							;if req len is OK
			std		,s								;else trim req len to avail
@mty?		tst		1,s								;len must be <= 255 here
			beq		@done							;if nothing to do
;
			ldb		,y								;B = cur dest length
			pshs	b
			addb	2,s								;update to final len
			stb		,y+
			puls	b
			leay	d,y								;Y = dest addr
;
			ldx		4,s								;X = source string
			ldd		2,s
			leax	d,x
			leax	1,x								;X = source addr
			ldb		1,s								;B = len to copy (1 - 255)
;
@cpylp		lda		,x+								;copy a byte
			sta		,y+
			decb
			bne		@cpylp							;loop til done
;
@done		leas	6,s								;clean parms off stack
			pulu	y								;restore IP
			NEXT

; >>> Formatted print.
; <args> <fmtstr> prf
; In format string:
;  %d = signed decimal, %u = unsigned decimal, %x = unsigned hex, %b = unsigned binary
;  %c = character, %s = string, %% = %, others ignored
; Args are taken off stack in REVERSE order (first printed is last pushed)
			DEF		3,"prf"
prfmt		pshu	y								;save IP
			puls	y								;Y = format string
			lda		,y+								;A = char count
;
@lp			tsta
			beq		@done							;if end of string
			deca
			ldb		,y+
			cmpb	#'%'
			beq		@fmt							;if is format spec
			pshs	a								;else display char
			jsr		prc_
			puls	a
			bra		@lp
;
@fmt		tsta
			beq		@done							;if hit end of string
			deca
			ldb		,y+								;B = format char
			pshu	a,y								;save count, string ptr
;
			cmpb	#'d'
			beq		@sdec							;if signed decimal
			cmpb	#'u'
			beq		@udec							;if unsigned decimal
			cmpb	#'x'
			beq		@uhex							;if unsigned hex
			cmpb	#'b'
			beq		@ubin							;if unsigned binary
			cmpb	#'c'
			beq		@char							;if character
			cmpb	#'s'
			beq		@str							;if string
			cmpb	#'%'
			beq		@prc							;if escaped percent
;
@efmt		pulu	a,y								;restore count, ptr
			bra		@lp
;
@sdec		puls	d								;signed decimal
			jsr		prsign_							;display sign if necessary
@prd		ldx		#10								;display in decimal
@prn		jsr		prnum_
			bra		@efmt
;
@udec		puls	d								;display unsigned decimal
			bra		@prd
;
@uhex		ldx		#16								;display unsigned hex
@pru		puls	d
			bra		@prn
;
@ubin		ldx		#2								;display unsigned binary
			bra		@pru
;
@char		puls	d								;display character
@prc		jsr		prc_
			bra		@efmt
;
@str		puls	x								;display string
			jsr		prs_
			bra		@efmt
;
@done		pulu	y								;restore IP
			NEXT

; >>> Convert char to upper case
; <char> cupper
			DEF		6,"cupper"
cupper		ldd		,s								;B = char
			bsr		cupper_							;convert to upper case
			std		,s
			NEXT
;
cupper_		cmpb	#'a'
			blo		@ret							;if not lower case
			cmpb	#'z'
			bhi		@ret							;ditto
			eorb	#$20							;convert to upper
@ret		rts
;
; >>> Compare strings (case significant)
; <s1> <s2> scmp -> -1 | 0 | 1
; -1 = <s1> < <s2>, 0 = <s1> == <s2>, 1 = <s1> > <s2>
			DEF		4,"scmp"
			ldd		#@ret							;D = do nothing function
			bra		@scmp
;
; >>> Compare strings ignoring case
; <s1> <s2> scmp -> -1 | 0 | 1
; -1 = <s1> < <s2>, 0 = <s1> == <s2>, 1 = <s1> > <s2>
			DEF		5,"scmpi"
			ldd		#cupper_						;D = convert char to upper case
;
@scmp		pshu	y								;save IP
			puls	x,y								;Y = <s1>, X = <s2>
			pshs	d								;0,s = normalize char function
;
			lda		,y+								;compare lengths
			cmpa	,x+
			blo		@lt								;if <s1> length < <s2> length
			bhi		@gt								;if <s1> lrngth > <s2> length
			tsta
			beq		@eq								;if both empty
;
@scmplp		ldb		,x+								;B = next <s2> char
			jsr		[,s]							;normalize it
			pshs	b
			ldb		,y+								;B = next <s1> char
			jsr		[1,s]							;normalize it
			cmpb	,s+
			blo		@lt								;if <s1> < <s2>
			bhi		@gt								;if <s1> > <s2>
			deca
			bne		@scmplp							;if not done yet
;
@eq			clrb									;equal, return 0
			bra		@res
@lt			ldb		#-1								;less, return -1
			bra		@res
@gt			ldb		#1								;greater, return 1
@res		sex
			std		,s								;replace fn with result
			pulu	y								;restore IP
			NEXT
