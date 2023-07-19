; "env.asm" - Environment utilities for Syl
; 2023-feb-25 dww  created

; >>> Allocate an empty environment
; RETURNS: X = addr/ref to newly allocated, empty environment list
; NOTE: On return the environment will NOT be protected from future
; garbage collections so make sure you push it to the ROOT stack
; before the next cycle or it will be freed.
env$alloc:
			ldd		#ATOM_env
			jmp		cons$alloc$tag

; >>> Allocate an ATOM binding entry for an environment
; PASSED:  D = ATOM reference, X = value to bind to it
; RETURNS: X = ref to newly allocate entry
; NOTE: returned ref is NOT protected from future GCs.
env$alloc$binding:
			pshs	y,x,d
			jsr		cons$alloc							;allocate ATOM binding cell
			puls	d,y
			std		CC_CAR_OFF,x						;CAR is ATOM
			sty		CC_CDR_OFF,x						;CDR is bound value
			pshu	x									;protect it
			jsr		cons$alloc							;allocate env list entry
			pulu	d
			std		CC_CAR_OFF,x						;CAR points to binding
			puls	y
			rts

; >>> Add a default global binding
; PASSED:  Y = addr of binding (name str, bound value)
; RETURNS: Y = addr after the binding
; ASSUMES: there is unallocated space for the ATOM name
env$add$default$binding:
			ldx		<atom_unalloc						;X = new ATOM addr/ref
;
			pshs	x									;copy ATOM name to ATOM pool
			lda		,y
@namelp		ldb		,y+
			stb		,x+
			deca
			bpl		@namelp
			stx		<atom_unalloc
;
			puls	d									;D = ATOM addr/ref
			ldx		,y++								;X = bound value
			bsr		env$alloc$binding
			tfr		x,d									;D = ENV entry
			ldx		<env_globals						;X = global ENV
	; fall through into ...
; >>> Add an ATOM binding to an existing environment list
; PASSED:  X = environment head, D = existing ATOM binding
; ALTERS:  D
env$add$binding:
			pshs	y
			ldy		CC_CDR_OFF,x						;old start of env
			std		CC_CDR_OFF,x						;new start
			exg		d,y
			std		CC_CDR_OFF,y						;old start is new second
			puls	y
			rts

; >>> Define a binding in an ENV (replaces existing or creates new.)
; PASSED:  D = ATOM to define; X = it's value; Y = ENV to define it within
env$def:
			bsr		env$cur								;Y = current ENV
env$def$in:
;
; See if it's already defined
;
			pshs	y
			ldy		CC_CDR_OFF,y						;Y = first binding
			beq		@add								;if the ENV is empty
@lp			cmpd	[CC_CAR_OFF,y]
			beq		@set								;if it's already defined
			ldy		CC_CDR_OFF,y
			bne		@lp									;else try next
;
; Not defined, add new definition
;
@add		bsr		env$alloc$binding					;allocate a new binding
			tfr		x,d									;D = binding
			puls	x									;X = ENV head
			bra		env$add$binding						;add it
;
; Already exists, set it's value
;
@set		ldy		CC_CAR_OFF,y						;Y = binding CONS
			stx		CC_CDR_OFF,y						;store new value
			leas	2,s									;clean up the stack
			rts

; >>> Get the current environment (bottom-most on the U stack)
; RETURNS: Y = current ENV (assumed to always succeed because the global ENV always exists)
; ALTERS NO OTHER REGISTERS.
env$cur:
			pshs	d
			tfr		u,y									;Y = root pointers
			ldd		#ATOM_env							;D = tag we're looking for
@lp			cmpd	[,y++]
			bne		@lp									;keep going til we find one
			ldy		-2,y								;Y = current ENV
			puls	d
			rts

; >>> Get ATOM's bound value or bail if it's unbound
; Searches all of the environments on the Root Pointers stack from newest to oldest
; PASSED:  X = addr/ref to ATOM
; RETURNS: X = ref to CONS with binding (CDR is bound value)
; DOES NOT ALTER Y.
; Mucks with U reg but it's OK since there are no possible allocs/deallocs here.
env$get$atom$binding:
			pshs	u,y,x								;,S = target ATOM
;
@next$env	ldd		#ATOM_env							;D = tag for environments
@env$lp		cmpu	#ROOT_STACK_TOP-1
			bhi		@unbound							;if hit top of stack then it's unbound
			ldy		,u++								;Y = next root pointer
			cmpd	CC_CAR_OFF,y
			bne		@env$lp								;if it's not an environment
;
			ldx		CC_CDR_OFF,y						;X = first link in chain
			beq		@next$env							;if it's empty
			ldd		,s									;D = target ATOM
@atom$lp	cmpd	[CC_CAR_OFF,x]
			beq		@bound								;if found a binding for the ATOM
			ldx		CC_CDR_OFF,x						;X = next link in ENV chain
			bne		@atom$lp							;if not done
			bra		@next$env							;else try next environment
;
@bound		ldx		CC_CAR_OFF,x						;X = CONS with binding
			stx		,s
			puls	x,y,u
			orcc	#$04								;return EQ
			rts
;
@unbound	puls	x,y,u								;restore regs
			jsr		bail$ref							;and bail
			fcb		9,'u','n','b','o','u','n','d','!',BEL
