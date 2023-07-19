; "cons.asm" - CONS pool management for Syl
; 2023-feb-20 dww created

; >>> Test if CONS Slot Ref is an integer
; PASSED:  X = CONS slot ref
; RETURNS: EQ if it's an integer else NE
ref$is$int:
			cmpx	#CCS_TYPE_INT
			blo		@ret							;if not an integer
@eq			orcc	#$04							;else return EQ
@ret		rts
;
;
; >>> Test if CONS Slot Ref is an ATOM
; PASSED:  X = CONS slot ref
; RETURNS: EQ if it's an ATOM else NE
ref$is$atom:
			cmpx	#CCS_REF_ATOM_END
			blo		@eq								;if it is an ATOM
@ne			andcc	#~$04							;else return NE
			rts
;
; >>> Test if CONS Slot Ref is a CONS cell
; PASSED:  D = CONS slot ref
; RETURNS: EQ if it's a CONS cell else NE
ref$is$cons:
			cmpx	#CCS_REF_CONS_FIRST
			blo		@ne								;if it's not a CONS cell
			cmpx	#CCS_REF_CONS_END
@blo		blo		@eq								;if it is
			bra		@ne								;else it's not
;
; >>> Test if CONS Slot Ref is to a built-in
; PASSED:  D = CONS slot ref
; RETURNS: EQ if it's a built-in function else NE
ref$is$bis:
			cmpx	#CCS_REF_BIS_FIRST
			blo		@ne								;if it's not a built-in
			cmpx	#CCS_REF_BIS_END
			bra		@blo							;finish like CONS

; >>> Allocate a CONS cell with a specific CAR tag value
; PASSED:  D = desired CAR tag value
; RETURNS: X = pointer/ref to a CONS cell
; NOTE: Doesn't return if memory is totally exhausted
cons$alloc$tag:
			pshs	d
			bsr		cons$alloc
			puls	d
			std		CC_CAR_OFF,x
			rts

; >>> Allocate a CONS Cell
; RETURNS: X = pointer/ref to a CONS cell
; NOTE: Doesn't return if memory is totally exhausted
cons$alloc:
			ldx		<cons_free_list						;try the free list
			beq		@unalloc							;if it's empty
;
@unlink		ldd		CC_CDR_OFF,x						;unlink it
			std		<cons_free_list
			ldd		#-1
			addd	<cons_free_count
			std		<cons_free_count
;
@init		clra										;init to NILs
			clrb
			std		CC_CAR_OFF,x
			std		CC_CDR_OFF,x
			rts
;
@unalloc	ldx		<cons_unalloc						;can we allocate a new one?
			cmpx	#CONS_POOL_END
			bhs		@gc									;nope, need to garbage collect
			tfr		x,d									;yep, update unallocated pointer
			addd	#CC_SIZE
			std		<cons_unalloc
			bra		@init								;and init it to NILs
;
@gc			bsr		gc$run								;collect the garbage
			ldx		<cons_free_list						;get the first freed
			bne		@unlink								;if got at least one
;
			jsr		bail								;else memory is full
			fcb		10,'M','e','m',' ','f','u','l','l','!',BEL
