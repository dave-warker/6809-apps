; "gc.asm" - Garbage Collection support for Syl
; 2023-feb-21 dww  created
;
; Your basic stop-the-world garbage collector for CONS cells.
;
; Walks through all currently live CONS refs marking them and
; any other CONS cells they reference as live in the GC mark pool.
; Then a sweep through all allocated cells adds any that are NOT
; marked as live to the free list. The free list is built from
; scratch on every GC.
;
; It is assumed that ONLY those refs rooted on items on the U stack
; are live. So if a function needs to protect a reference from the
; garbage collector it should push it on the U stack then pop it
; off when it's finished. The GC ignores non-CONS items on the U
; stack. Also never push anything that's not cell ref (ATOM, CONS,
; BIS, INT) on the U stack if there's a change the GC will see it
; otherwise Bad Things may happen.
;
; At the moment the ATOM pool is NOT garbage collected so when you
; run out of ATOM space you're screwed and have to restart the
; interpreter. Not that common since only unique ATOM names get
; stored there. GCing the ATOM pool is possible but kind of hairy.
; Assign 1 bit from the GC MARK pool for each byte of ATOM space.
; Walk the U stack and for every ATOM you find mark the assoc. bytes
; as live in the mark pool. Then scan the mark pool moving ATOM bytes
; down to fill in the free gaps and also build a table mapping offsets
; into the ATOM pool with the (negative) offset to add to shift it
; to it's new ATOM pool offset. Then walk the U stack again and use
; the offset table to adjust all of the ATOMs to their new values.
; Now you know why I decided not to do that just yet.

; >>> Free bytes
; RETURNS: X = free cell count as Int ref
gc$free:
			ldd		#CONS_POOL_END
			subd	<cons_unalloc							; = unallocated bytes
			lsra
			rorb
			lsra
			rorb											; = unallocated cell count
			addd	<cons_free_count						; + count in free list
			ora		#CCS_TYPE_INT/256
			tfr		d,x
			rts

; >>> Run a garbage collection cycle
; Assumes gc_mark_pool is already zeroed.
gc$run:
			pshs	u										;we're going to bach U so save to restore later
;
; Mark all entries on the Root Stack as LIVE
;
@rootlp		cmpu	#ROOT_STACK_TOP
			bhs		@sweep									;if we're all done
			ldy		,u++									;next root ref
			bsr		@marky?									;mark it if needed
			bra		@rootlp
;
@marky?		cmpy	#CCS_REF_CONS_FIRST
			blo		@ret									;if not a CONS
			cmpy	#CCS_REF_CONS_END
			blo		@marky									;if it *is* a CONS
@ret		rts												;else we're done
;
@marky		sty		,--s									;mark Y as LIVE in GC pool
			tfr		y,b
			lsrb											;low 2 bits are intra-cell
			lsrb
			orb		#%11111000
			tfr		b,x										;X = bit mask offset (0..7 -> -8..-1)
			tfr		y,d										;D = CONS ref
			subd	#CCS_REF_CONS_FIRST						;D = offset into CONS pool
			lsra
			rorb
			lsra
			rorb
			lsra
			rorb
			lsra
			rorb
			lsra
			rorb											;D = byte offset into MARK pool
			tfr		d,y
			ldb		gc_mark_pool,y							;turn ON MARK bit
			orb		@bits,x
			stb		gc_mark_pool,y
;
			ldy		,s										;mark CAR if needed
			ldy		CC_CAR_OFF,y
			bsr		@marky?
			ldy		,s++									;mark CDR if needed
			ldy		CC_CDR_OFF,y
			bra		@marky?
;
			fcb		$01,$02,$04,$08,$10,$20,$40,$80			;indexed with negative offset
@bits:
;
; Sweep CONS POOL looking for unmarked cells to add to the free list.
; We sweep 8 cells per loop (a SYL.ASM assertion ensures the pool size is a multiple of 8 cells)
; The sweep stops at the first unallocated. Doesn't matter if it's not a multiple of 8 cells
; as any extra unallocated will just get added to the free list.
;
@sweep		ldu		#gc_mark_pool							;U = MARK pool index
			ldy		#CCS_REF_CONS_FIRST						;Y = associated CONS cell ref
;
			ldx		#0										;start with empty free list
			stx		<cons_free_count
;
@sweep8		ldb		,u										;load next 8 MARKs
			clr		,u+										;and clear them for next time
			lda		#8
@sweeplp	cmpy	<cons_unalloc
			bhs		@done									;if done with allocated cells
			lsrb
			bcs		@swnext									;if this cell is LIVE
			stx		CC_CDR_OFF,y							;else add to free list
			ldx		<cons_free_count
			leax	1,x
			stx		<cons_free_count
			tfr		y,x
;
@swnext		leay	CC_SIZE,y								;next cell
			deca
			bne		@sweeplp
			bra		@sweep8									;if need to load next 8
;
; All done.
;
@done		stx		<cons_free_list							;updated free list head
			puls	u										;restore the root stack pointer
			ldx		#ATOM_NIL								;return NIL
			rts
