; "skv.asm" - static kernel variables for Alf
; 2022-dec-13 dww  created
;
; The holds the actual r/w storage for variables needed by the kernel.
; They can't live in the actual kernel code because that must be read/only.
; Assume this is all completely unitialized on a cold start.

warm$		rmb		2						;will be ALF_WARM_ID after cold start

dp$			rmb		2						;next available dictionary storage addr
defs$		rmb		2						;head of defined words list
def$$		rmb		2						;0 or points to word being defined
comp$		rmb		1						;0 if executing else 1 if compiling

loading$	rmb		1						;normally 0, 1 if doing a "load"
rdc$next	rmb		1						;RBC_BUF_EMPTY or index of next buffer char
rdc$buf		rmb		2+RBC_BUF_LEN			;console input buffer (max + lex + data)
str$buf		rmb		2+STR_MAX_LEN			;temp string buffer

base$		rmb		2						;used for numeric printing
