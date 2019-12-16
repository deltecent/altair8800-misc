CONC	EQU	10H	;CONSOLE STAT PORT
COND	EQU	11H	;CONSOLE DATA PORT
SPTR	EQU	0100H	;STACK POINTER
ESC	EQU	1BH	;ESC CHARACTER
;
;** MEMORY TEST PROGRAM
;
; This is the Vector Graphic memory test from the 2.0 monitor ROM
;       modified by Mike Douglas, 05/2013 for the Altair 2SIO.
;	Updated 5/4/15 to accept lower case in hex constants and
;	to look for ESC to restart console entry.
;
; Instructions for use:
;
;   At the "*" prompt, key in four hex characters for the starting
;   address followed by four hex characters for the ending address.
;   Press ESC to restart entry if a mistake is made. Starting
;   address is typically 0100h - just past this program. If RAM 
;   ends at DFFF (for example), specify an end address of E000.
;   This will cause the program to print an "error" for E000 at
;   the completion of each test cycle as an indicator of activity. 
;
;   The test will run continuously until reset. The program generates
;   a pseudo-random number sequence, writes a porition of it into
;   RAM and then regenerates the sequences from the same point to
;   compare with what is read from memory. If the pass is correct,
;   a new portion of the sequence is written to memory. Errors are
;   printed out with the address, what was written followed by what
;   was read.
;
	ORG	0
START	LXI	SP,SPTR
	MVI	A,3	;RESET THE 2SIO
	OUT	CONC
	MVI	A,15H	;8N1
	OUT	CONC
	CALL	CRLF
	MVI	A,'*'
	CALL	PTCN
	JMP	TMEM
;
;** CONVERT UP TO 4 HEX DIGITS TO BIN
;
AHEX	LXI	H,0	;GET 16 BIT ZERO
	MVI	C,4	;COUNT OF 4 DIGITS
AHE1	CALL	RDCN	;READ A BYTE
	DAD	H	;SHIFT 4 LEFT
	DAD	H
	DAD	H
	DAD	H
	SUI	48	;ASCII BIAS
	CPI	10	;DIGIT 0-10
	JC	ALF
	ANI	0DFH	;FORCE UPPER CASE
	SUI	7	;ALPHA BIAS
ALF	ADD	L
	MOV	L,A
	DCR	C	;4 DIGITS?
	JNZ	AHE1	;KEEP READING
	XCHG
SPCE	MVI	A,20H	;PRINT SPACE
PTCN	PUSH	PSW	;SAVE REG A
PTLOP	IN	CONC	;READ PRTR STATUS
	ANI	02H	;IF BIT 1 NOT 1,
	JZ	PTLOP	;WAIT TILL IT IS
	POP	PSW	;THEN RECOVER A
	OUT	COND	;AND PRINT IT
	RET		;RETURN FROM PTCN
CRLF	MVI	A,0DH	;PRINT CR
	CALL	PTCN
	MVI	A,0AH
	JMP	PTCN
;
;** READ FROM CONSOLE TO REG A. IF ESC ENTERED, RESTART
;      AT THE PROGRAM ENTRY POINT
;	
;
RDCN	IN	CONC	;READ KB STATUS
	ANI	1	;IF BIT 1 NOT 1
	JZ	RDCN	;REPEAT UNTIL IT IS
	IN	COND	;READ FROM KB
	ANI	7FH	;STRIP OFF MSB
	CPI	ESC	;ESCAPE PRESSED?
	JZ	START	;YES, START OVER
	JMP	PTCN	;ECHO ONTO PRINTER
;
;** MEMORY TEST ROUTINE
;
TMEM	CALL	AHEX	;READ BLOCK LEN
	CALL	AHEX	;READ START ADDRESS
	LXI	B,5A5AH	;INIT B,C
CYCL	CALL	RNDM
	PUSH	B	;KEEP ALL REGS
	PUSH	H
	PUSH	D
TLOP	CALL	RNDM
	MOV	M,B	;WRITE IN MEM
	CALL	BMP
	JNZ	TLOP	;REPEAT LOOP
	POP	D
	POP	H	;RESTORE ORIG
	POP	B	;VALUES OF
	PUSH	H
	PUSH	D
RLOP	CALL	RNDM	;GEN NEW SEQ
	MOV	A,M	;READ MEM
	CMP	B	;COMP MEM
	CNZ	ERR	;CALL ERROR ROUT
	CALL	BMP
	JNZ	RLOP
	POP	D
	POP	H
	JMP	CYCL
;** THIS ROUTINE GENERATES RANDOM NOS ***
RNDM	MOV	A,B	;LOOK AT B
	ANI	0B4H	;MASK BITS
	ANA	A	;CLEAR CY
	JPE	PEVE	;JUMP IF EVEN
	STC
PEVE	MOV	A,C	;LOOK AT C
	RAL		;ROTATE CY IN
	MOV	C,A	;RESTORE C
	MOV	A,B	;LOOK AT B
	RAL		;ROTATE CY IN
	MOV	B,A	;RESTORE B
	RET		;RETURN W NEW B,C
;
;** ERROR PRINT OUT ROUTINE
;
PTAD	CALL	CRLF	;PRINT CR,LF
	MOV	A,H	;PRINT
	CALL	PT2	;ASCII
	MOV	A,L	;CODES
	CALL	PT2	;FOR
	CALL	SPCE	;ADDRESS
	RET
ERR	PUSH	PSW	;SAVE ACC
	CALL	PTAD	;PRINT ADD.
	MOV	A,B	;DATA
	CALL	PT2	;WRITTEN
	CALL	SPCE
	POP	PSW	;DATA READ
PT2	PUSH	PSW
	CALL	BINH
	POP	PSW
	JMP	BINL
BINH	RAR
	RAR
	RAR
	RAR
BINL	ANI	0FH	;LOW 4 BITS
	ADI	48	;ASCII BIAS
	CPI	58	;DIGIT 0-9
	JC	PTCN
	ADI	7	;DIGIT A-F
	JMP	PTCN
BMP	MOV	A,E
	SUB	L
	JNZ	GOON
	MOV	A,D
	SBB	H
GOON	INX	H
	RET
	END











