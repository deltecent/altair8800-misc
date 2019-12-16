;******************************************************************
;	PROM BASED BOOT ROUTINE FOR TARBELL SINGLE DENSITY CP/M
;	  
;	Updated 2/3/2016 by Mike Douglas to fit in a single PROM
;   	and to make the PROM position independent - the same PROM
;	will run most anywhere in memory.
;
;******************************************************************
;
;	This program is designed to boot sector 1 of track 0
;	on a Tarbell disk using the Tarbell single density
;	disk controller. Track 0 is formatted single density
;	so the CPU can do the data transfer. The boot routine
;	loads a secondary boot loader from disk into memory
;	at address zero and then jumps to that routine.
;
;	The part of this program that actually does the boot
;	operation is moved from the PROM into RAM at 1000h for
;	execution. This is required due to the wait state
;	inserted for execution from PROM.
;
;		MICHAEL J. KARAS
;		MICRO RESOURCES
;		These days Mike can be reached at 
;		mkaras@carousel-design.com (March 23, 2009)
;
;******************************************************************

;SYSTEM EQUATES FOR TARBELL CONTROLLER

DWAIT	EQU	0FCH		;WAIT FOR DISK PORT
DCOM	EQU	0F8H		;DISK COMMAND PORT
DDATA	EQU	0FBH		;DISK DATA PORT
DSTAT	EQU	0F8H		;DISK STATUS PORT
DSEC	EQU	0FAH		;DISK SECTOR PORT
DTRK	EQU	0F9H		;DISK TRACK PORT
DSEL	EQU	0FCH		;DISK SELECT PORT

;SYSTEM VARIABLES AND ADDRESS POINTERS
;
SBOOT	EQU	007DH		;SINGLE DENSITY BOOT ENTRY
RDCMD	EQU	008CH		;READ COMMAND FOR 1791 CONTROLLER

;DEFINE SI/O RS-232 CONSOLE I/O PARAMETERS

CCTRL	EQU	010H		;CONSOLE COMMAND/STATUS PORT
CDATA	EQU	011H		;CONSOLE DATA PORT
CRRDY	EQU	001H		;RECEIVER READY BIT
CTRDY	EQU	002H		;TRANSMITTER READY BIT

CR	EQU	0Dh		;ASCII carriage return
LF	equ	0Ah		;ASCII line feed

	org	1000h		;use RAM address for assembly, the
				;PROM itself can go most anywhere
;--------------------------------------------------------------
; Copy the PROM content to RAM for execution. This copy routine is 
;   is position independent so the boot PROM can be at most any
;   address (256 byte boundary assumed).
;--------------------------------------------------------------
	lxi	d,MOVELP	;DE->MOVELP in RAM

	lxi	sp,STACK
	lxi	h,0E9E1h	;H=PCHL,L=POP H
	push	h		;POP H, PCHL at STACK-2, STACK-1
	call	STACK-2		;addr of MOVELP in HL and stack RAM
	
MOVELP:
	dcx	sp		;point SP to MOVELP address
	dcx	sp		;    in stack memory

	mov	a,m		;get next EPROM byte
	stax	d		;store it in RAM

	inr	e		;bump pointers
	inr	l
	rnz			;copy to end of 256 byte page
	
	jmp	INIT		;jump to code now in RAM

;---------------------------------------------------------------
; The loader once in RAM starts here
;---------------------------------------------------------------

;INITIALIZE

INIT:
	LXI	H,00FFFH	;DELAY SI/O INIT FOR
				; MESSAGE IN PROGRESS
LOOP:
	DCX	H
	MOV	A,H
	ORA	L
	JNZ	LOOP

	MVI	A,003H		;INITIALIZE SI/O WITH RESET
	OUT	CCTRL
	MVI	A,011H		;8N2, DIVIDE BY 16
	OUT	CCTRL

;START OF COLD BOOT LOADER CODE

START:
	LXI	H,CBMSG		;OUTPUT "CP/M COLD BOOT" TO THE CONSOLE
	CALL	MSG
	MVI	A,0F2H		;SELECT DISK A: AT SINGLE DENSITY
	OUT	DSEL
	MVI	A,0D0H		;CLEAR ANY PENDING COMMAND
	OUT	DCOM
	NOP			;ALLOW TIME FOR COMMAND SETTLING
	NOP
	NOP
	NOP
HOME:
	IN	DSTAT		;GET STATUS
	RRC
	JC	HOME		;WAIT FOR NOT BUSY COMPLETION

	MVI	A,002H		;ISSUE RESTORE CMND (10 MSEC. STEP RATE)
	OUT	DCOM
	NOP			;ALLOW TIME FOR COMMAND SETTLING
	NOP
	NOP
	NOP
	IN	DWAIT		;WAIT FOR COMPLETION
	ORA	A		;SET FLAGS FOR ERROR ON "DRQ",NOT "INTRQ"
	JM	DRQER

	IN	DSTAT		;GET DISK STATUS
	ANI	004H		;MASK FOR TRACK 00 STATUS BIT
	JZ	TK0ER

	XRA	A		;ZERO ACCUMULATOR
	MOV	L,A		;SETUP MEMORY LOAD ADDRESS 0000H
	MOV	H,A
	INR	A		;SETUP FOR SECTOR 01
	OUT	DSEC
	MVI	A,RDCMD		;SETUP READ COMMAND
	OUT	DCOM
	NOP			;ALLOW TIME FOR COMMAND SETTLING
	NOP
	NOP
	NOP
RLOOP:
	IN	DWAIT		;WAIT FOR DISK CONTROLLER
	ORA	A		;SET FLAGS
	JP	RDONE		;ARE WE DONE YET

	IN	DDATA		;GET DATA FORM DISK
	MOV	M,A		;MOVE IT INTO MEMORY
	INX	H		;INCREMENT MEMORY POINTER
	JMP	RLOOP		;GO GET NEXT BYTE
RDONE:
	IN	DSTAT		;GET DISK READ STATUS
	ORA	A		;CHECK FOR ERRORS
	JZ	SBOOT		;NO ERRORS?
				;THEN GO BOOT DOUBLE DENSITY CP/M

	LXI	H,LEMSG		;OUTPUT "BOOT LOAD ERROR=" TO CONSOLE
	JMP	LERR

DRQER:	LXI	H,RQMSG		;OUTPUT "COMMAND COMPLETION ERROR=" TO CONSOLE
	JMP	LERR

TK0ER:	LXI	H,REMSG		;OUTPUT "RESTORE ERROR=" TO CONSOLE

LERR:	mov	b,a		;save error code in B
	CALL	MSG
	CALL	BYTEO		;DISPLAY ERROR from B
	lxi	h,CRLF		;transmit CR/LF 
	call	MSG

	JMP	START		;GO TRY BOOTING AGAIN

;COLD BOOT LOADER CONSOLE I/O INTERFACE ROUTINES

; MESSAGE PRINTING ROUTINE (HL->string, last character has MSBit set)

MSG:
	mov	a,m		;A=next character to send
	call	co		;transmit it
	ora	m		;msb set?
	rm			;yes, we're done
	inx	h		;otherwise, do next character
	jmp	msg

; BYTE PRINT CONVERSION ROUTINE (byte in B)

BYTEO:
	mov	a,b		;A=byte to display
	CALL	BYTO1		;display MSN 

	mov	a,b		;A=byte to display
	JMP	BYTO2		;dislay LSN

BYTO1:
	RRC
	RRC
	RRC
	RRC
BYTO2:
	ANI	0FH
	CPI	0AH
	JM	BYTO3
	ADI	7
BYTO3:
	ADI	30H
				;fall through to display character

; CONSOLE OUTPUT ROUTINE (character in A)

CO:
	mov	c,a		;save character in C

WTRDY:	IN	CCTRL		;WAIT FOR READY TO XMIT
	ANI	CTRDY
	JZ	WTRDY

	MOV	A,C
	ani	07Fh		;remove msbit
	OUT	CDATA
	RET


; COLD BOOT ROUTINE MESSAGES

CBMSG:	DB	CR,LF,'CP/M BOOT'
CRLF:	DB	CR,LF+80h

LEMSG:	DB	CR,LF,'LOAD ERR','='+80h

RQMSG:	DB	CR,LF,'COMMAND ERR','='+80h

REMSG:	DB	CR,LF,'RESTORE ERR','='+80h

	DS	64	;SETUP STORAGE FOR A RAM BASED STACK
STACK	EQU	$

	END

