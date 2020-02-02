;****************************************************************************
;*
;*	Yet Another Memory Test (YAMT)
;*
;*	Written by Patrick A. Linstruth (patrick@deltecent.com)
;*	with routines by Mike Douglas.
;*
;****************************************************************************

maxSize		equ	64		; 64K

		org	0100h

;-----------------------------------------------------------------------------
;  Initialize the Console BIOS
;-----------------------------------------------------------------------------
boot		lxi	sp,ourStk	; initialize stack pointer

		call	chkCpm		; see if we're running under CP/M

		lda	cpmFlag		; running under CP/M?
		ora	a		; test for zero
		cz	conInit		; initialize console if not

		lxi	h,mBanner	; print welcome banner
		call	dispMsg		;

getSize		lxi	h,mSize		; memory size?
		call	dispMsg		;

		call	readLine	; get answer
		ora	a		;
		jnz	gotSize		;

		call	exitCpm
		jmp	getSize

gotSize		call	dec2bin		; convert to binary
		cpi	2		; 0-1?
		jc	badSize		;
		cpi	maxSize+1	; valid memory size?
		jc	setTop		; yes

badSize		lxi	h,mBadSz	; display invalid memory size
		call	dispMsg		;
		jmp	getSize

setTop		rlc			; covert to K
		rlc			;
		ani	0fch		; strip low 2 bits
		sta	memTop		; save

		lxi	h,mTesting
		call	dispMsg

		lxi	d,memBot
		call	dispWord

		lxi	h,mTo
		call	dispMsg

		lda	memTop
		mov	d,a
		mvi	e,0
		dcx	d
		call	dispWord

		lxi	h,mCrLf
		call	dispMsg

testMem		lxi	h,memBot	; start at the bottom

testLoop	mov	a,m		; get byte
		sta	memOr		; save original byte
		xra	a		; start at 0
testLoc		sta	memWr		; save byte wrote
		mov	m,a		; store byte
		cmp	m		; compare
		jnz	byteBad		; no match, found a bad byte

		inr	a		;
		jnz	testLoc		; test next value

byteGood	lda	memOr		; restore original byte
		mov	m,a		;
		jmp	nextByte

byteBad		mov	a,m		; save byte read
		sta	memRd

		shld	memBad		; save bad memory location

		lxi	h,mBad		;
		call	dispMsg		;

		lhld	memBad
		xchg
		call	dispWord

		lxi	h,mWrote	; display byte written
		call	dispMsg
		lda	memWr
		call	dispByte

		lxi	h,mRead		; display byte read
		call	dispMsg
		lda	memRd
		call	dispByte

		lxi	h,mCrLf		;
		call	dispMsg		;

		lhld	memBad

nextByte	inx	h		; next byte
		mov	a,h		; did we loop to page 0
		ora	a		;
		jz	testDone	;

		mov	a,l		; 0 byte boundary
		ora	a
		jnz	chkMemTop

		call	conStat		;
		jz	dispProg	; no keypress

		call	conIn		; get character
		cpi	CTRLC		;
		jz	abort		; Ctrl-C Abort?

dispProg	xchg			; hl to de
		call	dispWord
		mvi	b,CR
		call	conOut
		xchg			; de to hl

chkMemTop	lda	memTop		; did we hit top
		cmp	h
		jnz	testLoop	; loop

testDone	xchg
		lxi	h,mStopped
		call	dispMsg

		dcx	d		; display last address tested
		call	dispWord	;

		lxi	h,mDone		;
		call	dispMsg		;

		jmp	getSize		; Start Over

abort		lxi	h,mAbort
		call	dispMsg
		jmp	getSize


;**************************************************************************
; BIOS routines
;**************************************************************************

CR	equ	13		;ascii for carriage return
LF	equ	10		;ascii for line feed
DEL	equ	7fh		;ascii DEL
BS	equ	08h		;ascii backspace
CTRLC	equ	03		;ascii for control-c

wBoot   equ     0000h           ;CP/M warm boot vector
jmpInst equ     0c3h            ;8080 jump instruction
dSelect	equ	0c0h		;select drive 0

#if defined(_8080_)
#target rom
#code _CONSOLE
#endif

;-----------------------------------------------------------------------------
;  Initialize the Console BIOS
;-----------------------------------------------------------------------------
init	jmp	conInit		;initialize the 88-2SIO ports and return


; 88-2SIO Serial Board Equates

s2aCtl	equ	010h		;1st port on 88-2SIO board - control register
s2aDat	equ	011h		;1st port on 88-2SIO board - data register
s2Rdrf	equ	001h		;read data register full flag
s2Tdre	equ	002h		;transmit data register empty flag
s2Rst	equ	003h		;reset command
s28n1	equ	015h		;8N1 selection
s28n2	equ	011h		;8N2 selection

; The rcvByte subroutine above times a one second timeout with a code
;    loop that calls the hardware specific serIn routine below. ONESEC
;    must be set based on processor speed and the number of cycles in 
;    the serIn call + 59 cycles for the rcvByte code. 

ONESEC	equ	19231		;rcvByte loop count for 1 second

;----------------------------------------------------------------------------
; conInit - reset and initialize 88-2SIO ports for 8N2
;----------------------------------------------------------------------------
conInit	mvi	a,s2Rst		;reset and init 2nd 88-2SIO port
	out	s2aCtl
	mvi	a,s28n2		;configure console for 8N2 in case Teletype
	out	s2aCtl
	ret	

;----------------------------------------------------------------------------
; conStat - input character status
;    inputs:
;    outputs: z true if no character present
;	      z false if character present
;    clobbers none
;----------------------------------------------------------------------------
conStat	in	s2aCtl		;see if a new character is present
	ani	s2Rdrf
	ret	

;----------------------------------------------------------------------------
; conIn - input character from console
;    inputs:
;    outputs: z true if no character present
;	      z false if character returned in a
;    clobbers none
;----------------------------------------------------------------------------
conIn	in	s2aCtl		;see if a new character is present
	ani	s2Rdrf
	jz	conIn		;no character, wait

	in	s2aDat		;return character and non-zero status
	ret	

;----------------------------------------------------------------------------
; conOut - output character to console
;    inputs: b = character to send
;    clobbers a
;----------------------------------------------------------------------------
conOut	in	s2aCtl		;wait for OK to transmit
	ani	s2Tdre
	jz	conOut
	mov	a,b		;a=character to transmit
	out	s2aDat		;send it
	ret

;------------------------------------------------------------------------------
; dec2bin - Convert ascii-decimal string in h:l to binary value in a and b.
;   Return zero status if valid value found. Return non-zero status for error.
;   Clobbers c-e.
;------------------------------------------------------------------------------
dec2bin	mvi	b,0		;b accumulates result
	mov	a,m		;test for null string
	ora	a
	jz	decBad
	
decLoop	mov	a,m		;next ascii digit
	ora	a		;end of string?
	jz	decDone		;yes, we're done
	sui	'0'		;subtract ASCII offset
	rc			;before '0' - invalid digit (Z flag is cleared)
	cpi	10		;past 9?
	jnc	decBad	
	mov	c,a		;save digit in c for  now
	mov	a,b		;multiply value so far by 10
	add	a		;*2
	add	a		;*4
	add	b		;*5
	add	a		;*10
	add	c		;add in new digit to ones position
	mov	b,a		;leave result in b
	inx	h		;move to next digit
	jmp	decLoop

decDone	mov	a,b		;put result in a too
	ret			;zero flag was true upon entry to decDone

decBad	inr	a		;force zero flag false (a<>0FFh here.)
	ret

;------------------------------------------------------------------------------
; hex2bin - Convert ascii-hexadecimal string in h:l to binary value in a and b.
;   Return zero status if valid value found. Return non-zero status for error.
;   Clobbers c-e.
;------------------------------------------------------------------------------
hex2bin	mvi	b,0		;b accumulates result
	mov	a,m		;test for null string
	ora	a
	jz	decBad
	
hexLoop	mov	a,m		;next ascii digit
	ora	a		;end of string?
	jz	decDone		;yes, we're done
	cpi	'9'+1		;below ASCII 9?
	jc	HC1		;Yes: deal with digit
	cpi	'A'		;between 9 & A?
	rc			;y:error (Z flag is cleared.)
	sui	'A'-'9'-1	;no: subtract offset	
HC1:	sui	'0'
	cpi	10H		;above 0Fh?
	jnc	decBad		;y: error
	mov	c,a		;save digit in c for  now
	mov	a,b		;multiply value so far by 16
	add	a
	add	a
	add	a
	add	a
	add	c		;add in new digit to ones position
	mov	b,a		;leave result in b
	inx	h		;move to next digit
	jmp	hexLoop

;------------------------------------------------------------------------------
; dispDec - display value in A as a two digit ascii-decimal value. Won't
;   work for values over 99. Clobbers a,b,c.
;------------------------------------------------------------------------------
dispDec	mvi	c,'0'-1		;c accumulates the 10's digit in ascii

tenCnt	inr	c		;count the number of 10's in ascii
	sui	10		;divide A by 10 to get the 10's digit
	jp	tenCnt

	adi	'0'+10		;compute final 1's digit in ascii
	mov	b,a		;save 1's digit in b
	mov	a,c		;zero suppress 10's digit
	cpi	'0'
	cnz	conOut		;transmit the 10's digit
	mov	c,b		;transmits the 1's digit
	call	conOut
	ret

;-----------------------------------------------------------------------------
; dispMsg - display the null-terminated message passed in h:l on the
;    console device. Clobbers b, hl
;-----------------------------------------------------------------------------
dispMsg	mov	a,m		;get the next message byte
	ora	a		;null terminates
	rz

	mov	b,a		;conOut wants character in b
	call	conOut
	inx	h		;move to next byte
	jmp	dispMsg

;-----------------------------------------------------------------------------
; dispVer - display version
;-----------------------------------------------------------------------------
dispVer	lxi	h,mVer		;display version
	jmp	dispMsg


;-----------------------------------------------------------------------------
; dispNib - display nibble (0-F) in a
;-----------------------------------------------------------------------------
dispNib		ani	0FH		; LSB
		adi	'0'		; Add ASCII 0
		cpi	':'		; < ':'
		jc	dNout
		adi	7		; A-F

dNout		mov	b,a		; send character
		jmp	conOut

;-----------------------------------------------------------------------------
; dispByte - display byte (00-FF) in a
;-----------------------------------------------------------------------------
dispByte	push	psw		; save a
		rrc			; high nibble
		rrc			; 
		rrc			; 
		rrc			; 
		call	dispNib
		pop	psw
		jmp	dispNib

;-----------------------------------------------------------------------------
; dispWord - display word (0000-FFFF) in d:e
;-----------------------------------------------------------------------------
dispWord	mov	a,d
		call	dispByte
		mov	a,e
		jmp	dispByte

;-----------------------------------------------------------------------------
; getLine - get line from console
; length returned in a
; line in h:l
;-----------------------------------------------------------------------------
readLine	xra	a
		sta	lLen		; set length to zero
		lxi	h,lBuf		; h:l = line buffer
		mvi	m,0		; clear buffer

rLwait		call	conStat		; wait for a console character
		jz	rLwait
	
		call	conIn		; get character
		cpi	CR		; if cr done
		jz	rLdone
	
		cpi	BS		; Backspace?
		jnz	rLsave		; No, save character
	
		lda	lLen		; If buffer empty
		ora	a		; wait for next character
		jz	rLwait		;
	
		
		dcx	h		; decrement buffer
		mvi	m,0		; erase last character
		dcr	a		; decrement counter
		sta	lLen		;
		push	h		; save h:l
		lxi	h,mBS		; send backspace sequence
		call	dispMsg
		pop	h

		jmp	rLwait		; wait for next character

rLSave		mov	m,a		; store character
		mov	b,a
		call	conOut		; echo character

		inx	h		; move buffer to next location
		mvi	m,0		; null terminate

		lda	lLen		; increment length
		inr	a
		sta	lLen
		cpi	lSize		; buffer full?
		jz	rLdone

		jmp	rLwait		; wait for next character

rLdone		lxi	h,mCrLf		; send cr/lf
		call	dispMsg		;
		lda	lLen		; return length in a
		lxi	h,lBuf		; return buffer in h:l
		ret

;------------------------------------------------------------------------------
; chkCpm - check if running under CP/M. CP/M flag is set true (non-zero)
;     if yes, cleared otherwise.
;------------------------------------------------------------------------------
; First, initialize entries for stand-alone

chkCpm	xra	a
	sta	cpmFlag		;clear CP/M flag

; Determine if we're under CP/M or standalone. CP/M is assumed if
;   a jump instruction is present at the CP/M warm start location (0)
;   and five more jumps (e.g., a jump table) is present at the
;   jump-to destination.
		
	lda	wBoot		;see if jump instruction present for CP/M
	cpi	jmpInst
	rnz			;no, not CP/M

; A jump instruction is present at the CP/M warm boot location (0),
;   now see if that jump points to five more jumps. If so, assume CP/M

	lxi	h,wBoot+1	;point to lsb of jump address
	mov	e,m		;e=low byte of jump
	inx	h
	mov	d,m		;de=destination of jump
	mvi	b,5		;look for 5 more jumps (a jump table)
jmpTest	ldax	d		;a=opcode at jump destination
	sui	jmpInst		;another jump present?
	rnz			;no, not CP/M
	inx	d		;move to next jump
	inx	d
	inx	d
	dcr	b
	jnz	jmpTest

; Running under CP/M. Set CP/M flag, allow CP/M exit message to display

	mvi	a,1		;replace null with a leading space to
	sta	cpmFlag		;set CP/M flag to non-zero value

	lxi	h,mCpmFnd	;running under CP/M
	jmp	dispMsg		;display and return

;------------------------------------------------------------------------------
; exitCpm - if running under CP/M, prompt user to insert the CP/M
;    disk and then warm-start CP/M. Otherwise, just return.
;------------------------------------------------------------------------------
exitCpm	lda	cpmFlag		;running under CP/M?
	ora	a		;test for zero
	rz			;no, not CP/M

	jmp	wBoot		;warm boot CP/M

;**************************************************************************
; 
;  BIOS Data area
;
;**************************************************************************

mCrLf	db	CR,LF,0
mBS	db	BS,' ',BS,0
mVer	db	CR,LF,'Deltec Enterprises Console BIOS Ver. 1.0 of 11/24/19',CR,LF,0
mCpmFnd	db	CR,LF,'Found CP/M.',CR,LF,0

cpmFlag ds      1               ;non-zero if running under CP/M

lSize	equ	80		;line buffer size
lLen	db	0		;line length
lBuf	ds	lSize		;line buffer

;**************************************************************************
; 
;  YAMT Data area
;
;**************************************************************************

mBanner		db	CR,LF,'Yet Another Memory Test (YAMT) Ver. 1.2 of 02/01/20',CR,LF,0
mSize		db	CR,LF,'Memory Size (2-64)? ',0
mBadSz		db	'Invalid Memory Size',0
mTesting	db	'Testing Memory From 0x',0
mTo		db	' to 0x',0
mBad		db	'Found Bad Byte at 0x',0
mWrote		db	' Wrote:',0
mRead		db	' Read:',0
mStopped	db	'Stopped Test at 0x',0
mDone		db	CR,LF,'Test Complete!',CR,LF,0
mAbort		db	CR,LF,'Test Aborted!',CR,LF,0

		ds	24		; stack space
ourStk		equ	$

memSize		dw	0000H		; memory size
memOr		db	0H		; byte original
memWr		db	0H		; byte wrote
memRd		db	0H		; byte read
memBad		dw	0000H		; bad memory location
memTop		db	00H		; memory top
memBot		equ	$		; memory bottom

		end
