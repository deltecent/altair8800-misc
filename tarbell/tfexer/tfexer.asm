; TFEXER.ASM
;
; THIS PROGRAM IS USED TO EXERCISE FLOPPY DRIVES CONNECTED TO A
; TARBELL 1011 SSSD FLOPPY DISK CONTROLLER.
;
; THIS SOFTWARE IS BASED ON AFEXER BY MIKE DOUGLAS.
;
; TFEXER SUPPORTS THE FOLLOWING COMMANDS:
;
;   D [x]    Select drive x, If x omitted, current drive # is displayed.
;   C	     Compare track number read from disk with expected track
;	     number. If expected track number is undefined, it is
;	     set to the track number read from disk.
;   F x      Format track x.
;   G        Display FD1771 registers
;   R x      Read sector x
;   S x [y]  Step to track x. If optional track y is specified, then the 
;	     head is stepped between x and y on each subsequent press of 
;            the space bar. Pressing Q exits the test. 
;   W x hh   Write sector x with hex hh
;
; Immediate commands (action taken as soon as the key is pressed)
;   T        Toggle head load
;   I	     Step in one track
;   O        Step out one track
;   Z        Restore to track zero
; 
; Version    Author        Date        Description
; 1.0        P. Linstruth  12/10/19    Initial Version
;

; Tarbell SSSD Disk Drive Controller Equates

drvBase	equ	0f8h		;drive base IO port
drvStat	equ	drvBase+0	;drive status register (in)
drvCmd	equ	drvBase+0	;drive command register (out)
drvTrk	equ	drvBase+1	;drive track register (in/out)
drvSec	equ     drvBase+2	;drive sector register (in/out)
drvData	equ	drvBase+3	;drive data register (in/out)
drvWait	equ	drvBase+4	;drive wait register (in)
drvSel	equ	drvBase+4	;drive select register (out)

busyMsk	equ	001h		;mask to get busy bit alone
indxMsk	equ	002h		;mask to get index bit alone
drqMsk	equ	002h		;mask to get DRQ alone
trk0Msk	equ	004h		;mask to get TRACK0 bit alone
lostMsk	equ	004h		;mask to get lost data bit alone
crcMsk	equ	008h		;mask to get CRC error bit alone
seekMsk	equ	010h		;mask to get seek error bit alone
notfMsk	equ	010h		;mask to get not found bit alone
headMsk	equ	020h		;mask to get head load bit alone
wrtfMsk	equ	020h		;mask to get write fault bit alone
wrtpMsk	equ	040h		;mask to get write protect bit alone
nrdyMsk	equ	080h		;mask to get not ready bit alone
errMsk	equ	0fch		;mask to test for errors

dcRest	equ     000h            ;drive command - restore
dcSeek	equ     010h            ;drive command - seek
dcStep	equ     020h            ;drive command - step
dcStepI	equ     040h            ;drive command - step in
dcStepO	equ     060h            ;drive command - step out
dcRead	equ     08ch            ;drive command - read sector
dcWrite	equ     0ach            ;drive command - write sector
dcRdTr	equ     0e4h            ;drive command - read track
dcWrtTr	equ     0f4h            ;drive command - write track
dcReadA	equ     0c4h            ;drive command - read address
dcIntr	equ     0d0h            ;drive command - force interrupt

fHeadLd	equ	008h		;flag - head load
fVerify	equ	004h		;flag - verify
fUpdate	equ	010h		;flag - update
fMult	equ	010h		;flag - multiple record
fLength	equ	008h		;flag - block length
fImmInt	equ	008h		;flag - immediate interrupt
f10ms	equ	002		;flag - 10ms step rate

dSelect	equ	0c0h		;select drive 0

; Disk subsystem parameters

sectLen	equ     128             ;length of Tarbell sector
numSPT	equ     26              ;sectors/track
maxTrk	equ     77              ;max track number of tracks
numDrvs equ     4               ;number of drives on a controller

; 88-2SIO Channel A Serial Interface Equates

sioACtl equ     010h            ;port A on 88-2SIO board
sioADat equ     011h
sioTdre equ     002h            ;mask to test for xmit ready
sioRdrf equ     001h            ;mask to test for rcv read

;  Misc Equates

cmdLen  equ     16              ;length of command buffer
cr      equ     13              ;ascii carriage return
lf      equ     10              ;ascii line feed
bs      equ     8               ;ascii backspace
notSet  equ     0ffh            ;track or drive number not set
wBoot   equ     0000h           ;CP/M warm boot vector
jmpInst equ     0c3h            ;8080 jump instruction


	org	0100h		;start at 0100h

;------------------------------------------------------------------------------
; Initialize stack pointer, UART and data structures
;------------------------------------------------------------------------------
	lxi     sp,stack        ;init stack pointer

	call    chkCpm          ;determine if running under CP/M

	call    initSio         ;init 2SIO if needed

	xra     a               ;default to drive zero
	sta     newDrv          ;
	sta     curDrv          ;...for first drive selection

       	lxi     h,mVer          ;display version message
       	call    dispMsg
       	lxi     h,mAuthor	;display author message
       	call    dispMsg

;------------------------------------------------------------------------------
; cmdLoop - main command loop
;------------------------------------------------------------------------------
cmdLoop	lxi	h,mPrompt	;display the prompt
	call	dispMsg
	lxi	h,cmdLoop	;create return address for commands
	push	h
	call	readCmd		;get the command
	lxi	h,cmdBuf	;assume 1st character is the command
	mov	a,m
	inx	h		;point hl to params past command character
	cpi	'D'		;select drive N
	jz	cmdDrvN
	cpi	'Z'		;restore to track zero
	jz	cmdTrk0
	cpi	'C'		;compare track on disk to expected track
	jz	cmdCmp
	cpi	'S'		;step to track n
	jz	cmdTrkN
	cpi	'O'		;immedate step out-in
	jz	cmdOut
	cpi	'T'		;immediate step in-out
	jz	cmdTog
	cpi	'I'		;immediate step in-out
	jz	cmdIn
	cpi	'F'		;format track
	jz	cmdFmtN
	cpi	'G'		;display registers
	jz	cmdRegs
	cpi	'R'		;read sector
	jz	cmdRead
	cpi	'W'		;write sector
	jz	cmdWrt
	cpi	'X'		;exit to CP/M
	cz	exitCpm
	lxi	h,mHelp		;invalid command entered, display help
	call	dispMsg	
	lxi	h,mHelp2
	jmp	dispMsg		;display and return to cmdLoop

;------------------------------------------------------------------------------
; cmdRegs - display registers
;------------------------------------------------------------------------------
cmdRegs	lxi	h,mTrkReg	;track register
	call	dispMsg
	in	drvTrk
	call	dispHex
	call	dispEOL

	lxi	h,mSecReg	;sector register
	call	dispMsg
	in	drvSec		
	call	dispHex
	call	dispEOL

	lxi	h,mStaReg	;status register
	call	dispMsg
	in	drvStat
	jmp	dispHex		;return

;------------------------------------------------------------------------------
; cmdDrvN - Select drive N
;------------------------------------------------------------------------------
cmdDrvN	call	getTokn		;get the drive number
	call	dec2bin		;convert ascii token to binary in A
	jnz	noDrv		;no drive specified
	cpi	numDrvs		;valid drive number?
	jnc	badDrv
	sta	newDrv		;newDrv = the new drive to select	
	call	selDrv		;select the drive in newDrv

; New drive selected. Display the selected drive

	lxi	h,mDrive	;display 'Selected Drive n'
	call	dispMsg
	lda	curDrv		;display selected drive number
	call	dispDec

; Print current track

	lxi	h,mDrvTrk	;display the track the drive is on
	call	dispMsg
	in	drvTrk		;get the current track to display
	jmp	dispDec		;display and return to cmdLoop

; noTrack - current track is undefined. Display "not set"
;
;noTrack	lxi	h,mNoTrk
;	jmp	dispMsg		;display and return to cmdLoop

; noDrv - no drive specified, or drive selection failed. Display 
;    the current drive number

noDrv	lxi	h,mCurDrv	;display current drive prompt
	call	dispMsg
	lda	curDrv		;display current drive number
	jmp	dispDec		;display and return to cmdLoop

; noSet - curDrv has not been set yet. Display "not set" as current drive number
;
;noSet	lxi	h,mNotSet
;	jmp	dispMsg		;display and return to cmdLoop

; badDrv - bad drive number specified. Display message.
	
badDrv	lxi	h,mBadDrv	;bad drive number specified	
	jmp	dispMsg		;display and return to cmdLoop

;------------------------------------------------------------------------------
; cmdTrk0 - Seek to track zero
;------------------------------------------------------------------------------
cmdTrk0	call	seek0
	rz			;exit if seek0 failed
	lxi	h,mTrack0	;display track 0 message
	jmp	dispMsg		;display and return to cmdLoop

;------------------------------------------------------------------------------
; cmdOut - Step out one track. Step is issued no matter what. 
;    If curTrk valid, update it.
;------------------------------------------------------------------------------
cmdOut	in	drvStat		;already at track 0?
	ani	trk0Msk
	jnz	atDisp		;display at track message

	lda	headLd		;head load flag
	ori	dcStepO		;step out command
	ori	fUpdate		;update track register
	ori	f10ms		;10ms step rate
	out	drvCmd
	in	drvWait		;wait for INTRQ

	in	drvTrk		;get track from drive
	mov	c,a		;save track in c
	jmp	movExit		;save and display it

;------------------------------------------------------------------------------
; cmdIn - Step in one track. Step is issued no matter what.
;     If curTrk valid, update it.
;------------------------------------------------------------------------------
cmdIn	in	drvTrk		;get current track from drive
	cpi	maxTrk-1	;already stepped in max
	jz	atDisp		;display at track message

	lda	headLd		;head load flag
	ori	dcStepI		;step in command
	ori	fUpdate		;update track register
	ori	f10ms		;10ms step rate
	out	drvCmd
	in	drvWait		;wait for INTRQ

	in	drvTrk		;read track register
	mov	c,a		;c=current track number
	jmp	movExit		;save and display it

;------------------------------------------------------------------------------
; cmdTrkN - Seek to track N or toggle seek between two tracks each time
;	the space bar is pressed. Any other key exits.
;------------------------------------------------------------------------------
cmdTrkN	call	getTokn		;get track token		
	call	dec2bin		;convert ascii token to binary in a and b
	jnz	badTrk		;1st track specification invalid
	sta	track1		;save 1st track number value
	mvi	a,maxTrk-1
	cmp	b		;reasonable track number?
	jc	badTrk		;n: error

	call	getTokn		;get 2nd track number (if any)
	call	dec2bin
	jnz	noTrk2		;2nd track not specified
	sta	track2		;save 2nd track number
	mvi	a,maxTrk-1
	cmp	b		;reasonable track number?
	jc	badTrk		;n: error

; Two track numbers provided. Loop here toggling back and forth when
;    space bar pressed. Exit to command loop when any other key pressed.

trkLoop	lda	track1		;move to 1st track number
	call	movTrk
	call	tglTrk		;prompt to toggle tracks
	rnz			;return to cmdLoop
	lda	track2
	call	movTrk
	call	tglTrk
	jz	trkLoop
	ret			;return to cmdLoop
	
; One track number provided. Seek to that track then exit

noTrk2	lda	track1		;move to 1st track number
	jmp	movTrk		;move and return to cmdLoop

; No valid track number provided.

badTrk	lxi	h,mBadTrk	;bad track message
	jmp	dispMsg		;display and return to cmdLoop

;------------------------------------------------------------------------------
; tglTrk - put up message to toggle between track 1 and track 2 and then
;    wait for the user's response. Zero status true to continue. Zero
;    status false to exit. 
;------------------------------------------------------------------------------
tglTrk	lxi	h,mSpace	;press space bar message
	call	dispMsg
	call	rcvChar
	cpi	' '		;space toggles tracks
	rnz			;no space - exit with Z clear
	lxi	h,mCrLf
	jmp	dispMsg		;returns with Z set

;------------------------------------------------------------------------------
; cmdRead - read and display sector
;------------------------------------------------------------------------------
cmdRead	call	getTokn		;get track token		
	call	dec2bin		;convert ascii token to binary in a and b
	jnz	badSect		;1st track specification invalid
	mvi	a,numSPT
	cmp	b		;reasonable track number?
	jc	badSect		;> sectors error
	xra	a		;sector 0 error
	cmp	b
	jz	badSect	

	lxi	h,sectBuf

	call	chkRdy		;is drive ready?
	jnz	notRdy		;no - not ready message and return

	mov	a,b		;send sector to drive
	out	drvSec

	mvi	a,dcRead	;send read command
	out	drvCmd

rdLoop	in	drvWait		;wait for INTRQ/DRQ
	ora	a
	jp	rdDone		;INTRQ is done

	in	drvData		;get byte
	mov	m,a		;store byte
	inx	h
	jmp	rdLoop		;get next byte

rdDone	in	drvStat		;get status
	ora	a		;
	jz	dispSec		;no error - display sector

rdErr	mov	e,a		;save error
	lxi	h,mRdErr	;read error
	jmp	dispErr		;display error - return

;------------------------------------------------------------------------------
; cmdWrt - write sector
;------------------------------------------------------------------------------
cmdWrt	call	getTokn		;get track token		
	call	dec2bin		;convert ascii token to binary in a and b
	jnz	badSect		;1st track specification invalid
	mvi	a,numSPT
	cmp	b		;reasonable track number?
	jc	badSect		;> sectors error
	xra	a		;sector 0 error
	cmp	b
	jz	badSect	
	mov	a,b
	sta	wrSec		;save sector number

	call	getTokn		;get value to write
	call	hex2bin
	jnz	noVal		;write value not specified
	sta	wrVal		;save write value

	call	chkRdy		;is drive ready?
	jnz	notRdy		;no - not ready message and return

	lda	wrSec		;send sector to drive
	out	drvSec

	mvi	a,dcWrite	;send write command
	out	drvCmd

wrLoop	in	drvWait		;wait for INTRQ/DRQ
	ora	a
	jp	wrDone		;INTRQ is done

	lda	wrVal		;get write value
	out	drvData		;write byte
	jmp	wrLoop		;write next byte

wrDone	in	drvStat		;get status
	ora	a		;
	rz			;no error - return

	mov	e,a		;save error
	lxi	h,mWrErr	;write error
	jmp	dispErr		;display error - return

; No valid sector number provided.

badSect	lxi	h,mBadSec	;bad track message
	jmp	dispMsg		;display and return to cmdLoop

noVal	lxi	h,mBadVal	;bad value message
	jmp	dispMsg		;display and return to cmdLoop

;------------------------------------------------------------------------------
; dispErr - display error message in h:l and value in e
;------------------------------------------------------------------------------
dispErr call	dispMsg
	mov	a,e
	call	dispHex		;display code and return
	lxi	h,mCrLf
	call	dispMsg
	xra	a		;set zero flag
	ret

;------------------------------------------------------------------------------
; cmdTog - Toggle head load
;------------------------------------------------------------------------------
cmdTog	call	chkRdy		;is drive ready?
	jz	togHld		;yes - toggle head load

	xra	a		;drive not ready
	sta	headLd		;clear head load flag

	jmp	notRdy		;no ready message - return

togHld	lda	headLd		;get current head load flag
	xri	fHeadLd		;flip it
	ani	fHeadLd		;isolate it
	sta	headLd		;save it
	jz	unldHd

	lxi	h,mHeadL
	jmp	dispMsg

unldHd	lxi	h,mHeadU
	jmp	dispMsg


;------------------------------------------------------------------------------
; seekCur - Seek to current track. Return status in A
;------------------------------------------------------------------------------
seekCur	in	drvTrk		;get current track
	out	drvData		;save in data register

	lda	headLd		;get head load flag
	ori	dcSeek		;seek command
	out	drvCmd		;execute
	in	drvWait		;wait for INTRQ

	in	drvStat		;return status in a
	ret

;------------------------------------------------------------------------------
; readTrk - Reads a track
;------------------------------------------------------------------------------

readTrk	call	chkRdy		;is drive ready
	jnz	notRdy		;no - exit

	mvi	a,dcRdTr	;issue read track command
	out	drvCmd

rtLoop	in	drvWait		;wait for DRQ or INTRQ
	ora	a
	rp			;INTRQ - done with track read

	in	drvData		;read byte
	jmp	rtLoop		;get next byte

;------------------------------------------------------------------------------
; cmdCmp - Read and display the track number from disk along with the
;    expected track number. If the expected track number is not valid
;    and a valid track number was found on the disk, set curTrk and
;    the trkTbl entry for this drive to the track number from disk.
;------------------------------------------------------------------------------
cmdCmp	call	chkRdy
	jnz	notRdy

	call	rdTrkId		;read track ID again
	mov	b,a		;b=read track id

; display the track number found on disk

	mov	e,b		;e=track read from disk
	lxi	h,mTrkId	;display track ID message
	call	dispMsg
	mov	a,e		;a=track number read
	call	dispDec		;display it
	jmp	dispExp		;go display expected track

; dispExp - display the expected track number

dispExp	lxi	h,mExpTrk	;display expected track number
	call	dispMsg

	in	drvTrk		;a=expected track
	jmp	dispDec		;yes, display it and exit

;------------------------------------------------------------------------------
;  rdTrkId - Read track number from the next sector found. Returns
;    track number in A if found. If sector hunt times out, an invalid
;    sector number is returned in A.
;    clobbers a,de,hl
;------------------------------------------------------------------------------
rdTrkId	call	chkRdy		;drive must be ready
	jnz	notRdy		;

	lxi	h,addrBuf

	mvi	a,dcReadA	;read address
	out	drvCmd

rdTData	in	drvWait		;wait for DRQ/INTRQ
	ora	a
	jp	rdTDone		;INTRQ = done

	in	drvData		;read the track byte
	mov	m,a		;save in address buffer
	inx	h
	jmp	rdTData

rdTDone	in	drvStat		;error?
	ani	errMsk
	jnz	rdErr		;yes - display error

	lda	addrBuf		;return first byte in a

	ret

;-----------------------------------------------------------------------------
; chkRdy - Check if drive is ready
;   Not zero if drive not ready, otherwise zero
;-----------------------------------------------------------------------------
chkRdy	in	drvStat
	ani	nrdyMsk
	ret

;-----------------------------------------------------------------------------
; notRdy - display drive not ready message
;-----------------------------------------------------------------------------
notRdy	lxi	h,mNotRdy
	jmp	dispMsg

;-----------------------------------------------------------------------------
;  resDrv - reset the drive's controller. return status is A.
;-----------------------------------------------------------------------------
resDrv	mvi	a,dcIntr	;force interrupt
	ori	fImmInt		;immediate interrupt
	out	drvCmd

	in	drvWait		;wait for DRQ/INTRQ

	in	drvStat		;a=drive status

	ret

;-----------------------------------------------------------------------------
;  selDrv - select the drive specified in newDrv and update track.
;       clobbers a, c, de, hl
;-----------------------------------------------------------------------------
selDrv	lda	newDrv		;drive number to select
	lxi	h,curDrv	;point to currently selected drive number
	mov	m,a

	ani	003h		;drive number 0-3
	cma			;tarbell's latch is inverted
	add	a		;shift bits 4 places
	add	a			
	add	a
	add	a
	ori	2		;set latch
	out	drvSel		;select new drive

	call	resDrv		;reset drive and get status

	jmp	seek0		;seek to track 0 - return

;-----------------------------------------------------------------------------
; seek0 - seek to track zero. Steps in 3 tracks before seeking back out
;    to track zero in case the zero stop is incorrect. The max number of
;    steps outward is the max number of tracks on the disk plus 16. These
;    extra steps will cause an SA400 minidisk to find track 0 even if the
;    actuator mechanism is out of its spiral groove.
;-----------------------------------------------------------------------------
seek0	lda	headLd		;head load flag
	ori	dcRest		;issue restore command
	ori	f10ms		;10ms step rate
	out	drvCmd
	in	drvWait

	call	waitBsy		;kill some time for zero flag

	in	drvStat		;get status register
	ani	004h		;check track 0 flag
	rnz			;no error - return

stErr	mov	e,a		;save error in e
	lxi	h,mStErr	;step error
	jmp	dispErr		;display error - return

;------------------------------------------------------------------------------
;  movTrk - moves to the track specified in A. Assumes A is valid.
;     trashes a,c,de,hl
;------------------------------------------------------------------------------	
movTrk	out	drvData		;store A in track register

	lda	headLd		;head load flag
	ori	dcSeek		;issue SEEK
	ori	f10ms		;10ms step rate
	out	drvCmd		;
	in	drvWait		;wait for INTRQ

; movExit - store the new track number in curTrk and in trkTbl for the
;    current drive number, then display "moved to track" message
; movDisp - display "moved to track" message

movExit	lxi	h,mTrackN	;'Moved to track '
	call	dispMsg
	in	drvTrk		;display track number from binary value
	jmp	dispDec		;display and exit

; atDisp - display "at track" message

atDisp	lxi	h,matTrkN	;'Already at track '
	call	dispMsg
	in	drvTrk		;display track number from binary value
	jmp	dispDec		;display and exit

;------------------------------------------------------------------------------
;  saveTrk - Save the track number passed in C to curTrk and to the
;    track table entry for the current drive.
;    clobbers a,de,hl
;------------------------------------------------------------------------------
saveTrk	ret

;------------------------------------------------------------------------------
; cmdFmtN - format a track
;------------------------------------------------------------------------------
cmdFmtN	call	getTokn		;get the drive number
	call	dec2bin		;convert ascii token to binary in A
	jnz	badTrk		;no track specified
	cpi	maxTrk		;valid track number?
	jnc	badTrk
	call	movTrk		;move to selected track

	mvi	d,1		;sector cnt to 0
	mvi	e,numSPT	;set max # sectors
	mvi	b,40		;gap 4 preindex 40 bytes of ff

	mvi	a,dcWrtTr	;load track write command
	out	drvCmd		;issue track write

; write preindex fill

preInd	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0FFh		;load preindex fill
	out	drvData		;write it on disk
	dcr	b		;count = count - 1
	jnz	preInd		;go back till b = 0

	mvi	b,6

preIn1	in	drvWait
	ora	a
	jp	fmtFail

	xra	a
	out	drvData
	dcr	b
	jnz	preIn1

; write address mark on track

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0FCh		;load address mark
	out	drvData		;write it on disk

; post index gap

postGap	mvi	b,26		;set # of bytes

postId	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0FFh		;load fill data
	out	drvData		;write it on disk
	dcr	b		;count = count - 1
	jnz	postId		;if not 0 go back

; pre id section

	mvi	b,6		;get # of bytes

sector	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	xra	a		;make a = 0
	out	drvData		;write it on track
	dcr	b		;count = count - 1
	jnz	sector		;jmp back if not done

; write id address mark

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;if error jmp out

	mvi	a,0FEh		;get address mark
	out	drvData		;write it on disk

; write track number on disk

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	in	drvTrk		;get track number
	out	drvData		;write it on disk

; write one byte of 00

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	xra	a		;set a to 0
	out	drvData		;write it on disk

; write sector # on disk

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mov	a,d		;get sector #
	out	drvData		;write it on disk

; one more byte 0

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	xra	a		;set a to 00
	out	drvData		;write it on disk
	inr	d		;bump sect. #

; write 2 crc's on this sector

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0F7h		;get crc pattern
	out	drvData		;write it on disk

; pre data 11 bytes FF, 6 bytes 00

	mvi	b,11		;set count
preDat	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0FFh		;set a to FF
	out	drvData		;write it on disk
	dcr	b		;reduce count by 1
	jnz	preDat		;go back if not done

	mvi	b,6

preDa1	in	drvWait
	ora	a
	jp	fmtFail

	xra	a 
	out	drvData
	dcr	b
	jnz	preDa1

; data address mark

	in	drvWait		;wait for drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0FBh		;get data address mark
	out	drvData		;write it on disk

; fill data field with E5

	mvi	b,128		;set field length
dFill	in	drvWait		;wait for drq
	ora	a		;you know what
	jp	fmtFail		;happens here by now

	mvi	a,0E5h		;get fill byte
	out	drvData		;write it on disk
	dcr	b		;drop 1 from count
	jnz	dFill		;do till 00

; write CRC's

	in	drvWait		;wait till drq
	ora	a		;set flags
	jp	fmtFail		;jmp out if error

	mvi	a,0F7h		;get crc byte
	out	drvData		;write it on disk

; end of sector fill

	dcr	e		;reduce sector count
	jz	endTrk		;if 0 do end of track rtn

datGap	in	drvWait		;wait for drq
	ora	a		;set flags	
	jp	fmtFail		;jmp out if error

	mvi	a,0FFh		;get fill character
	out	drvData		;write it on disk
	jmp	postGap		;go back for more

; endTrk - fill with FFh until command ends at index hole

endTrk	in	drvWait		;wait for drq or intrq
	ora	a		;set flags
	jp	done		;jmp out if error

	mvi	a,0FFh		;load a with FFh
	out	drvData		;write it on disk
	jmp	endTrk		;do until intrq

; done - check for error from the write track command. Wait for trim
;     erase delay in case the drive has tunnel erase heads, then
;     move to the next track.

done	in	drvStat		;read status of write track command
	ani	0ffh		;test for any flag
	jnz	fmtFail		;if err go to err print rtn

	lxi	h,mFmtDon	;format complete message
	jmp	dispMsg		;display message and return

fmtFail mov	e,a		;save error in e
	lxi	h,mFmtErr	;format fail message
	jmp	dispErr		;display error - return


;------------------------------------------------------------------------------
; waitBsy - Wait for busy flag to clear
;------------------------------------------------------------------------------
waitBsy	in	drvStat
	ani	busyMsk
	jnz	waitBsy

	ret

;------------------------------------------------------------------------------
; readCmd - Read a command line from the console into cmdBuf. Handles
;   backspace, terminates on C/R. Converts lower case to upper case.
;   Clobbers a, b, c, h, l.
;------------------------------------------------------------------------------
readCmd	mvi	b,0		;b = stored character count
	lxi	h,cmdBuf	;hl = pointer to cmdBuf
nxtChar	call	rcvChar		;get character from serial port

; Look for special characters (CR, BS, control characters)

	cpi	cr		;C/R?
	jz	cmdDone
	cpi	bs		;back space?
	jz	backSpc
	cpi	020h		;ignore control characters 
	jc	nxtChar
	cpi	'a'		;convert lower to upper case (garbage past 'z')
	jc	upper
	sui	020h
upper	mov	c,a		;save the character in c
	mov	a,b		;any more room left?
	cpi	cmdLen-1	
	jz	nxtChar		;out of room for more characters
	mov	m,c		;put the new character in the buffer
	inx	h		;increment buffer pointer
	inr	b		;increment stored character counter
	call	sndChar		;echo character in c to the serial port

; check for an immediate T, Z, I, or O command

	mov	a,c		;a=current character
	cpi	'T'
	jz	test1st		;see if first character on the line
	cpi	'Z'
	jz	test1st		;see if first character on the line
	cpi	'I'
	jz	test1st		;see if first character on the line
	cpi	'O'
	jnz	nxtChar		;not 1st on the line, continue

test1st	mov	a,b		;see if character count = 1 
	dcr	a
	jz	cmdDone		;im,ediate command at 1st position
	jmp	nxtChar		;otherwise, keep looping

;  backSpc - backspace pressed. Backup up in the buffer and echo a backspace,
;     space, backspace to visually delete the character.

backSpc	mov	a,b		;see if already at zero characters
	ora	a
	jz	nxtChar		;nothing to delete
	dcr	b		;decrement the character count
	dcx	h		;and the the buffer pointer
	mvi	c,bs		;echo BS, space, BS to do a delete
	call	sndChar	
	mvi	c,' '
	call	sndChar
	mvi	c,bs
	call	sndChar
	jmp	nxtChar

;  cmdDone - Carriage return received. Zero terminate the string. Echo
;     the carriage return and add a line feed.

cmdDone	mvi	m,0		;store null terminator
	lxi	h,mCrLf		;echo carriage return, line-feed
	jmp	dispMsg

;------------------------------------------------------------------------------
; rcvChar - Return a character from the serial port in A. MSB is cleared.
; Z is cleared cleared unless received chr is a null.
;------------------------------------------------------------------------------
rcvChar	lda	headLd		;see if we need to load the head
	ora	a
	jz	rStat

	lda	headCnt		;get counter
	dcr	a		;decrement
	sta	headCnt		;save counter
	jnz	rstat		;if not zero skip seek

	in	drvStat		;check for busy
	ani	busyMsk
	cz	seekCur		;not busy - seek to load head

rStat	call	rcvStat		;wait for a character
	jz	rcvChar
	ret

;------------------------------------------------------------------------------
; rcvStat - Test for Serial Port A chr and get it if available. Return
;    with a=0 and Z set if no character available or if null received.
;------------------------------------------------------------------------------
rcvStat	in	sioACtl		;wait for a character
	ani	sioRdrf		;set z, clear a if no chr		
	rz
	in	sioADat		;a = received character
	ani	07fh		;strip parity, clear Z unless null
	ret

;------------------------------------------------------------------------------
; getTokn - moves the next token as pointed to by HL to the token buffer.
;    Leading spaces or commas are skipped. Trailing space, comma or 
;    terminating null in the input buffer terminates the token. The token
;    is null terminated. Clobbers a, d, e. hl updated to allow subsequent
;    calls for the next token
;------------------------------------------------------------------------------
getTokn	lxi	d,token		;de points to token string
skpLead	mov	a,m		;move from cmdBuf to token
	stax	d				
	ora	a		;end of string?
	rz			;yes, all done
	inx	h		;move to next input character
	cpi	' '		;skip leading spaces
	jz	skpLead
	cpi	','		;treat commas as spaces
	jz	skpLead
	inx	d		;move to next token spot

;  Leading spaces skipped. Move characters until trailing space, comma
;      or null is reached.

tokLoop	mov	a,m		;get next character from cmdBuf
	stax	d				
	ora	a		;end of string?
	rz			;yes, all done
	inx	h		;move to next input character
	cpi	' '		;trailing space terminates token
	jz	tokDone
	cpi	','
	jz	tokDone
	inx	d		;move to next token spot
	jmp	tokLoop

; Insert null terminator at end of token

tokDone	xra	a		;store terminating null
	stax	d
	ret

;------------------------------------------------------------------------------
; dec2bin - Convert ascii-decimal token to binary value in a and b. Return zero
;   status if valid value found. Return non-zero status for error.
;   Clobbers c-e.
;------------------------------------------------------------------------------
dec2bin	mvi	b,0		;b accumulates result
	lxi	d,token		;de is token string pointer
	ldax	d		;test for null string
	ora	a
	jz	decBad
	
decLoop	ldax	d		;next ascii digit
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
	inx	d		;move to next digit
	jmp	decLoop

decDone	mov	a,b		;put result in a too
	ret			;zero flag was true upon entry to decDone

decBad	inr	a		;force zero flag false (a<>0FFh here.)
	ret

;------------------------------------------------------------------------------
; hex2bin - Convert ascii-hexadecimal token to binary value in a and b.
;   Return zero status if valid value found. Return non-zero status for error.
;   Clobbers c-e.
;------------------------------------------------------------------------------
hex2bin	mvi	b,0		;b accumulates result
	lxi	d,token		;de is token string pointer
	ldax	d		;test for null string
	ora	a
	jz	decBad
	
hexLoop	ldax	d		;next ascii digit
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
	inx	d		;move to next digit
	jmp	hexLoop


;------------------------------------------------------------------------------
; dispSpin - display spinner
;------------------------------------------------------------------------------
dispSpn	lxi	h,mSpin
	lda	spinCnt
	mov	b,a		;save count in b
	add	l
	mov	l,a
	mov	a,m
	ora	a
	jnz	sendSpn

	xra	a		;reset spin counter
	sta	spinCnt
	jmp	dispSpn

sendSpn	mov	c,a
	call	sndChar
	mvi	c,bs
	call	sndChar
	mov	a,b
	inr	a
	sta	spinCnt

	ret
	
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
	cnz	sndChar		;transmit the 10's digit
	mov	c,b		;transmits the 1's digit
	call	sndChar
	ret

;------------------------------------------------------------------------------
; dispSec - display sector buffer
;------------------------------------------------------------------------------
dispSec lxi	h,sectBuf
	xra	a
dispNxt	sta	bufCnt
	push	h
	mov	a,m
	call	dispHex
	mvi	c,' '
	call	sndChar
	lda	bufCnt
	ani	007h		;if 7th byte, send CR
	cpi	007h
	jnz	skipCr

	lxi	h,mCrLf		;start new line
	call	dispMsg

skipCr	pop	h
	lda	bufCnt
	inr	a
	cpi	sectLen		;last character, return
	rz

	inx	h
	jmp	dispNxt

;------------------------------------------------------------------------------
; dispHex - display value in A as a two digit ascii-hexidecimal value.
;   Clobbers a,b.
;------------------------------------------------------------------------------
dispHex	mov	b,a		;store value in b

	ani	0f0h		;high nibble
	rrc
	rrc
	rrc
	rrc
	call	dispNib		;transmit the 16's digit
	mov	a,b
	ani	00fh
	jmp	dispNib		;transmits the 1's digit
	ret

;------------------------------------------------------------------------------
; sndHex - display value in A as a single digit ascii-hexidecimal value.
;   Clobbers a,c.
;------------------------------------------------------------------------------
dispNib	cpi	10		;greater than 9?
	jc	sndNib		;0-9
	adi	007h

sndNib  adi	'0'
	mov	c,a
	jmp	sndChar

;------------------------------------------------------------------------------
; dispAny - Display null terminated string, followed by '. any key to abort.
; Clobbers a,c,h,l.  Clears a, sets Z.
;------------------------------------------------------------------------------
dispAny	call	dispMsg		;print pessage
	lxi	h,mAnyKey	;followed by ". any key to abort"

; fall into dispMsg

;------------------------------------------------------------------------------
; dispMsg - Display null terminated string.
;   Clobbers a,c,h,l. Clears a, sets Z.
;------------------------------------------------------------------------------
dispMsg	mov	a,m		;next character to send
	ora	a		;exit on null
	rz
	mov	c,a		;send the character
	call	sndChar
	inx	h
	jmp	dispMsg

;------------------------------------------------------------------------------
; dispEOL - Display CF/LF
; Clobbers h,l.
;------------------------------------------------------------------------------
dispEOL	lxi	h,mCrLf
	jmp	dispMsg		;returns with Z set

;------------------------------------------------------------------------------
; sndChar - Send the character in C out the serial port. Clobbers a.
;------------------------------------------------------------------------------
sndChar	in	sioACtl		;wait until OK to xmit
	ani	sioTdre
	jz	sndChar
	mov	a,c		
	out	sioADat		;send the character
	ret

;------------------------------------------------------------------------------
; chkCpm - check if running under CP/M. CP/M flag is set true (non-zero)
;     if yes, cleared otherwise.
;------------------------------------------------------------------------------
; First, initialize entries for stand-alone

chkCpm	xra	a
	sta	cpmFlag		;clear CP/M flag
	sta	mExit		;prevent CP/M exit message from showing

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

	mvi	a,' '		;replace null with a leading space to
	sta	mExit		;   allow CP/M exit message to display
	sta	cpmFlag		;set CP/M flag to non-zero value

;	lxi	h,mCpmFnd	;running under CP/M
;	jmp	dispMsg		;display and return
	ret

;------------------------------------------------------------------------------
; exitCpm - if running under CP/M, prompt user to insert the CP/M
;    disk and then warm-start CP/M. Otherwise, just return.
;------------------------------------------------------------------------------
exitCpm	lda	cpmFlag		;running under CP/M?
	ora	a		;test for zero
	rz			;no, not CP/M

; Prompt user to re-insert CP/M disk, wait for response, then
;    warm boot CP/M

	mvi	a,dSelect	;select drive 0
	out	drvSel
	lxi	h,mCpm		;display "Insert CP/M disk"	
	call	dispMsg
	call	rcvChar		;wait for a character
	jmp	wBoot		;warm boot CP/M

;------------------------------------------------------------------------------
; initSio - reset and initialize 2SIO port A if not running under CP/M
;------------------------------------------------------------------------------
initSio	lda	cpmFlag		;running under CP/M?
	ora	a
	rnz			;yes, 2SIO already initialized
	mvi	a,3		;reset ACIA
	out	sioACtl
	mvi	a,015h		;RTS on, 8N1
	out	sioACtl
	ret

;------------------------------------------------------------------------------
; message strings
;------------------------------------------------------------------------------
mVer	db	'Tarbell Floppy Drive Exerciser, Ver 1.0',cr,lf,0
mAuthor	db	'Deltec Enterprises LLC 2019',cr,lf,0
mPrompt	db	cr,lf,'CMD>',0

mHelp	db	cr,lf,'Valid commands are:',cr,lf
	db	'  D [x]        Select drive x or display current',cr,lf
	db	'               drive if x omitted',cr,lf
	db	'  C            Compare the track number read from',cr,lf
	db	'               disk with the expected track number',cr,lf
	db	'  F x          Format track x',cr,lf
	db	'  G            Display FD1771 registers',cr,lf
	db	'  R x          Read sector x',cr,lf
	db	'  S x [y]      Step to track x and optionally toggle to y',cr,lf
	db	'  W x hh       Write sector x with hex hh',cr,lf
mExit	db	'  X            Exit to CP/M',cr,lf,0

mHelp2	db	lf,'Immediate commands:',cr,lf
	db	'  T            Toggle head load',cr,lf
	db	'  O            Step out one track',cr,lf
	db	'  I            Step in one track',cr,lf
	db	'  Z            Restore to track zero',cr,lf,0

mNoAct	db	'No action taken',0
mHeadL	db	'Head loaded',0
mHeadU	db	'Head unloaded',0
mTrack0	db	'Moved to track zero',0
mTrackN	db	'Moved to track ',0
mFmtDon	db	'Format complete',0
mFmtErr	db	'Format error ',0
mAtTrkN	db	'Already at track ',0
mTrkId	db	'Track ID from disk: ',0
mNoTkId	db	'Track ID could not be read',0
mExpTrk	db	cr,lf,'Expected track: ',0
mTrkSet	db	cr,lf,'  ** Current track set to ',0
mDrive	db	'Selected drive ',0
mCurDrv	db	'Current drive is ',0
mNotRdy	db	cr,lf,'Drive not present or loaded',cr,lf,0
mDrvTrk	db	cr,lf,'Current track is ',0
mTrkReg	db	' Track Register: ',0
mSecReg	db	'Sector Register: ',0
mStaReg	db	'Status Register: ',0
mNotSet	equ	$
mNoTrk	db	'not set',0
mSpace	db	cr,lf,'   Press Spacebar to toggle tracks: ',0
mStpIn	db	'Stepped in one track',0
mStpOut	db	'Stepped out one track',0
mBadDrv	db	'Invalid drive number',0
mBadTrk	db	'Invalid track number',0
mBadSec	db	'Invalid sector number',0
mWtDrv	db	'Waiting for drive to be ready',0
mBadVal	db	'Invalid write value',0
mWrtDat	db	cr,lf,'Writing to disk',0
mNoTk0	db	'Track 0 not found',0
mMSpind	db	cr,lf,'Measuring Spindle Revs',0
mAnyKey	db	'. Press any key to abort.',0
mMsRev	db	' mS/rev',0
mRdErr	db	'Read error: ',0
mWrErr	db	'Write error: ',0
mStErr	db	'Step error: ',0
mCpmFnd	db	cr,lf,'Found CP/M.',cr,lf,0
mCpm	db	cr,lf,'Insert CP/M disk into drive A, then press Return...',0
mTimOut	db	cr,lf,'Disk timeout',0
mCrLf	db	cr,lf,0
mComma	db	', ',0
mSpin	db	'|/-\',0


;------------------------------------------------------------------------------
; cmdBuf - command buffer, variables and stack space.
;------------------------------------------------------------------------------
cmdBuf  ds      16              ;command input buffer
token   ds      16              ;token buffer
track1  ds      1               ;1st track # for toggling tracks
track2  ds      1               ;2nd track # for toggling tracks
wrSec	ds	1		;write sector number
wrVal	ds	1		;write sector value
newDrv  ds      1               ;new drive # to select
curDrv  ds      1               ;currently selected drive #
curErr  ds      1               ;current error
sectBuf	ds	sectLen		;sector buffer
addrBuf	ds	6		;address buffer
bufCnt	ds	1		;buffer count
spinCnt	db	0		;spinner count
headLd	db	0		;head load flag
headCnt	db	0		;head load counter
cpmFlag ds      1               ;non-zero if running under CP/M

;------------------------------------------------------------------------------
; stack
;------------------------------------------------------------------------------
	ds	64
stack	equ	$

	end

