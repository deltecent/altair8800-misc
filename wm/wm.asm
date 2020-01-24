;*********************************************************************
;*
;*	MicroPro WORDMASTER release 5.55A
;*	Copyright (c) 1978 MicroPro International Corporation
;*
;*	Reconstructed from memory image on January 21, 2020
;*	by Patrick A Llinstruth (patrick@deltecent.com)
;*
;*********************************************************************

MEMORY	equ	29b8h

WBOOT	equ	0000h
BIOSJVT	equ	0001h		;BIOS JMP Vector Table
BDOS	equ	0005h
FBASE	equ	0006h
RESTART	equ	0038h
FCB	equ	05ch		;File Control Block
FCB2	equ	06ch		;File Control Block
FCBFN	equ	1
FCBEXT	equ	9
DBUFF	equ	0080h

CONIN	equ	1
CONOUT	equ	2
RDRIN	equ	3
LISTOUT	equ	5
DIRIO	equ	6
PRINTS	equ	9
OPENF	equ	15
CLOSEF	equ	16
DELEF	equ	19
READS	equ	20
MAKEF	equ	22
CURDSK	equ	25

BS	equ	008h
TAB	equ	009h
LF	equ	00ah
CR	equ	00dh
EOF	equ	01ah
ESC	equ	01bh
DEL	equ	07fh

	org	00100h

	jmp ENTRY		;0100	c3 69 02 	. i . 
	jmp BREAKBP		;0103	c3 60 02 	. ` . 
COPYR:
	db ' COPYRIGHT (C) 1978  MICROPRO INTERNATIONAL CORPORATION      '
CRLF:
	db CR,LF,0
BANNER:
	db CR,'MicroPro WORDMASTER release 1.07A serial # WM6344V5   ',CR,LF,0
CLRSCRN:
	db 0,0,0,0,0,0		;PATCH SPACE
	mvi	a,ESC		;0186	3e 1b 	> . 
	call	OUTCHAR		;0188	cd ef 01 	. . . 
	mvi	a,'*'		;018b	3e 2a 	> * 
	call	OUTCHAR		;018d	cd ef 01 	. . . 
	mvi	a,EOF		;0190	3e 1a 	> . 
	call	OUTCHAR		;0192	cd ef 01 	. . . 
	ret

;
; *** SUBROUTINE TO POSITION CURSOR AT      ***
; ***     LINE L (0=TOP), COLUMN H (0=LEFT) ***
;
;PTCO SOL VERSION:
; SENDS ESC, 02H, Y(L), ESC, 01H, X(H)
;
TCURSOR:
	mvi	a,ESC		;0196	3e 1b 	> . 
	call	OUTCHAR		;0198	cd ef 01 	. . . 
	mvi	a,'='		;019b	3e 3d 	> = 
	call	OUTCHAR		;019d	cd ef 01 	. . . 
	mvi	a,' '		;01a0	3e 20 	>   
	add	l			;01a2	85 	. 
	call	OUTCHAR		;01a3	cd ef 01 	. . . 
	mvi	a,' '		;01a6	3e 20 	>   
	add	h			;01a8	84 	. 
	call	OUTCHAR		;01a9	cd ef 01 	. . . 
	ret			;01ac	c9 	. 

	db	0,0,0,0,0
	db	0,0,0,0,0

;PBEGMEM POINTS TO BEGINNING OF MEMORY TO USE
;FOR EDIT BUFFER AND SCRATCHPAD. IF SPACE IS NEEDED
;FOR PATCHES, PUT THEM WHERE THIS POINTS AND
;INCREASE THIS POINTER. REMEMBER TO USE A LARGE
;ENOUGH "SAVE" COMMAND!

PBEGMEM: DW MEMORY

 ;SCREEN SIZE: TAKEN FROM THE FOLLOWING,
 ;EXCEPT SET AUTOMATICALLY TO MATCH HARDWARE
 ;VALUE WHEN IMSAI VIO VIDEO DISPLAY IS IN USE
 ;(DETECTED BY PRESENSE OF THE VIO ROM AT PROPER
 ;ADDRESS, AND CON: IOBYTE FIELD = 2 OR 3).

HITE:	db 24			;MUST BE EXACT SCREEN HEIGHT IN LINES
WID:	db 80			;MUST BE <= EXACT SCREEN WIDTH

 ;EREOL CONTAINS THE CHARACTER(S) TO ERASE SCREEN
 ;TO END-OF-LINE WITHOUT MOVING CURSOR, IF SUCH A
 ;CHARACTER IS AVAILABLE IN THE TERMINAL HARDWARE.
 ;IF 0, WILL BE SIMULATED BY SOFTWARE.
 ;AUTOMATICALLY SET TO CTL-U WHEN VIO IS IN USE.

EREOL:	DB 0	;(FIRST) CHARACTER, OR 0 IF NONE
	DB 0	;SECOND CHARACTER IF TERMINAL
		;...REQUIRES 2-CHARACTER
		;...SEQUENCE, ELSE A 0.

 ;NOVIO: IF NON-0, WILL NOT LOOK FOR IMSAI VIO.
 ;PATCH NON-0 IF VIO PROVIOSIONS INTERFERE WITH
 ;YOUR TERMINAL.

NOVIO:	DB 0		;PATCHED FOR PTCO-SOL
	DB 0,0,0	;RESERVED FOR EXPANSION


;DELAYS EXECUTED AFTER VARIOUS TERMINAL FUNCTIONS,
;BEFORE NEXT CHARACTER IS SENT TO TERMINAL. THESE
;ALLOW TIME FOR TERMINAL TO RESPOND, AS REQUIRED
;BY SOME TERMINALS WHEN USED AT HIGH BAUD RATES.
;INCREASE IF YOU EXPERIENCE, FOR EXAMPLE, LOSS OF
;CHARACTERS AFTER CLEAR SCREEN.  EACH DELAY IS
;APPROX NUMBER OF MILLISECONDS ON 4MHZ PROCESSOR;
;DELAY IS TWICE AS LONG AS SHOWN FOR 2MHZ 8080.

DELCLR:	DB 25	;DELAY AFTER CLEAR SCREEN: 25+ MSEC.
DELCUS:	DB 10	;DELAY AFTER POSITION CURSOR: 10+MSEC.
DELERE:	DB 5	;DELAY AFTER ERASE TO EOL: 5+ MSEC.

	DB 0,0,0,0,0,0,0,0,0,0	;MORE 
	DB 0,0,0,0,0,0,0,0,0,0	;EXTRA 
	DB 0,0,0,0,0,0,0,0,0,0	;PATCHING 
	DB 0,0,0,0,0,0,0,0,0,0	;SPACE 

	jmp DELAY		;01ec	c3 4f 02 	. O . 

OUTCHAR:
	jmp l2592h		;01ef	c3 92 25 	. . % 
s01f2h:
	lxi h,00000h		;01f2	21 00 00 	. . . 
	shld CURXPOS		;01f5	22 83 26 	" . & 
	push b			;01f8	c5 	. 
	push d			;01f9	d5 	. 
	push h			;01fa	e5 	. 
	call CLRSCRN		;01fb	cd 80 01 	. . . 
	lda DELCLR		;01fe	3a c1 01 	: . . 
	call DELAY		;0201	cd 4f 02 	. O . 
	pop h			;0204	e1 	. 
	pop d			;0205	d1 	. 
	pop b			;0206	c1 	. 
	ret			;0207	c9 	. 
s0208h:
	lhld l268fh		;0208	2a 8f 26 	* . & 
l020bh:
	xra a			;020b	af 	. 
	sta l26a1h		;020c	32 a1 26 	2 . & 
	push d			;020f	d5 	. 
	xchg			;0210	eb 	. 
	lhld CURXPOS		;0211	2a 83 26 	* . & 
	call CMPDEHL		;0214	cd 9f 1f 	. . . 
	xchg			;0217	eb 	. 
	pop d			;0218	d1 	. 
	rz			;0219	c8 	. 
	shld CURXPOS		;021a	22 83 26 	" . & 
	push b			;021d	c5 	. 
	push d			;021e	d5 	. 
	push h			;021f	e5 	. 
	call TCURSOR		;H,L -> X,Y
	lda DELCUS		;0223	3a c2 01 	: . . 
	call DELAY		;0226	cd 4f 02 	. O . 
	pop h			;0229	e1 	. 
	pop d			;022a	d1 	. 
	pop b			;022b	c1 	. 
	jmp l234eh		;022c	c3 4e 23 	. N # 
s022fh:
	push h			;022f	e5 	. 
	lhld CURXPOS		;0230	2a 83 26 	* . & 
	dcr h			;0233	25 	% 
	call l020bh		;0234	cd 0b 02 	. . . 
	pop h			;0237	e1 	. 
	ret			;0238	c9 	. 

ERAEOL:
	push	psw			;0239	f5 	. 
	lda	EREOL		;023a	3a bb 01 	: . . 
	call	s259bh		;023d	cd 9b 25 	. . % 
	lda	EREOL+1		;0240	3a bc 01 	: . . 
	ora	a			;0243	b7 	. 
	cnz	s259bh		;0244	c4 9b 25 	. . % 
	lda	DELERE		;0247	3a c3 01 	: . . 
	call	DELAY		;024a	cd 4f 02 	. O . 
	pop	psw			;024d	f1 	. 
	ret			;024e	c9 	. 

DELAY:
	inr a			;024f	3c 	< 
DELAY$0:
	dcr a			;0250	3d 	= 
	rz			;0251	c8 	. 
	push psw			;0252	f5 	. 
	call l234eh		;0253	cd 4e 23 	. N # 
	xra a			;0256	af 	. 
	nop			;0257	00 	. 
DELAY$1:
	dcr a			;0258	3d 	= 
	jnz DELAY$1		;0259	c2 58 02 	. X . 
	pop psw			;025c	f1 	. 
	jmp DELAY$0		;025d	c3 50 02 	. P . 


BREAKBP:
	lxi sp,029b6h		;0260	31 b6 29 	1 . ) 
	mvi a,0ffh		;0263	3e ff 	> . 
	call INIT		;0265	cd 74 02 	. t . 
	rst 7			;0268	ff 	. 

ENTRY:
	lxi sp,029b6h		;0269	31 b6 29 	1 . ) 
	xra a			;026c	af 	. 
	call INIT		;026d	cd 74 02 	. t . 
	call s1fd9h		;0270	cd d9 1f 	. . . 
	db 0			;0273	00 	. 

INIT:
	sta 0274ch		;0274	32 4c 27 	2 L ' 
	lxi h,00000h		;0277	21 00 00 	. . . 
	dad sp			;027a	39 	9 
	shld 02760h		;027b	22 60 27 	" ` ' 
	lxi d,BANNER		;027e	11 46 01 	. F . 
	call PRINT		;0281	cd 0e 23 	. . # 
	lxi d,COPYR		;0284	11 06 01 	. . . 
	call PRINT		;0287	cd 0e 23 	. . # 
	lhld BIOSJVT		;HL = BIOS Vector Table (WBOOTE)
	lxi d,00030h		;028d	11 30 00 	. 0 . 
	dad d			;0290	19 	. 
	mov a,m			;0291	7e 	~ 
	inx h			;0292	23 	# 
	ora a			;0293	b7 	. 
	jnz l029fh		;0294	c2 9f 02 	. . . 
	mov a,m			;0297	7e 	~ 
	inr a			;0298	3c 	< 
	jz 002b2h		;0299	ca b2 02 	. . . 
	dcx h			;029c	2b 	+ 
	mov a,m			;029d	7e 	~ 
	inx h			;029e	23 	# 
l029fh:
	cpi 002h		;029f	fe 02 	. . 
	jnz l02afh		;02a1	c2 af 02 	. . . 
	mov a,m			;02a4	7e 	~ 
	cpi 064h		;02a5	fe 64 	. d 
	jnc l02afh		;02a7	d2 af 02 	. . . 
	cpi 001h		;02aa	fe 01 	. . 
	jnc 002b2h		;02ac	d2 b2 02 	. . . 
l02afh:
	mvi a,0ffh		;02af	3e ff 	> . 
	cpi 0afh		;02b1	fe af 	. . 
	sta 0274bh		;02b3	32 4b 27 	2 K ' 
	lda NOVIO		;02b6	3a bd 01 	: . . 
	ora a			;02b9	b7 	. 
	jnz l02c7h		;02ba	c2 c7 02 	. . . 
	lxi h,0fffdh		;02bd	21 fd ff 	. . . 
	mov b,m			;02c0	46 	F 
	mvi a,0a9h		;02c1	3e a9 	> . 
	mov m,a			;02c3	77 	w 
	xra m			;02c4	ae 	. 
	mov m,b			;02c5	70 	p 
	inr a			;02c6	3c 	< 
l02c7h:
	lxi h,02749h		;02c7	21 49 27 	. I ' 
	mov m,a			;02ca	77 	w 
	lda 00003h		;02cb	3a 03 00 	: . . 
	ani 003h		;02ce	e6 03 	. . 
	cpi 002h		;02d0	fe 02 	. . 
	jnc l02d7h		;02d2	d2 d7 02 	. . . 
	mvi m,0ffh		;02d5	36 ff 	6 . 
l02d7h:
	call s1901h		;02d7	cd 01 19 	. . . 
	lhld PBEGMEM		;02da	2a b7 01 	* . . 
	xchg			;02dd	eb 	. 
	lxi h,02731h		;02de	21 31 27 	. 1 ' 
	mvi a,004h		;02e1	3e 04 	> . 
l02e3h:
	mov m,e			;02e3	73 	s 
	inx h			;02e4	23 	# 
	mov m,d			;02e5	72 	r 
	inx h			;02e6	23 	# 
	dcr a			;02e7	3d 	= 
	jnz l02e3h		;02e8	c2 e3 02 	. . . 
	xra a			;02eb	af 	. 
	stax d			;02ec	12 	. 
	inx d			;02ed	13 	. 
	xchg			;02ee	eb 	. 
	shld 02739h		;02ef	22 39 27 	" 9 ' 
l02f2h:
	lhld 02739h		;02f2	2a 39 27 	* 9 ' 
	shld 0273bh		;02f5	22 3b 27 	" ; ' 
	shld 0273dh		;02f8	22 3d 27 	" = ' 
	shld 0273fh		;02fb	22 3f 27 	" ? ' 
	shld 02741h		;02fe	22 41 27 	" A ' 
	lxi d,l2680h		;0301	11 80 26 	. . & 
	mvi c,0b1h		;0304	0e b1 	. . 
	xra a			;0306	af 	. 
l0307h:
	stax d			;0307	12 	. 
	inx d			;0308	13 	. 
	dcr c			;0309	0d 	. 
	jnz l0307h		;030a	c2 07 03 	. . . 
	sta 0005bh		;030d	32 5b 00 	2 [ . 
	sta 00068h		;0310	32 68 00 	2 h . 
	sta 0006bh		;0313	32 6b 00 	2 k . 
	sta 0007dh		;0316	32 7d 00 	2 } . 
	lhld FBASE		;0319	2a 06 00 	* . . 
	mov a,l			;031c	7d 	} 
	cpi 006h		;031d	fe 06 	. . 
	jz l0326h		;031f	ca 26 03 	. & . 
	mov a,h			;0322	7c 	| 
	sui 007h		;0323	d6 07 	. . 
	mov h,a			;0325	67 	g 
l0326h:
	dcx h			;0326	2b 	+ 
	shld 02743h		;0327	22 43 27 	" C ' 
	lda 001b8h		;032a	3a b8 01 	: . . 
	adi 00fh		;032d	c6 0f 	. . 
	cmp h			;032f	bc 	. 
	jc l0345h		;0330	da 45 03 	. E . 
	call s1fe7h		;0333	cd e7 1f 	. . . 
	db 'NOT ENOUGH MEM',0
l0345h:
	mov a,h			;0345	7c 	| 
	ora a			;0346	b7 	. 
	rar			;0347	1f 	. 
	rar			;0348	1f 	. 
	rar			;0349	1f 	. 
	inr a			;034a	3c 	< 
	inr a			;034b	3c 	< 
	ani 03ch		;034c	e6 3c 	. < 
	mov b,a			;034e	47 	G 
	sta 0272bh		;034f	32 2b 27 	2 + ' 
	rlc			;0352	07 	. 
	sta 0272eh		;0353	32 2e 27 	2 . ' 
	mov a,b			;0356	78 	x 
	cpi 008h		;0357	fe 08 	. . 
	jnc l035eh		;0359	d2 5e 03 	. ^ . 
	mvi a,008h		;035c	3e 08 	> . 
l035eh:
	sta 02730h		;035e	32 30 27 	2 0 ' 
	mov a,h			;0361	7c 	| 
	cpi 064h		;0362	fe 64 	. d 
	lxi h,l0400h		;0364	21 00 04 	. . . 
	shld 02726h		;0367	22 26 27 	" & ' 
	jc l036eh		;036a	da 6e 03 	. n . 
	dad h			;036d	29 	) 
l036eh:
	shld 02728h		;036e	22 28 27 	" ( ' 
	mvi a,008h		;0371	3e 08 	> . 
	sta 0272ch		;0373	32 2c 27 	2 , ' 
	jc l037ah		;0376	da 7a 03 	. z . 
	add a			;0379	87 	. 
l037ah:
	sta 0272dh		;037a	32 2d 27 	2 - ' 
	lxi h,FCB+FCBFN		;037d	21 5d 00 	. ] . 
l0380h:
	mov a,m			;0380	7e 	~ 
	dcx h			;0381	2b 	+ 
	cpi ' '			;0382	fe 20 	.   
	jnz l0397h		;0384	c2 97 03 	. . . 
	call s1fe7h		;0387	cd e7 1f 	. . . 
	db 'NO FILE NAME',0
l0397h:
	mov a,m			;0397	7e 	~ 
	call s2052h		;0398	cd 52 20 	. R   
	mov m,a			;039b	77 	w 
	lxi d,TMPFCB		;039c	11 ab 26 	. . & 
	push d			;039f	d5 	. 
	lxi b,00010h		;03a0	01 10 00 	. . . 
	call MEMCPY		;03a3	cd 0c 1f 	. . . 
	mov a,m			;03a6	7e 	~ 
	pop h			;03a7	e1 	. 
	ora a			;03a8	b7 	. 
	jz l03b0h		;03a9	ca b0 03 	. . . 
	call s2052h		;03ac	cd 52 20 	. R   
	mov m,a			;03af	77 	w 
l03b0h:
	lda FCB		;03b0	3a 5c 00 	: \ . 
	cmp m			;03b3	be 	. 
	jz l03bch		;03b4	ca bc 03 	. . . 
	mvi a,0ffh		;03b7	3e ff 	> . 
	sta 0271ch		;03b9	32 1c 27 	2 . ' 
l03bch:
	mov a,m			;03bc	7e 	~ 
	lxi d,BAKFCB+FCBFN		;03bd	11 d1 26 	. . & 
	stax d			;03c0	12 	. 
	lxi h,l043bh		;03c1	21 3b 04 	. ; . 
	lxi b,0000bh		;03c4	01 0b 00 	. . . 
	inx d			;03c7	13 	. 
	call MEMCPY		;03c8	cd 0c 1f 	. . . 
	lda 0271ch		;03cb	3a 1c 27 	: . ' 
	ora a			;03ce	b7 	. 
	jz l03fdh		;03cf	ca fd 03 	. . . 
	lxi d,TMPFCB		;03d2	11 ab 26 	. . & 
	call s1ff4h		;03d5	cd f4 1f 	. . . 
	jz l03fdh		;03d8	ca fd 03 	. . . 
	call s1fe7h		;03db	cd e7 1f 	. . . 
	db 'FILE EXISTS ON DESTINATION DSK',0
l03fdh:
	lxi d,FCB		;03fd	11 5c 00 	. \ . 
l0400h:
	call s1ff4h		;0400	cd f4 1f 	. . . 
	mvi a,0ffh		;0403	3e ff 	> . 
	jz l040eh		;0405	ca 0e 04 	. . . 
	sta 0005bh		;0408	32 5b 00 	2 [ . 
	jmp l0421h		;040b	c3 21 04 	. . . 
l040eh:
	sta 0271dh		;040e	32 1d 27 	2 . ' 
	call PRINTSP		;0411	cd 06 23 	. . # 
	db LF,'NEW FILE',CR,LF,LF,0
l0421h:
	lxi d,TMPFCB+FCBEXT	;0421	11 b4 26 	. . & 
	push d			;0424	d5 	. 
	call s1f06h		;0425	cd 06 1f 	. . . 
	lxi d,TMPFCB		;0428	11 ab 26 	. . & 
	call s2028h		;042b	cd 28 20 	. (   
	pop d			;042e	d1 	. 
	lxi h,l043bh+8		;042f	21 43 04 	. C . 
l0432h:
	call s1f09h		;0432	cd 09 1f 	. . . 
	call s1c4dh		;0435	cd 4d 1c 	. M . 
	jmp l04b9h		;0438	c3 b9 04 	. . . 
l043bh:
	db 'WMBACKUP$$$'
l0446h:
	call s0468h		;0446	cd 68 04 	. h . 
	call s2005h		;0449	cd 05 20 	. .   
	lxi h,FCB		;044c	21 5c 00 	. \ . 
	mov a,m			;044f	7e 	~ 
	sta FCB2		;0450	32 6c 00 	2 l . 
l0453h:
	lda TMPFCB		;0453	3a ab 26 	: . & 
	mov m,a			;0456	77 	w 
	jmp l02f2h		;0457	c3 f2 02 	. . . 
l045ah:
	call s0468h		;045a	cd 68 04 	. h . 
	lxi d,BAKFCB+FCBFN		;045d	11 d1 26 	. . & 
l0460h:
	call s2028h		;0460	cd 28 20 	. (   
l0463h:
	lhld 02760h		;0463	2a 60 27 	* ` ' 
	sphl			;0466	f9 	. 
	ret			;0467	c9 	. 
s0468h:
	call s1d51h		;0468	cd 51 1d 	. Q . 
	lxi h,0271dh		;046b	21 1d 27 	. . ' 
	mov a,m			;046e	7e 	~ 
	ora a			;046f	b7 	. 
	lxi d,FCB		;0470	11 5c 00 	. \ . 
	lxi h,l1f53h		;0473	21 53 1f 	. S . 
	cz s206dh		;0476	cc 6d 20 	. m   
	lxi d,TMPFCB		;0479	11 ab 26 	. . & 
	lxi h,00065h		;047c	21 65 00 	. e . 
	jmp s206dh		;047f	c3 6d 20 	. m   
l0482h:
	call s0497h		;0482	cd 97 04 	. . . 
	jnz l04b9h		;0485	c2 b9 04 	. . . 
	call s2005h		;0488	cd 05 20 	. .   
	lxi d,TMPFCB		;048b	11 ab 26 	. . & 
	call s2008h		;048e	cd 08 20 	. .   
	lxi h,FCB2		;0491	21 6c 00 	. l . 
	jmp l0453h		;0494	c3 53 04 	. S . 
s0497h:
	call PRINTSP		;0497	cd 06 23 	. . # 
	db CR,LF,'ABORT (Y/N)? ',0
l04aah:
	call s2230h		;04aa	cd 30 22 	. 0 " 
	call s064ch		;04ad	cd 4c 06 	. L . 
	cpi 'Y'			;04b0	fe 59 	. Y 
	ret			;04b2	c9 	. 
l04b3h:
	call s0497h		;04b3	cd 97 04 	. . . 
	jz l0463h		;04b6	ca 63 04 	. c . 
l04b9h:
	lhld 02760h		;04b9	2a 60 27 	* ` ' 
	sphl			;04bc	f9 	. 
	lxi h,l04b9h		;04bd	21 b9 04 	. . . 
	push h			;04c0	e5 	. 
	call s23fah		;04c1	cd fa 23 	. . # 
	call s0668h		;04c4	cd 68 06 	. h . 
	lxi h,l26a6h+2		;04c7	21 a8 26 	. . & 
	mov a,m			;04ca	7e 	~ 
l04cbh:
	ora a			;04cb	b7 	. 
	jnz l04deh		;04cc	c2 de 04 	. . . 
	dcr m			;04cf	35 	5 
	lxi h,0286ch		;04d0	21 6c 28 	. l ( 
	mvi m,000h		;04d3	36 00 	6 . 
	shld 0274dh		;04d5	22 4d 27 	" M ' 
	call s10f7h		;04d8	cd f7 10 	. . . 
	call s0668h		;04db	cd 68 06 	. h . 
l04deh:
	mvi a,'*'		;04de	3e 2a 	> * 
	call s24fch		;04e0	cd fc 24 	. . $ 
	call s2230h		;04e3	cd 30 22 	. 0 " 
	shld 0274dh		;04e6	22 4d 27 	" M ' 
	mov a,m			;04e9	7e 	~ 
	ora a			;04ea	b7 	. 
	jnz l04f6h		;04eb	c2 f6 04 	. . . 
	lda 0286bh		;04ee	3a 6b 28 	: k ( 
	cpi LF			;04f1	fe 0a 	. . 
	jz l078dh		;04f3	ca 8d 07 	. . . 
l04f6h:
	mov b,a			;04f6	47 	G 
	push h			;04f7	e5 	. 
	inx h			;04f8	23 	# 
	call s22f1h		;04f9	cd f1 22 	. . " 
	pop h			;04fc	e1 	. 
	ora a			;04fd	b7 	. 
	jnz l0519h		;04fe	c2 19 05 	. . . 
	mov a,b			;0501	78 	x 
	call s064ch		;0502	cd 4c 06 	. L . 
	cpi 'H'		;0505	fe 48 	. H 
	jz l0446h		;0507	ca 46 04 	. F . 
	cpi 'Q'		;050a	fe 51 	. Q 
	jz l04b3h		;050c	ca b3 04 	. . . 
	cpi 'O'		;050f	fe 4f 	. O 
	jz l0482h		;0511	ca 82 04 	. . . 
	cpi 'E'		;0514	fe 45 	. E 
	jz l045ah		;0516	ca 5a 04 	. Z . 
l0519h:
	call s0541h		;0519	cd 41 05 	. A . 
	rz			;051c	c8 	. 
l051dh:
	call PRINTSP		;051d	cd 06 23 	. . # 
	db CR,LF,' ???',0
l0527h:
	lhld 0274dh		;0527	2a 4d 27 	* M ' 
	xchg			;052a	eb 	. 
	call PRINT		;052b	cd 0e 23 	. . # 
	call s24e3h		;052e	cd e3 24 	. . $ 
	jmp l04b9h		;0531	c3 b9 04 	. . . 
s0534h:
	call s1209h		;0534	cd 09 12 	. . . 
	call s24e3h		;0537	cd e3 24 	. . $ 
	pop d			;053a	d1 	. 
	call PRINT		;053b	cd 0e 23 	. . # 
	jmp l051dh		;053e	c3 1d 05 	. . . 
s0541h:
	push b			;0541	c5 	. 
	push d			;0542	d5 	. 
	push h			;0543	e5 	. 
	shld 0274fh		;0544	22 4f 27 	" O ' 
	call s0668h		;0547	cd 68 06 	. h . 
	lxi h,l0636h		;054a	21 36 06 	. 6 . 
	push h			;054d	e5 	. 
l054eh:
	lhld 0274fh		;054e	2a 4f 27 	* O ' 
	call s0806h		;0551	cd 06 08 	. . . 
	call s22f1h		;0554	cd f1 22 	. . " 
	shld 0274dh		;0557	22 4d 27 	" M ' 
	push h			;055a	e5 	. 
	call s0649h		;055b	cd 49 06 	. I . 
	shld 0274fh		;055e	22 4f 27 	" O ' 
	pop h			;0561	e1 	. 
	ora a			;0562	b7 	. 
	jz l0627h		;0563	ca 27 06 	. ' . 
	cpi '>'		;0566	fe 3e 	. > 
	jz l0627h		;0568	ca 27 06 	. ' . 
	cpi ';'		;056b	fe 3b 	. ; 
	jz l067ch		;056d	ca 7c 06 	. | . 
	cpi 011h		;0570	fe 11 	. . 
	jz l1954h		;0572	ca 54 19 	. T . 
	cpi 004h		;0575	fe 04 	. . 
	jz l062ch		;0577	ca 2c 06 	. , . 
	cpi 'V'		;057a	fe 56 	. V 
	jz s10f7h		;057c	ca f7 10 	. . . 
	call s21b4h		;057f	cd b4 21 	. . ! 
	call s22f1h		;0582	cd f1 22 	. . " 
	shld 02754h		;0585	22 54 27 	" T ' 
l0588h:
	lhld 02754h		;0588	2a 54 27 	* T ' 
	call s0649h		;058b	cd 49 06 	. I . 
	shld 02752h		;058e	22 52 27 	" R ' 
	shld 0274fh		;0591	22 4f 27 	" O ' 
	ora a			;0594	b7 	. 
	jz l0790h		;0595	ca 90 07 	. . . 
	cpi 'C'		;0598	fe 43 	. C 
	jz l072ch		;059a	ca 2c 07 	. , . 
	cpi 'D'		;059d	fe 44 	. D 
	jz l0732h		;059f	ca 32 07 	. 2 . 
	cpi 'K'		;05a2	fe 4b 	. K 
	jz l0778h		;05a4	ca 78 07 	. x . 
	cpi 'L'		;05a7	fe 4c 	. L 
	jz l0754h		;05a9	ca 54 07 	. T . 
	cpi 'P'		;05ac	fe 50 	. P 
	jz l07cdh		;05ae	ca cd 07 	. . . 
	cpi 'T'		;05b1	fe 54 	. T 
	jz l079ah		;05b3	ca 9a 07 	. . . 
	push h			;05b6	e5 	. 
	lhld 02758h		;05b7	2a 58 27 	* X ' 
	xchg			;05ba	eb 	. 
	lxi h,00002h		;05bb	21 02 00 	. . . 
	call CMPDEHL		;05be	cd 9f 1f 	. . . 
	pop h			;05c1	e1 	. 
	jnc l05cah		;05c2	d2 ca 05 	. . . 
	cpi 'B'		;05c5	fe 42 	. B 
	jz l0747h		;05c7	ca 47 07 	. G . 
l05cah:
	cpi '/'		;05ca	fe 2f 	. / 
	jnz l05ddh		;05cc	c2 dd 05 	. . . 
	mvi a,0ffh		;05cf	3e ff 	> . 
	sta 0275eh		;05d1	32 5e 27 	2 ^ ' 
	call s0649h		;05d4	cd 49 06 	. I . 
	shld 0274fh		;05d7	22 4f 27 	" O ' 
	shld 02752h		;05da	22 52 27 	" R ' 
l05ddh:
	cpi 'F'		;05dd	fe 46 	. F 
	jz l0835h		;05df	ca 35 08 	. 5 . 
	cpi 'N'		;05e2	fe 4e 	. N 
	jz l083bh		;05e4	ca 3b 08 	. ; . 
	cpi 'R'		;05e7	fe 52 	. R 
	jz l0847h		;05e9	ca 47 08 	. G . 
	cpi 'S'		;05ec	fe 53 	. S 
	jz l0841h		;05ee	ca 41 08 	. A . 
	call s0655h		;05f1	cd 55 06 	. U . 
	cpi 'Q'		;05f4	fe 51 	. Q 
	jz l06abh		;05f6	ca ab 06 	. . . 
	call s065fh		;05f9	cd 5f 06 	. _ . 
	jnz l0627h		;05fc	c2 27 06 	. ' . 
	cpi 'A'		;05ff	fe 41 	. A 
	jz l08d4h		;0601	ca d4 08 	. . . 
	cpi 'I'		;0604	fe 49 	. I 
	jz l08d7h		;0606	ca d7 08 	. . . 
	cpi 'Z'		;0609	fe 5a 	. Z 
	jz l080fh		;060b	ca 0f 08 	. . . 
	cpi '<'		;060e	fe 3c 	. < 
	jz l0689h		;0610	ca 89 06 	. . . 
	cpi 'M'		;0613	fe 4d 	. M 
	jz l0689h		;0615	ca 89 06 	. . . 
	cpi '!'		;0618	fe 21 	. . 
	jz l08f4h		;061a	ca f4 08 	. . . 
	cpi 'W'		;061d	fe 57 	. W 
	jz l1a20h		;061f	ca 20 1a 	.   . 
	cpi 'Y'		;0622	fe 59 	. Y 
	jz l19dfh		;0624	ca df 19 	. . . 
l0627h:
	pop h			;0627	e1 	. 
	ora a			;0628	b7 	. 
	jmp POPHDB		;0629	c3 cc 1f 	. . . 
l062ch:
	lda 0274ch		;062c	3a 4c 27 	: L ' 
	ora a			;062f	b7 	. 
	jz l0627h		;0630	ca 27 06 	. ' . 
	jmp RESTART		;0633	c3 38 00 	. 8 . 
l0636h:
	dcx sp			;0636	3b 	; 
	dcx sp			;0637	3b 	; 
	call s0668h		;0638	cd 68 06 	. h . 
	call s0828h		;063b	cd 28 08 	. ( . 
	jnz l0588h		;063e	c2 88 05 	. . . 
	jmp l054eh		;0641	c3 4e 05 	. N . 
s0644h:
	mov a,m			;0644	7e 	~ 
	ora a			;0645	b7 	. 
	rz			;0646	c8 	. 
	inx h			;0647	23 	# 
	ret			;0648	c9 	. 
s0649h:
	call s0644h		;0649	cd 44 06 	. D . 
s064ch:
	cpi 'a'		;064c	fe 61 	. a 
	rc			;064e	d8 	. 
	cpi '{'		;064f	fe 7b 	. { 
	rnc 			;0651	d0 	. 
	ani 0dfh		;0652	e6 df 	. . 
	ret			;0654	c9 	. 
s0655h:
	push psw			;0655	f5 	. 
	lda 0275ch		;0656	3a 5c 27 	: \ ' 
	ora a			;0659	b7 	. 
	jm l051dh		;065a	fa 1d 05 	. . . 
	pop psw			;065d	f1 	. 
	ret			;065e	c9 	. 
s065fh:
	push b			;065f	c5 	. 
	mov b,a			;0660	47 	G 
	lda 0275eh		;0661	3a 5e 27 	: ^ ' 
	ora a			;0664	b7 	. 
	mov a,b			;0665	78 	x 
	pop b			;0666	c1 	. 
	ret			;0667	c9 	. 
s0668h:
	lxi h,00200h		;0668	21 00 02 	. . . 
	shld 02745h		;066b	22 45 27 	" E ' 
	shld 02747h		;066e	22 47 27 	" G ' 
	lxi h,00000h		;0671	21 00 00 	. . . 
	shld 0275eh		;0674	22 5e 27 	" ^ ' 
	dcr l			;0677	2d 	- 
	shld l26a4h+1		;0678	22 a5 26 	" . & 
	ret			;067b	c9 	. 
l067ch:
	shld 0274fh		;067c	22 4f 27 	" O ' 
	call s0644h		;067f	cd 44 06 	. D . 
	rz			;0682	c8 	. 
	cpi CR		;0683	fe 0d 	. . 
	rz			;0685	c8 	. 
	jmp l067ch		;0686	c3 7c 06 	. | . 
l0689h:
	xchg			;0689	eb 	. 
	call s0719h		;068a	cd 19 07 	. . . 
	lhld 0275ah		;068d	2a 5a 27 	* Z ' 
	xchg			;0690	eb 	. 
l0691h:
	call s0541h		;0691	cd 41 05 	. A . 
	rc			;0694	d8 	. 
	cpi '>'		;0695	fe 3e 	. > 
	jz l069eh		;0697	ca 9e 06 	. . . 
	ora a			;069a	b7 	. 
	jnz l051dh		;069b	c2 1d 05 	. . . 
l069eh:
	mov a,d			;069e	7a 	z 
	ora e			;069f	b3 	. 
	rz			;06a0	c8 	. 
	dcx d			;06a1	1b 	. 
	mov a,d			;06a2	7a 	z 
	ora e			;06a3	b3 	. 
	rz			;06a4	c8 	. 
	call s23d6h		;06a5	cd d6 23 	. . # 
	jmp l0691h		;06a8	c3 91 06 	. . . 
l06abh:
	call s0649h		;06ab	cd 49 06 	. I . 
	shld 0274fh		;06ae	22 4f 27 	" O ' 
	shld 02752h		;06b1	22 52 27 	" R ' 
	cpi 'P'		;06b4	fe 50 	. P 
	jz l0a5bh		;06b6	ca 5b 0a 	. [ . 
	cpi 'X'		;06b9	fe 58 	. X 
	jz l06dah		;06bb	ca da 06 	. . . 
	push psw			;06be	f5 	. 
	call s081ah		;06bf	cd 1a 08 	. . . 
	pop psw			;06c2	f1 	. 
	cpi 'L'		;06c3	fe 4c 	. L 
	jz l0a2eh		;06c5	ca 2e 0a 	. . . 
	cpi 'T'		;06c8	fe 54 	. T 
	jz l0a3ah		;06ca	ca 3a 0a 	. : . 
	cpi 'K'		;06cd	fe 4b 	. K 
	jz l0a47h		;06cf	ca 47 0a 	. G . 
	cpi 'G'		;06d2	fe 47 	. G 
	jz l0a4eh		;06d4	ca 4e 0a 	. N . 
	jmp l051dh		;06d7	c3 1d 05 	. . . 
l06dah:
	call s0719h		;06da	cd 19 07 	. . . 
	lhld 0274fh		;06dd	2a 4f 27 	* O ' 
	push h			;06e0	e5 	. 
	lhld 02754h		;06e1	2a 54 27 	* T ' 
	push h			;06e4	e5 	. 
l06e5h:
	lhld 02758h		;06e5	2a 58 27 	* X ' 
	push h			;06e8	e5 	. 
	call s1eb9h		;06e9	cd b9 1e 	. . . 
	lhld 02733h		;06ec	2a 33 27 	* 3 ' 
	call s0541h		;06ef	cd 41 05 	. A . 
	jc l06f8h		;06f2	da f8 06 	. . . 
	jnz l070eh		;06f5	c2 0e 07 	. . . 
l06f8h:
	pop h			;06f8	e1 	. 
	shld 02758h		;06f9	22 58 27 	" X ' 
	cc s0806h		;06fc	dc 06 08 	. . . 
	call s081ah		;06ff	cd 1a 08 	. . . 
	jnz l06e5h		;0702	c2 e5 06 	. . . 
	pop h			;0705	e1 	. 
	shld 02754h		;0706	22 54 27 	" T ' 
	pop h			;0709	e1 	. 
	shld 0274fh		;070a	22 4f 27 	" O ' 
	ret			;070d	c9 	. 
l070eh:
	call PRINTSP		;070e	cd 06 23 	. . # 
	db ' QX?',0
	jmp l0527h		;0716	c3 27 05 	. ' . 
s0719h:
	lxi h,0d6c4h		;0719	21 c4 d6 	. . . 
	dad sp			;071c	39 	9 
	rc			;071d	d8 	. 
	call s0534h		;071e	cd 34 05 	. 4 . 
	db 'STACK OVFL',0
l072ch:
	call s0da0h		;072c	cd a0 0d 	. . . 
	jmp l1a87h		;072f	c3 87 1a 	. . . 
l0732h:
	call s0da0h		;0732	cd a0 0d 	. . . 
l0735h:
	jm l073eh		;0735	fa 3e 07 	. > . 
	shld 0273fh		;0738	22 3f 27 	" ? ' 
	jmp l0741h		;073b	c3 41 07 	. A . 
l073eh:
	shld 0273dh		;073e	22 3d 27 	" = ' 
l0741h:
	call s1d9bh		;0741	cd 9b 1d 	. . . 
	jmp l1a8dh		;0744	c3 8d 1a 	. . . 
l0747h:
	lda 0275ch		;0747	3a 5c 27 	: \ ' 
	cma			;074a	2f 	/ 
s074bh:
	lxi h,0ffffh		;074b	21 ff ff 	. . . 
l074eh:
	sta 0275ch		;074e	32 5c 27 	2 \ ' 
s0751h:
	shld 02758h		;0751	22 58 27 	" X ' 
l0754h:
	call s0e04h		;0754	cd 04 0e 	. . . 
	call l1a87h		;0757	cd 87 1a 	. . . 
	call s0828h		;075a	cd 28 08 	. ( . 
	jnz l0754h		;075d	c2 54 07 	. T . 
	ret			;0760	c9 	. 
s0761h:
	lxi d,BIOSJVT		;DE = BIOS Vector Table (WBOOTE)
l0764h:
	lhld 02758h		;0764	2a 58 27 	* X ' 
	push h			;0767	e5 	. 
	xchg			;0768	eb 	. 
	call s0751h		;0769	cd 51 07 	. Q . 
	pop h			;076c	e1 	. 
	shld 02758h		;076d	22 58 27 	" X ' 
	ret			;0770	c9 	. 
s0771h:
	mvi a,0ffh		;0771	3e ff 	> . 
	cpi 0afh		;0773	fe af 	. . 
	jmp l074eh		;0775	c3 4e 07 	. N . 
l0778h:
	call s0e04h		;0778	cd 04 0e 	. . . 
	jmp l0735h		;077b	c3 35 07 	. 5 . 
s077eh:
	lxi h,00001h		;077e	21 01 00 	. . . 
s0781h:
	xra a			;0781	af 	. 
s0782h:
	sta 0275ch		;0782	32 5c 27 	2 \ ' 
	xra a			;0785	af 	. 
	sta 0271eh		;0786	32 1e 27 	2 . ' 
	shld 02758h		;0789	22 58 27 	" X ' 
	ret			;078c	c9 	. 
l078dh:
	call s077eh		;078d	cd 7e 07 	. ~ . 
l0790h:
	call l0754h		;0790	cd 54 07 	. T . 
	lxi h,00001h		;0793	21 01 00 	. . . 
s0796h:
	xra a			;0796	af 	. 
	call s0782h		;0797	cd 82 07 	. . . 
l079ah:
	call s0de8h		;079a	cd e8 0d 	. . . 
	xchg			;079d	eb 	. 
	jm l07b3h		;079e	fa b3 07 	. . . 
	lhld 0273fh		;07a1	2a 3f 27 	* ? ' 
	xchg			;07a4	eb 	. 
	jnc l07bbh		;07a5	d2 bb 07 	. . . 
	call l07bbh		;07a8	cd bb 07 	. . . 
	mvi a,'}'		;07ab	3e 7d 	> } 
s07adh:
	call s24fch		;07ad	cd fc 24 	. . $ 
	jmp s24fch		;07b0	c3 fc 24 	. . $ 
l07b3h:
	lhld 0273dh		;07b3	2a 3d 27 	* = ' 
	mvi a,'{'		;07b6	3e 7b 	> { 
	cc s07adh		;07b8	dc ad 07 	. . . 
l07bbh:
	call s0806h		;07bb	cd 06 08 	. . . 
l07beh:
	call CMPDEHL		;07be	cd 9f 1f 	. . . 
	rz			;07c1	c8 	. 
	ldax d			;07c2	1a 	. 
	call s24fch		;07c3	cd fc 24 	. . $ 
	inx d			;07c6	13 	. 
	call s23d6h		;07c7	cd d6 23 	. . # 
	jmp l07beh		;07ca	c3 be 07 	. . . 
l07cdh:
	lhld 02758h		;07cd	2a 58 27 	* X ' 
	push h			;07d0	e5 	. 
	lhld 0275ch		;07d1	2a 5c 27 	* \ ' 
	push h			;07d4	e5 	. 
	call s0828h		;07d5	cd 28 08 	. ( . 
	lhld HITE		;07d8	2a b9 01 	* . . 
	mvi h,000h		;07db	26 00 	& . 
	dcx h			;07dd	2b 	+ 
	push h			;07de	e5 	. 
	cnz s0751h		;07df	c4 51 07 	. Q . 
	pop h			;07e2	e1 	. 
	call s0796h		;07e3	cd 96 07 	. . . 
	pop h			;07e6	e1 	. 
	shld 0275ch		;07e7	22 5c 27 	" \ ' 
	pop h			;07ea	e1 	. 
	shld 02758h		;07eb	22 58 27 	" X ' 
	call s081ah		;07ee	cd 1a 08 	. . . 
	call s1cb5h		;07f1	cd b5 1c 	. . . 
	rnz			;07f4	c0 	. 
	lda 0275ch		;07f5	3a 5c 27 	: \ ' 
	ora a			;07f8	b7 	. 
	lxi h,0273fh		;07f9	21 3f 27 	. ? ' 
	jp l0802h		;07fc	f2 02 08 	. . . 
	lxi h,0273bh		;07ff	21 3b 27 	. ; ' 
l0802h:
	call s1efbh		;0802	cd fb 1e 	. . . 
	rnz			;0805	c0 	. 
s0806h:
	push h			;0806	e5 	. 
	lxi h,00000h		;0807	21 00 00 	. . . 
	call s0781h		;080a	cd 81 07 	. . . 
	pop h			;080d	e1 	. 
	ret			;080e	c9 	. 
l080fh:
	lxi h,0a000h		;080f	21 00 a0 	. . . 
l0812h:
	xthl			;0812	e3 	. 
	xthl			;0813	e3 	. 
	dcx h			;0814	2b 	+ 
	mov a,h			;0815	7c 	| 
	ora l			;0816	b5 	. 
	jnz l0812h		;0817	c2 12 08 	. . . 
s081ah:
	push h			;081a	e5 	. 
	lhld 02758h		;081b	2a 58 27 	* X ' 
	mov a,h			;081e	7c 	| 
	ora l			;081f	b5 	. 
	jz l0824h		;0820	ca 24 08 	. $ . 
	dcx h			;0823	2b 	+ 
l0824h:
	shld 02758h		;0824	22 58 27 	" X ' 
	pop h			;0827	e1 	. 
s0828h:
	push h			;0828	e5 	. 
	lhld 02758h		;0829	2a 58 27 	* X ' 
	lda 0271eh		;082c	3a 1e 27 	: . ' 
	ora h			;082f	b4 	. 
	ora l			;0830	b5 	. 
	pop h			;0831	e1 	. 
	jmp s23d6h		;0832	c3 d6 23 	. . # 
l0835h:
	call s0cb5h		;0835	cd b5 0c 	. . . 
	jmp l084ah		;0838	c3 4a 08 	. J . 
l083bh:
	call s0c96h		;083b	cd 96 0c 	. . . 
	jmp l084ah		;083e	c3 4a 08 	. J . 
l0841h:
	call s0cebh		;0841	cd eb 0c 	. . . 
	jmp l084ah		;0844	c3 4a 08 	. J . 
l0847h:
	call s0ca8h		;0847	cd a8 0c 	. . . 
l084ah:
	jnc s081ah		;084a	d2 1a 08 	. . . 
	call s065fh		;084d	cd 5f 06 	. _ . 
	jnz l085eh		;0850	c2 5e 08 	. ^ . 
	call PRINTSP		;0853	cd 06 23 	. . # 
	dw 02320h		;0856	20 23 	  # 
	inx h			;0858	23 	# 
	dw 00020h		;0859	20 00 	  . 
l085bh:
	jmp l0527h		;085b	c3 27 05 	. ' . 
l085eh:
	call s0870h		;085e	cd 70 08 	. p . 
	call s0806h		;0861	cd 06 08 	. . . 
	pop h			;0864	e1 	. 
	ora a			;0865	b7 	. 
	stc			;0866	37 	7 
	jmp POPHDB		;0867	c3 cc 1f 	. . . 
l086ah:
	call s0719h		;086a	cd 19 07 	. . . 
	call s0870h		;086d	cd 70 08 	. p . 
s0870h:
	lhld 0274fh		;0870	2a 4f 27 	* O ' 
	call s0649h		;0873	cd 49 06 	. I . 
	shld 0274fh		;0876	22 4f 27 	" O ' 
	cpi '<'		;0879	fe 3c 	. < 
l087bh:
	jz l086ah		;087b	ca 6a 08 	. j . 
	cpi 'M'		;087e	fe 4d 	. M 
	jz l086ah		;0880	ca 6a 08 	. j . 
	cpi 'A'		;0883	fe 41 	. A 
	jz l08ceh		;0885	ca ce 08 	. . . 
	cpi 'F'		;0888	fe 46 	. F 
	jz l08ceh		;088a	ca ce 08 	. . . 
	cpi 'I'		;088d	fe 49 	. I 
	jz l08ceh		;088f	ca ce 08 	. . . 
	cpi 'N'		;0892	fe 4e 	. N 
	jz l08ceh		;0894	ca ce 08 	. . . 
	cpi 'R'		;0897	fe 52 	. R 
	jz l08cbh		;0899	ca cb 08 	. . . 
	cpi 'S'		;089c	fe 53 	. S 
	jz l08cbh		;089e	ca cb 08 	. . . 
	cpi 'W'		;08a1	fe 57 	. W 
	jz l08ceh		;08a3	ca ce 08 	. . . 
	cpi 'Y'		;08a6	fe 59 	. Y 
	jz l08ceh		;08a8	ca ce 08 	. . . 
	cpi ';'		;08ab	fe 3b 	. ; 
	cz l067ch		;08ad	cc 7c 06 	. | . 
	jz s0870h		;08b0	ca 70 08 	. p . 
	cpi 'Q'		;08b3	fe 51 	. Q 
	jnz l08c3h		;08b5	c2 c3 08 	. . . 
	call s0649h		;08b8	cd 49 06 	. I . 
	shld 0274fh		;08bb	22 4f 27 	" O ' 
	cpi 'L'		;08be	fe 4c 	. L 
	jz l08ceh		;08c0	ca ce 08 	. . . 
l08c3h:
	cpi '>'		;08c3	fe 3e 	. > 
	rz			;08c5	c8 	. 
	ora a			;08c6	b7 	. 
	rz			;08c7	c8 	. 
	jmp s0870h		;08c8	c3 70 08 	. p . 
l08cbh:
	call s091ah		;08cb	cd 1a 09 	. . . 
l08ceh:
	call s091ah		;08ce	cd 1a 09 	. . . 
	jmp s0870h		;08d1	c3 70 08 	. p . 
l08d4h:
	call s0761h		;08d4	cd 61 07 	. a . 
l08d7h:
	lhld 02752h		;08d7	2a 52 27 	* R ' 
	mov a,m			;08da	7e 	~ 
	ora a			;08db	b7 	. 
	jz l0aceh		;08dc	ca ce 0a 	. . . 
	call s0913h		;08df	cd 13 09 	. . . 
	call s0c1bh		;08e2	cd 1b 0c 	. . . 
	lda 02762h		;08e5	3a 62 27 	: b ' 
	ora a			;08e8	b7 	. 
	lxi d,CRLF		;08e9	11 43 01 	. C . 
	mvi a,002h		;08ec	3e 02 	> . 
	cz s0c23h		;08ee	cc 23 0c 	. # . 
	jmp s081ah		;08f1	c3 1a 08 	. . . 
l08f4h:
	lda 02758h		;08f4	3a 58 27 	: X ' 
	push psw			;08f7	f5 	. 
	call s0806h		;08f8	cd 06 08 	. . . 
	pop psw			;08fb	f1 	. 
	ani 07fh		;08fc	e6 7f 	.  
	call s0bach		;08fe	cd ac 0b 	. . . 
	cpi EOF		;0901	fe 1a 	. . 
	rnz			;0903	c0 	. 
s0904h:
	call PRINTSP		;0904	cd 06 23 	. . # 
	db ' TURKEY ',CR,LF,0
	ret			;0912	c9 	. 
s0913h:
	push h			;0913	e5 	. 
	lhld 02752h		;0914	2a 52 27 	* R ' 
	jmp l091eh		;0917	c3 1e 09 	. . . 
s091ah:
	push h			;091a	e5 	. 
	lhld 0274fh		;091b	2a 4f 27 	* O ' 
l091eh:
	push b			;091e	c5 	. 
	push psw			;091f	f5 	. 
	lxi d,02763h		;0920	11 63 27 	. c ' 
	push d			;0923	d5 	. 
	mvi c,0ffh		;0924	0e ff 	. . 
l0926h:
	call s0644h		;0926	cd 44 06 	. D . 
	cpi 00eh		;0929	fe 0e 	. . 
	jnz l093bh		;092b	c2 3b 09 	. ; . 
	mvi a,CR		;092e	3e 0d 	> . 
	inr c			;0930	0c 	. 
	inx d			;0931	13 	. 
	stax d			;0932	12 	. 
	mvi a,LF		;0933	3e 0a 	> . 
l0935h:
	inr c			;0935	0c 	. 
	inx d			;0936	13 	. 
	stax d			;0937	12 	. 
	jmp l0926h		;0938	c3 26 09 	. & . 
l093bh:
	cpi 019h		;093b	fe 19 	. . 
	jnz l0945h		;093d	c2 45 09 	. E . 
	mvi a,ESC		;0940	3e 1b 	> . 
	jmp l0935h		;0942	c3 35 09 	. 5 . 
l0945h:
	call s095bh		;0945	cd 5b 09 	. [ . 
	jnz l0926h		;0948	c2 26 09 	. & . 
	sta 02762h		;094b	32 62 27 	2 b ' 
	shld 0274fh		;094e	22 4f 27 	" O ' 
	xra a			;0951	af 	. 
	stax d			;0952	12 	. 
	pop d			;0953	d1 	. 
	mov a,c			;0954	79 	y 
	stax d			;0955	12 	. 
	inx d			;0956	13 	. 
	pop psw			;0957	f1 	. 
	pop b			;0958	c1 	. 
	pop h			;0959	e1 	. 
	ret			;095a	c9 	. 
s095bh:
	inr c			;095b	0c 	. 
	inx d			;095c	13 	. 
l095dh:
	stax d			;095d	12 	. 
s095eh:
	cpi EOF		;095e	fe 1a 	. . 
	rz			;0960	c8 	. 
	cpi ESC		;0961	fe 1b 	. . 
	rz			;0963	c8 	. 
	ora a			;0964	b7 	. 
	ret			;0965	c9 	. 
s0966h:
	push b			;0966	c5 	. 
	push d			;0967	d5 	. 
	push h			;0968	e5 	. 
	push d			;0969	d5 	. 
	xra a			;096a	af 	. 
	mvi c,010h		;096b	0e 10 	. . 
l096dh:
	stax d			;096d	12 	. 
	inx d			;096e	13 	. 
	dcr c			;096f	0d 	. 
	jnz l096dh		;0970	c2 6d 09 	. m . 
	pop d			;0973	d1 	. 
	push d			;0974	d5 	. 
	lhld 0274fh		;0975	2a 4f 27 	* O ' 
	call s22f1h		;0978	cd f1 22 	. . " 
	lda 0274bh		;097b	3a 4b 27 	: K ' 
	ora a			;097e	b7 	. 
	jnz l099ah		;097f	c2 9a 09 	. . . 
	push d			;0982	d5 	. 
	shld 0274fh		;0983	22 4f 27 	" O ' 
	lxi b,0019fh		;0986	01 9f 01 	. . . 
	lxi d,0274fh		;0989	11 4f 27 	. O ' 
	call BDOS		;098c	cd 05 00 	. . . 
	lhld 0274fh		;098f	2a 4f 27 	* O ' 
	pop d			;0992	d1 	. 
	jm l09ach		;0993	fa ac 09 	. . . 
	stax d			;0996	12 	. 
	jmp l09ach		;0997	c3 ac 09 	. . . 
l099ah:
	inx h			;099a	23 	# 
	mov a,m			;099b	7e 	~ 
	dcx h			;099c	2b 	+ 
	cpi ':'		;099d	fe 3a 	. : 
	jnz l09ach		;099f	c2 ac 09 	. . . 
	call s0a12h		;09a2	cd 12 0a 	. . . 
	jz l09ach		;09a5	ca ac 09 	. . . 
	sui 040h		;09a8	d6 40 	. @ 
	stax d			;09aa	12 	. 
	inx h			;09ab	23 	# 
l09ach:
	inx d			;09ac	13 	. 
	mvi c,008h		;09ad	0e 08 	. . 
	call s09f4h		;09af	cd f4 09 	. . . 
	cpi '.'		;09b2	fe 2e 	. . 
	push psw			;09b4	f5 	. 
	push h			;09b5	e5 	. 
	lxi h,l09f0h		;09b6	21 f0 09 	. . . 
	cnz s1f09h		;09b9	c4 09 1f 	. . . 
	pop h			;09bc	e1 	. 
	pop psw			;09bd	f1 	. 
	mvi c,003h		;09be	0e 03 	. . 
	cz s09f3h		;09c0	cc f3 09 	. . . 
	pop d			;09c3	d1 	. 
	inx d			;09c4	13 	. 
	ldax d			;09c5	1a 	. 
	dcx d			;09c6	1b 	. 
	cpi ' '		;09c7	fe 20 	.   
	jz l09dbh		;09c9	ca db 09 	. . . 
	call s22f1h		;09cc	cd f1 22 	. . " 
	call s0644h		;09cf	cd 44 06 	. D . 
	shld 0274fh		;09d2	22 4f 27 	" O ' 
	call s095eh		;09d5	cd 5e 09 	. ^ . 
	jz POPHDB		;09d8	ca cc 1f 	. . . 
l09dbh:
	call s0534h		;09db	cd 34 05 	. 4 . 
	db 'INVALID FILE NAME',0
l09f0h:
	db 'LIB'
s09f3h:
	inx h			;09f3	23 	# 
s09f4h:
	call s0a12h		;09f4	cd 12 0a 	. . . 
	jz l0a07h		;09f7	ca 07 0a 	. . . 
	stax d			;09fa	12 	. 
	inx d			;09fb	13 	. 
	dcr c			;09fc	0d 	. 
	jnz s09f4h		;09fd	c2 f4 09 	. . . 
l0a00h:
	call s0a12h		;0a00	cd 12 0a 	. . . 
	jnz l0a00h		;0a03	c2 00 0a 	. . . 
	ret			;0a06	c9 	. 
l0a07h:
	push psw			;0a07	f5 	. 
l0a08h:
	mvi a,' '		;0a08	3e 20 	>   
	stax d			;0a0a	12 	. 
	inx d			;0a0b	13 	. 
	dcr c			;0a0c	0d 	. 
	jnz l0a08h		;0a0d	c2 08 0a 	. . . 
	pop psw			;0a10	f1 	. 
	ret			;0a11	c9 	. 
s0a12h:
	mov a,m			;0a12	7e 	~ 
	call s0a1dh		;0a13	cd 1d 0a 	. . . 
	rz			;0a16	c8 	. 
	inx h			;0a17	23 	# 
	call s064ch		;0a18	cd 4c 06 	. L . 
	ora a			;0a1b	b7 	. 
	ret			;0a1c	c9 	. 
s0a1dh:
	cpi '.'		;0a1d	fe 2e 	. . 
	rz			;0a1f	c8 	. 
	cpi ':'		;0a20	fe 3a 	. : 
	rz			;0a22	c8 	. 
	cpi '*'		;0a23	fe 2a 	. * 
	rz			;0a25	c8 	. 
	cpi '?'		;0a26	fe 3f 	. ? 
	rz			;0a28	c8 	. 
	cpi ' '		;0a29	fe 20 	.   
	rnc 			;0a2b	d0 	. 
	cmp a			;0a2c	bf 	. 
l0a2dh:
	ret			;0a2d	c9 	. 
l0a2eh:
	call s0a43h		;0a2e	cd 43 0a 	. C . 
	call s0913h		;0a31	cd 13 09 	. . . 
	lxi h,02735h		;0a34	21 35 27 	. 5 ' 
	jmp l0c1eh		;0a37	c3 1e 0c 	. . . 
l0a3ah:
	lxi h,02733h		;0a3a	21 33 27 	. 3 ' 
	call s1f91h		;0a3d	cd 91 1f 	. . . 
	jmp l07beh		;0a40	c3 be 07 	. . . 
s0a43h:
	call s065fh		;0a43	cd 5f 06 	. _ . 
	rnz			;0a46	c0 	. 
l0a47h:
	lhld 02733h		;0a47	2a 33 27 	* 3 ' 
	shld 02735h		;0a4a	22 35 27 	" 5 ' 
	ret			;0a4d	c9 	. 
l0a4eh:
	lxi h,02733h		;0a4e	21 33 27 	. 3 ' 
	call s1efbh		;0a51	cd fb 1e 	. . . 
	call s1f95h		;0a54	cd 95 1f 	. . . 
	xchg			;0a57	eb 	. 
	jmp l0c29h		;0a58	c3 29 0c 	. ) . 
l0a5bh:
	call s0a43h		;0a5b	cd 43 0a 	. C . 
l0a5eh:
	lxi h,02735h		;0a5e	21 35 27 	. 5 ' 
	call s0c68h		;0a61	cd 68 0c 	. h . 
	call s0e04h		;0a64	cd 04 0e 	. . . 
	xchg			;0a67	eb 	. 
	lhld 0273fh		;0a68	2a 3f 27 	* ? ' 
	xchg			;0a6b	eb 	. 
	call s1f85h		;0a6c	cd 85 1f 	. . . 
	lxi h,02735h		;0a6f	21 35 27 	. 5 ' 
	call s0a82h		;0a72	cd 82 0a 	. . . 
	call s1eb9h		;0a75	cd b9 1e 	. . . 
	call l1a8dh		;0a78	cd 8d 1a 	. . . 
	call s0828h		;0a7b	cd 28 08 	. ( . 
	jnz l0a5eh		;0a7e	c2 5e 0a 	. ^ . 
	ret			;0a81	c9 	. 
s0a82h:
	call s0c8dh		;0a82	cd 8d 0c 	. . . 
	rz			;0a85	c8 	. 
l0a86h:
	call s0c68h		;0a86	cd 68 0c 	. h . 
	call s0aa1h		;0a89	cd a1 0a 	. . . 
	xchg			;0a8c	eb 	. 
	lhld 0273fh		;0a8d	2a 3f 27 	* ? ' 
	xchg			;0a90	eb 	. 
	call s0c3fh		;0a91	cd 3f 0c 	. ? . 
	xchg			;0a94	eb 	. 
	shld 0273fh		;0a95	22 3f 27 	" ? ' 
	xchg			;0a98	eb 	. 
	call s1d9bh		;0a99	cd 9b 1d 	. . . 
	dcr b			;0a9c	05 	. 
	jnz l0a86h		;0a9d	c2 86 0a 	. . . 
	ret			;0aa0	c9 	. 
s0aa1h:
	push b			;0aa1	c5 	. 
	push d			;0aa2	d5 	. 
	push h			;0aa3	e5 	. 
	lxi h,02733h		;0aa4	21 33 27 	. 3 ' 
	call s1efbh		;0aa7	cd fb 1e 	. . . 
	lhld 02731h		;0aaa	2a 31 27 	* 1 ' 
	xchg			;0aad	eb 	. 
	lhld 02743h		;0aae	2a 43 27 	* C ' 
	call s1f7ch		;0ab1	cd 7c 1f 	. | . 
	call s1f71h		;0ab4	cd 71 1f 	. q . 
	dad b			;0ab7	09 	. 
	lxi d,00d80h		;0ab8	11 80 0d 	. . . 
	call CMPDEHL		;0abb	cd 9f 1f 	. . . 
	jc POPHDB		;0abe	da cc 1f 	. . . 
	call s0534h		;0ac1	cd 34 05 	. 4 . 
	db 'QBUF FULL',0
l0aceh:
	call s0806h		;0ace	cd 06 08 	. . . 
	call s0c65h		;0ad1	cd 65 0c 	. e . 
l0ad4h:
	call s18beh		;0ad4	cd be 18 	. . . 
	push psw			;0ad7	f5 	. 
	cc s0b85h		;0ad8	dc 85 0b 	. . . 
	pop psw			;0adb	f1 	. 
	jc l0ad4h		;0adc	da d4 0a 	. . . 
	call s24f0h		;0adf	cd f0 24 	. . $ 
	call s23d6h		;0ae2	cd d6 23 	. . # 
	cpi EOF		;0ae5	fe 1a 	. . 
	rz			;0ae7	c8 	. 
	cpi ESC		;0ae8	fe 1b 	. . 
	rz			;0aea	c8 	. 
	lxi h,l0ad4h		;0aeb	21 d4 0a 	. . . 
	push h			;0aee	e5 	. 
	cpi 01fh		;0aef	fe 1f 	. . 
	jz l0af9h		;0af1	ca f9 0a 	. . . 
	cpi 07fh		;0af4	fe 7f 	.  
	jnz l0b06h		;0af6	c2 06 0b 	. . . 
l0af9h:
	call s0c14h		;0af9	cd 14 0c 	. . . 
	call s24fch		;0afc	cd fc 24 	. . $ 
l0affh:
	shld 0273dh		;0aff	22 3d 27 	" = ' 
	call s1d9bh		;0b02	cd 9b 1d 	. . . 
	ret			;0b05	c9 	. 
l0b06h:
	cpi ' '		;0b06	fe 20 	.   
	jnc s0bach		;0b08	d2 ac 0b 	. . . 
	cpi 019h		;0b0b	fe 19 	. . 
	jnz l0b12h		;0b0d	c2 12 0b 	. . . 
l0b10h:
	mvi a,ESC		;0b10	3e 1b 	> . 
l0b12h:
	cpi 005h		;0b12	fe 05 	. . 
	jz s24e3h		;0b14	ca e3 24 	. . $ 
	cpi 015h		;0b17	fe 15 	. . 
	jz l0b21h		;0b19	ca 21 0b 	. . . 
	cpi 018h		;0b1c	fe 18 	. . 
	jnz l0b30h		;0b1e	c2 30 0b 	. 0 . 
l0b21h:
	call s24e3h		;0b21	cd e3 24 	. . $ 
l0b24h:
	call s0c14h		;0b24	cd 14 0c 	. . . 
	cpi LF			;0b27	fe 0a 	. . 
	rz			;0b29	c8 	. 
	shld 0273dh		;0b2a	22 3d 27 	" = ' 
	jmp l0b24h		;0b2d	c3 24 0b 	. $ . 
l0b30h:
	cpi BS			;0b30	fe 08 	. . 
	jnz l0b49h		;0b32	c2 49 0b 	. I . 
	lda CURYPOS		;0b35	3a 84 26 	: . & 
	ora a			;0b38	b7 	. 
	rz			;0b39	c8 	. 
	call s0c14h		;0b3a	cd 14 0c 	. . . 
	cpi LF			;0b3d	fe 0a 	. . 
	rz			;0b3f	c8 	. 
	cpi CR		;0b40	fe 0d 	. . 
	rz			;0b42	c8 	. 
s0b43h:
	call s0bb9h		;0b43	cd b9 0b 	. . . 
	jmp l0affh		;0b46	c3 ff 0a 	. . . 
l0b49h:
	cpi 01ch		;0b49	fe 1c 	. . 
	jnz l0b7dh		;0b4b	c2 7d 0b 	. } . 
l0b4eh:
	lda CURYPOS		;0b4e	3a 84 26 	: . & 
	ora a			;0b51	b7 	. 
	rz			;0b52	c8 	. 
	call s0c14h		;0b53	cd 14 0c 	. . . 
	cpi CR		;0b56	fe 0d 	. . 
	rz			;0b58	c8 	. 
	cpi LF			;0b59	fe 0a 	. . 
	rz			;0b5b	c8 	. 
	push psw			;0b5c	f5 	. 
	call s0b43h		;0b5d	cd 43 0b 	. C . 
	pop psw			;0b60	f1 	. 
	cpi TAB		;0b61	fe 09 	. . 
	jz l0b4eh		;0b63	ca 4e 0b 	. N . 
	cpi ' '		;0b66	fe 20 	.   
	jz l0b4eh		;0b68	ca 4e 0b 	. N . 
l0b6bh:
	lda CURYPOS		;0b6b	3a 84 26 	: . & 
	ora a			;0b6e	b7 	. 
	rz			;0b6f	c8 	. 
	call s0c14h		;0b70	cd 14 0c 	. . . 
	call s0ed7h		;0b73	cd d7 0e 	. . . 
	rnc 			;0b76	d0 	. 
	call s0b43h		;0b77	cd 43 0b 	. C . 
	jmp l0b6bh		;0b7a	c3 6b 0b 	. k . 
l0b7dh:
	cpi 012h		;0b7d	fe 12 	. . 
	jnz l0b9dh		;0b7f	c2 9d 0b 	. . . 
	call s24e3h		;0b82	cd e3 24 	. . $ 
s0b85h:
	lhld 0273dh		;0b85	2a 3d 27 	* = ' 
l0b88h:
	dcx h			;0b88	2b 	+ 
	call s0bebh		;0b89	cd eb 0b 	. . . 
	jc l0b95h		;0b8c	da 95 0b 	. . . 
	mov a,m			;0b8f	7e 	~ 
	cpi LF			;0b90	fe 0a 	. . 
	jnz l0b88h		;0b92	c2 88 0b 	. . . 
l0b95h:
	inx h			;0b95	23 	# 
	xchg			;0b96	eb 	. 
	lhld 0273dh		;0b97	2a 3d 27 	* = ' 
	jmp l07beh		;0b9a	c3 be 07 	. . . 
l0b9dh:
	cpi CR		;0b9d	fe 0d 	. . 
	jnz s0bach		;0b9f	c2 ac 0b 	. . . 
s0ba2h:
	mvi a,CR		;0ba2	3e 0d 	> . 
	call s0bach		;0ba4	cd ac 0b 	. . . 
	mvi a,LF		;0ba7	3e 0a 	> . 
	call s24fch		;0ba9	cd fc 24 	. . $ 
s0bach:
	push psw			;0bac	f5 	. 
	call s0c55h		;0bad	cd 55 0c 	. U . 
	call s1f95h		;0bb0	cd 95 1f 	. . . 
	pop psw			;0bb3	f1 	. 
	mov m,a			;0bb4	77 	w 
	inx h			;0bb5	23 	# 
	jmp l0affh		;0bb6	c3 ff 0a 	. . . 
s0bb9h:
	cpi TAB		;0bb9	fe 09 	. . 
	jnz l0bd6h		;0bbb	c2 d6 0b 	. . . 
	xchg			;0bbe	eb 	. 
	lhld l25a7h		;0bbf	2a a7 25 	* . % 
	mov b,m			;0bc2	46 	F 
	xchg			;0bc3	eb 	. 
	inr b			;0bc4	04 	. 
	rz			;0bc5	c8 	. 
	dcr b			;0bc6	05 	. 
	rz			;0bc7	c8 	. 
l0bc8h:
	call s0be0h		;0bc8	cd e0 0b 	. . . 
	dcr b			;0bcb	05 	. 
	jnz l0bc8h		;0bcc	c2 c8 0b 	. . . 
	xchg			;0bcf	eb 	. 
	dcx h			;0bd0	2b 	+ 
	shld l25a7h		;0bd1	22 a7 25 	" . % 
	xchg			;0bd4	eb 	. 
	ret			;0bd5	c9 	. 
l0bd6h:
	cpi ESC		;0bd6	fe 1b 	. . 
	jz s0be0h		;0bd8	ca e0 0b 	. . . 
	cpi ' '		;0bdb	fe 20 	.   
	cc s0be0h		;0bdd	dc e0 0b 	. . . 
s0be0h:
	call s022fh		;0be0	cd 2f 02 	. / . 
	mvi a,' '		;0be3	3e 20 	>   
	call s2567h		;0be5	cd 67 25 	. g % 
	jmp s022fh		;0be8	c3 2f 02 	. / . 
s0bebh:
	xchg			;0beb	eb 	. 
	lhld 0273bh		;0bec	2a 3b 27 	* ; ' 
	call CMPDEHL		;0bef	cd 9f 1f 	. . . 
	xchg			;0bf2	eb 	. 
	rnc 			;0bf3	d0 	. 
	xchg			;0bf4	eb 	. 
	lhld 0273dh		;0bf5	2a 3d 27 	* = ' 
	call s1f7bh		;0bf8	cd 7b 1f 	. { . 
	push h			;0bfb	e5 	. 
	call s1cc1h		;0bfc	cd c1 1c 	. . . 
	lhld 0273dh		;0bff	2a 3d 27 	* = ' 
	pop d			;0c02	d1 	. 
	push psw			;0c03	f5 	. 
	dad d			;0c04	19 	. 
	pop psw			;0c05	f1 	. 
	ret			;0c06	c9 	. 
s0c07h:
	lhld 0273dh		;0c07	2a 3d 27 	* = ' 
	dcx h			;0c0a	2b 	+ 
	push d			;0c0b	d5 	. 
	call s0bebh		;0c0c	cd eb 0b 	. . . 
	pop d			;0c0f	d1 	. 
	rc			;0c10	d8 	. 
	mov a,m			;0c11	7e 	~ 
	ora a			;0c12	b7 	. 
	ret			;0c13	c9 	. 
s0c14h:
	call s0c07h		;0c14	cd 07 0c 	. . . 
	rnc 			;0c17	d0 	. 
	inx sp			;0c18	33 	3 
	inx sp			;0c19	33 	3 
	ret			;0c1a	c9 	. 
s0c1bh:
	lxi h,0273dh		;0c1b	21 3d 27 	. = ' 
l0c1eh:
	lxi d,02763h		;0c1e	11 63 27 	. c ' 
	ldax d			;0c21	1a 	. 
	inx d			;0c22	13 	. 
s0c23h:
	mov c,a			;0c23	4f 	O 
	mvi b,000h		;0c24	06 00 	. . 
	jmp l0c2ch		;0c26	c3 2c 0c 	. , . 
l0c29h:
	lxi h,0273dh		;0c29	21 3d 27 	. = ' 
l0c2ch:
	call s1d92h		;0c2c	cd 92 1d 	. . . 
	call s0c8dh		;0c2f	cd 8d 0c 	. . . 
	rz			;0c32	c8 	. 
l0c33h:
	call s0c68h		;0c33	cd 68 0c 	. h . 
	call s0c3fh		;0c36	cd 3f 0c 	. ? . 
	dcr b			;0c39	05 	. 
	jnz l0c33h		;0c3a	c2 33 0c 	. 3 . 
	ora a			;0c3d	b7 	. 
	ret			;0c3e	c9 	. 
s0c3fh:
	push h			;0c3f	e5 	. 
	call s1f95h		;0c40	cd 95 1f 	. . . 
l0c43h:
	ldax d			;0c43	1a 	. 
	ani 07fh		;0c44	e6 7f 	.  
	mov m,a			;0c46	77 	w 
	inx d			;0c47	13 	. 
	inx h			;0c48	23 	# 
	dcr c			;0c49	0d 	. 
	jnz l0c43h		;0c4a	c2 43 0c 	. C . 
	xchg			;0c4d	eb 	. 
	xthl			;0c4e	e3 	. 
	mov m,e			;0c4f	73 	s 
	inx h			;0c50	23 	# 
	mov m,d			;0c51	72 	r 
	dcx h			;0c52	2b 	+ 
l0c53h:
	pop d			;0c53	d1 	. 
	ret			;0c54	c9 	. 
s0c55h:
	push d			;0c55	d5 	. 
	lhld 0273dh		;0c56	2a 3d 27 	* = ' 
	xchg			;0c59	eb 	. 
	lhld 0273fh		;0c5a	2a 3f 27 	* ? ' 
	call CMPDEHL		;0c5d	cd 9f 1f 	. . . 
	pop d			;0c60	d1 	. 
	lxi h,0273dh		;0c61	21 3d 27 	. = ' 
	rc			;0c64	d8 	. 
s0c65h:
	lxi h,0273dh		;0c65	21 3d 27 	. = ' 
s0c68h:
	push b			;0c68	c5 	. 
	push d			;0c69	d5 	. 
	push h			;0c6a	e5 	. 
l0c6bh:
	call s1efbh		;0c6b	cd fb 1e 	. . . 
	mov a,b			;0c6e	78 	x 
	ora a			;0c6f	b7 	. 
	jnz POPHDB		;0c70	c2 cc 1f 	. . . 
	call s1e95h		;0c73	cd 95 1e 	. . . 
	mov a,d			;0c76	7a 	z 
	ora a			;0c77	b7 	. 
	jnz l1e4ch		;0c78	c2 4c 1e 	. L . 
	call s1a9eh		;0c7b	cd 9e 1a 	. . . 
	jnc l0c6bh		;0c7e	d2 6b 0c 	. k . 
	call s0534h		;0c81	cd 34 05 	. 4 . 
	db 'MEM FULL',0
s0c8dh:
	call s1f56h		;0c8d	cd 56 1f 	. V . 
	inr b			;0c90	04 	. 
	inr c			;0c91	0c 	. 
	dcr c			;0c92	0d 	. 
	rnz			;0c93	c0 	. 
	dcr b			;0c94	05 	. 
	ret			;0c95	c9 	. 
s0c96h:
	call s0cbdh		;0c96	cd bd 0c 	. . . 
	rnc 			;0c99	d0 	. 
	call l1a87h		;0c9a	cd 87 1a 	. . . 
	jnc s0c96h		;0c9d	d2 96 0c 	. . . 
l0ca0h:
	lda 0275ch		;0ca0	3a 5c 27 	: \ ' 
	call s074bh		;0ca3	cd 4b 07 	. K . 
	stc			;0ca6	37 	7 
	ret			;0ca7	c9 	. 
s0ca8h:
	call s0cf3h		;0ca8	cd f3 0c 	. . . 
	rnc 			;0cab	d0 	. 
	call l1a87h		;0cac	cd 87 1a 	. . . 
	jnc s0ca8h		;0caf	d2 a8 0c 	. . . 
	jmp l0ca0h		;0cb2	c3 a0 0c 	. . . 
s0cb5h:
	call s0cbdh		;0cb5	cd bd 0c 	. . . 
	rnc 			;0cb8	d0 	. 
	call s1aaeh		;0cb9	cd ae 1a 	. . . 
	rc			;0cbc	d8 	. 
s0cbdh:
	call s0913h		;0cbd	cd 13 09 	. . . 
	lda 0275ch		;0cc0	3a 5c 27 	: \ ' 
	ora a			;0cc3	b7 	. 
	jm l0cd8h		;0cc4	fa d8 0c 	. . . 
	lxi h,0273fh		;0cc7	21 3f 27 	. ? ' 
	call s0d24h		;0cca	cd 24 0d 	. $ . 
	lhld 0273fh		;0ccd	2a 3f 27 	* ? ' 
	rc			;0cd0	d8 	. 
	call s0d51h		;0cd1	cd 51 0d 	. Q . 
l0cd4h:
	rc			;0cd4	d8 	. 
	jmp l0ee5h		;0cd5	c3 e5 0e 	. . . 
l0cd8h:
	lxi h,0273bh		;0cd8	21 3b 27 	. ; ' 
	call s0d24h		;0cdb	cd 24 0d 	. $ . 
	lhld 0273dh		;0cde	2a 3d 27 	* = ' 
	rc			;0ce1	d8 	. 
	call s0d39h		;0ce2	cd 39 0d 	. 9 . 
	call s0d79h		;0ce5	cd 79 0d 	. y . 
	jmp l0cd4h		;0ce8	c3 d4 0c 	. . . 
s0cebh:
	call s0cf3h		;0ceb	cd f3 0c 	. . . 
	rnc 			;0cee	d0 	. 
	call s1aaeh		;0cef	cd ae 1a 	. . . 
	rc			;0cf2	d8 	. 
s0cf3h:
	call s0cbdh		;0cf3	cd bd 0c 	. . . 
	lda 02763h		;0cf6	3a 63 27 	: c ' 
	call s091ah		;0cf9	cd 1a 09 	. . . 
	rc			;0cfc	d8 	. 
	mov b,a			;0cfd	47 	G 
	lda 0275ch		;0cfe	3a 5c 27 	: \ ' 
	ora a			;0d01	b7 	. 
	mov a,b			;0d02	78 	x 
	jm l0d12h		;0d03	fa 12 0d 	. . . 
	lhld 0273dh		;0d06	2a 3d 27 	* = ' 
	call s0d3ch		;0d09	cd 3c 0d 	. < . 
	shld 0273dh		;0d0c	22 3d 27 	" = ' 
	jmp s0c1bh		;0d0f	c3 1b 0c 	. . . 
l0d12h:
	lhld 0273fh		;0d12	2a 3f 27 	* ? ' 
	call BIDX		;0d15	cd 4a 0d 	. J . 
	shld 0273fh		;0d18	22 3f 27 	" ? ' 
	call s0c1bh		;0d1b	cd 1b 0c 	. . . 
	call s0d36h		;0d1e	cd 36 0d 	. 6 . 
	jmp l0ee5h		;0d21	c3 e5 0e 	. . . 
s0d24h:
	call s1efbh		;0d24	cd fb 1e 	. . . 
	inx b			;0d27	03 	. 
	dcx d			;0d28	1b 	. 
	ldax d			;0d29	1a 	. 
	inx d			;0d2a	13 	. 
	push b			;0d2b	c5 	. 
	mov b,a			;0d2c	47 	G 
	mov a,c			;0d2d	79 	y 
	sub b			;0d2e	90 	. 
	pop b			;0d2f	c1 	. 
	mov c,a			;0d30	4f 	O 
	mov a,b			;0d31	78 	x 
	sbi 000h		;0d32	de 00 	. . 
	mov b,a			;0d34	47 	G 
	ret			;0d35	c9 	. 
s0d36h:
	lhld 0273dh		;0d36	2a 3d 27 	* = ' 
s0d39h:
	lda 02763h		;0d39	3a 63 27 	: c ' 
s0d3ch:
	push h			;0d3c	e5 	. 
	mov h,a			;0d3d	67 	g 
	mov a,l			;0d3e	7d 	} 
	sub h			;0d3f	94 	. 
	pop h			;0d40	e1 	. 
	mov l,a			;0d41	6f 	o 
	mov a,h			;0d42	7c 	| 
	sbi 000h		;0d43	de 00 	. . 
	mov h,a			;0d45	67 	g 
	ret			;0d46	c9 	. 
	lda 02763h		;0d47	3a 63 27 	: c ' 
;
;	CALCULATE BYTE INDEX
;	BASE ADDRESS = HL
;	INDEX = A
;	RETURN HL=HL+A
BIDX:
	add l			;0d4a	85 	. 
	mov l,a			;0d4b	6f 	o 
	mov a,h			;0d4c	7c 	| 
	aci 000h		;0d4d	ce 00 	. . 
	mov h,a			;0d4f	67 	g 
	ret			;0d50	c9 	. 
s0d51h:
	ldax d			;0d51	1a 	. 
	ora a			;0d52	b7 	. 
	rz			;0d53	c8 	. 
	push d			;0d54	d5 	. 
	call s0e7fh		;0d55	cd 7f 0e 	.  . 
	jc l0d77h		;0d58	da 77 0d 	. w . 
	dcr b			;0d5b	05 	. 
	dcr c			;0d5c	0d 	. 
	push h			;0d5d	e5 	. 
l0d5eh:
	inx d			;0d5e	13 	. 
	ldax d			;0d5f	1a 	. 
	ora a			;0d60	b7 	. 
	jz l0d76h		;0d61	ca 76 0d 	. v . 
	cmp m			;0d64	be 	. 
	inx h			;0d65	23 	# 
	jz l0d5eh		;0d66	ca 5e 0d 	. ^ . 
	dcx h			;0d69	2b 	+ 
	call s0eabh		;0d6a	cd ab 0e 	. . . 
	inx h			;0d6d	23 	# 
	jz l0d5eh		;0d6e	ca 5e 0d 	. ^ . 
	pop h			;0d71	e1 	. 
	pop d			;0d72	d1 	. 
	jmp s0d51h		;0d73	c3 51 0d 	. Q . 
l0d76h:
	pop d			;0d76	d1 	. 
l0d77h:
	pop d			;0d77	d1 	. 
	ret			;0d78	c9 	. 
s0d79h:
	ldax d			;0d79	1a 	. 
	ora a			;0d7a	b7 	. 
	rz			;0d7b	c8 	. 
	inx h			;0d7c	23 	# 
l0d7dh:
	push d			;0d7d	d5 	. 
	ldax d			;0d7e	1a 	. 
	call s0e96h		;0d7f	cd 96 0e 	. . . 
	jc l0d77h		;0d82	da 77 0d 	. w . 
	dcr b			;0d85	05 	. 
	dcr c			;0d86	0d 	. 
	push h			;0d87	e5 	. 
l0d88h:
	inx d			;0d88	13 	. 
	inx h			;0d89	23 	# 
	ldax d			;0d8a	1a 	. 
	ora a			;0d8b	b7 	. 
	jz l0d99h		;0d8c	ca 99 0d 	. . . 
	cmp m			;0d8f	be 	. 
	jz l0d88h		;0d90	ca 88 0d 	. . . 
	call s0eabh		;0d93	cd ab 0e 	. . . 
	jz l0d88h		;0d96	ca 88 0d 	. . . 
l0d99h:
	pop h			;0d99	e1 	. 
	pop d			;0d9a	d1 	. 
	ora a			;0d9b	b7 	. 
	jnz l0d7dh		;0d9c	c2 7d 0d 	. } . 
	ret			;0d9f	c9 	. 
s0da0h:
	push b			;0da0	c5 	. 
	push d			;0da1	d5 	. 
	lxi b,00000h		;0da2	01 00 00 	. . . 
	lhld 02758h		;0da5	2a 58 27 	* X ' 
	xchg			;0da8	eb 	. 
	lda 0275ch		;0da9	3a 5c 27 	: \ ' 
	ora a			;0dac	b7 	. 
	jm l0dc7h		;0dad	fa c7 0d 	. . . 
	lhld 0273fh		;0db0	2a 3f 27 	* ? ' 
	dad d			;0db3	19 	. 
	jnc l0dbdh		;0db4	d2 bd 0d 	. . . 
	inx h			;0db7	23 	# 
	mov b,h			;0db8	44 	D 
	mov c,l			;0db9	4d 	M 
	lxi h,0ffffh		;0dba	21 ff ff 	. . . 
l0dbdh:
	xchg			;0dbd	eb 	. 
	lhld 02741h		;0dbe	2a 41 27 	* A ' 
	call s1fbdh		;0dc1	cd bd 1f 	. . . 
	jmp l0ddfh		;0dc4	c3 df 0d 	. . . 
l0dc7h:
	lhld 0273dh		;0dc7	2a 3d 27 	* = ' 
	call s1f7ch		;0dca	cd 7c 1f 	. | . 
	jc l0dd8h		;0dcd	da d8 0d 	. . . 
	call s1f67h		;0dd0	cd 67 1f 	. g . 
	mov b,h			;0dd3	44 	D 
	mov c,l			;0dd4	4d 	M 
	lxi h,00000h		;0dd5	21 00 00 	. . . 
l0dd8h:
	xchg			;0dd8	eb 	. 
	lhld 0273bh		;0dd9	2a 3b 27 	* ; ' 
	call s1fabh		;0ddc	cd ab 1f 	. . . 
l0ddfh:
	dad b			;0ddf	09 	. 
	jmp l0e4ch		;0de0	c3 4c 0e 	. L . 
s0de3h:
	mvi a,0ffh		;0de3	3e ff 	> . 
	call s0782h		;0de5	cd 82 07 	. . . 
s0de8h:
	push d			;0de8	d5 	. 
	lhld 02758h		;0de9	2a 58 27 	* X ' 
	push h			;0dec	e5 	. 
	call s0e04h		;0ded	cd 04 0e 	. . . 
	pop d			;0df0	d1 	. 
	jnc l0c53h		;0df1	d2 53 0c 	. S . 
	push psw			;0df4	f5 	. 
	call s1aaeh		;0df5	cd ae 1a 	. . . 
	jnc l0dfeh		;0df8	d2 fe 0d 	. . . 
	pop psw			;0dfb	f1 	. 
	pop d			;0dfc	d1 	. 
	ret			;0dfd	c9 	. 
l0dfeh:
	pop psw			;0dfe	f1 	. 
	xchg			;0dff	eb 	. 
	shld 02758h		;0e00	22 58 27 	" X ' 
	pop d			;0e03	d1 	. 
s0e04h:
	push b			;0e04	c5 	. 
	push d			;0e05	d5 	. 
	lhld 02758h		;0e06	2a 58 27 	* X ' 
	xchg			;0e09	eb 	. 
	lxi h,0275ch		;0e0a	21 5c 27 	. \ ' 
	mov a,d			;0e0d	7a 	z 
	ora e			;0e0e	b3 	. 
	jnz l0e13h		;0e0f	c2 13 0e 	. . . 
	dcr m			;0e12	35 	5 
l0e13h:
	mov a,m			;0e13	7e 	~ 
	ora a			;0e14	b7 	. 
	jm l0e2fh		;0e15	fa 2f 0e 	. / . 
	call s1ef8h		;0e18	cd f8 1e 	. . . 
	call s1f95h		;0e1b	cd 95 1f 	. . . 
l0e1eh:
	call s0e7dh		;0e1e	cd 7d 0e 	. } . 
	jc l0e4bh		;0e21	da 4b 0e 	. K . 
	dcr b			;0e24	05 	. 
	dcr c			;0e25	0d 	. 
	dcx d			;0e26	1b 	. 
	mov a,d			;0e27	7a 	z 
	ora e			;0e28	b3 	. 
	jnz l0e1eh		;0e29	c2 1e 0e 	. . . 
	jmp l0e4bh		;0e2c	c3 4b 0e 	. K . 
l0e2fh:
	inx d			;0e2f	13 	. 
	lxi h,0273bh		;0e30	21 3b 27 	. ; ' 
	call s1efbh		;0e33	cd fb 1e 	. . . 
	lhld 0273dh		;0e36	2a 3d 27 	* = ' 
l0e39h:
	call l234eh		;0e39	cd 4e 23 	. N # 
	call s0e94h		;0e3c	cd 94 0e 	. . . 
	jc l0e76h		;0e3f	da 76 0e 	. v . 
	dcr b			;0e42	05 	. 
	dcr c			;0e43	0d 	. 
	dcx d			;0e44	1b 	. 
	mov a,d			;0e45	7a 	z 
	ora e			;0e46	b3 	. 
	jnz l0e39h		;0e47	c2 39 0e 	. 9 . 
	inx h			;0e4a	23 	# 
l0e4bh:
	xchg			;0e4b	eb 	. 
l0e4ch:
	xra a			;0e4c	af 	. 
l0e4dh:
	sta 0271eh		;0e4d	32 1e 27 	2 . ' 
	call s1cb5h		;0e50	cd b5 1c 	. . . 
	jnz l0e5ch		;0e53	c2 5c 0e 	. \ . 
	lxi h,00000h		;0e56	21 00 00 	. . . 
	shld 0271eh		;0e59	22 1e 27 	" . ' 
l0e5ch:
	shld 02758h		;0e5c	22 58 27 	" X ' 
	xchg			;0e5f	eb 	. 
	shld 02756h		;0e60	22 56 27 	" V ' 
	lda 0275ch		;0e63	3a 5c 27 	: \ ' 
	ora a			;0e66	b7 	. 
	push h			;0e67	e5 	. 
	lxi h,0ffffh		;0e68	21 ff ff 	. . . 
	dad d			;0e6b	19 	. 
	jc POPHDB		;0e6c	da cc 1f 	. . . 
	lhld 0271dh		;0e6f	2a 1d 27 	* . ' 
	dad h			;0e72	29 	) 
	jmp POPHDB		;0e73	c3 cc 1f 	. . . 
l0e76h:
	dcx d			;0e76	1b 	. 
	xchg			;0e77	eb 	. 
	mvi a,0ffh		;0e78	3e ff 	> . 
	jmp l0e4dh		;0e7a	c3 4d 0e 	. M . 
s0e7dh:
	mvi a,LF		;0e7d	3e 0a 	> . 
s0e7fh:
	inr b			;0e7f	04 	. 
	inr c			;0e80	0c 	. 
	jmp l0e8ah		;0e81	c3 8a 0e 	. . . 
l0e84h:
	cmp m			;0e84	be 	. 
	cnz s0eabh		;0e85	c4 ab 0e 	. . . 
	inx h			;0e88	23 	# 
	rz			;0e89	c8 	. 
l0e8ah:
	dcr c			;0e8a	0d 	. 
	jnz l0e84h		;0e8b	c2 84 0e 	. . . 
	dcr b			;0e8e	05 	. 
	jnz l0e84h		;0e8f	c2 84 0e 	. . . 
	stc			;0e92	37 	7 
	ret			;0e93	c9 	. 
s0e94h:
	mvi a,LF		;0e94	3e 0a 	> . 
s0e96h:
	inr b			;0e96	04 	. 
	inr c			;0e97	0c 	. 
	jmp l0ea1h		;0e98	c3 a1 0e 	. . . 
l0e9bh:
	dcx h			;0e9b	2b 	+ 
	cmp m			;0e9c	be 	. 
	cnz s0eabh		;0e9d	c4 ab 0e 	. . . 
	rz			;0ea0	c8 	. 
l0ea1h:
	dcr c			;0ea1	0d 	. 
	jnz l0e9bh		;0ea2	c2 9b 0e 	. . . 
	dcr b			;0ea5	05 	. 
	jnz l0e9bh		;0ea6	c2 9b 0e 	. . . 
	stc			;0ea9	37 	7 
	ret			;0eaa	c9 	. 
s0eabh:
	cpi 013h		;0eab	fe 13 	. . 
	jz l0ec7h		;0ead	ca c7 0e 	. . . 
	rnc 			;0eb0	d0 	. 
	cpi 001h		;0eb1	fe 01 	. . 
	rz			;0eb3	c8 	. 
	cpi 00fh		;0eb4	fe 0f 	. . 
	rnz			;0eb6	c0 	. 
	inx d			;0eb7	13 	. 
	ldax d			;0eb8	1a 	. 
	dcx d			;0eb9	1b 	. 
	ora a			;0eba	b7 	. 
	jz l0ec5h		;0ebb	ca c5 0e 	. . . 
	cmp m			;0ebe	be 	. 
	mvi a,00fh		;0ebf	3e 0f 	> . 
	jz l0ed0h		;0ec1	ca d0 0e 	. . . 
	inx d			;0ec4	13 	. 
l0ec5h:
	cmp a			;0ec5	bf 	. 
	ret			;0ec6	c9 	. 
l0ec7h:
	mov a,m			;0ec7	7e 	~ 
	call s0ed7h		;0ec8	cd d7 0e 	. . . 
	mvi a,013h		;0ecb	3e 13 	> . 
	jnc l0ec5h		;0ecd	d2 c5 0e 	. . . 
l0ed0h:
	ora a			;0ed0	b7 	. 
	ret			;0ed1	c9 	. 
s0ed2h:
	call s22fah		;0ed2	cd fa 22 	. . " 
	stc			;0ed5	37 	7 
	rz			;0ed6	c8 	. 
s0ed7h:
	call s2229h		;0ed7	cd 29 22 	. ) " 
	rc			;0eda	d8 	. 
	call s064ch		;0edb	cd 4c 06 	. L . 
	cpi 041h		;0ede	fe 41 	. A 
	cmc			;0ee0	3f 	? 
	rnc 			;0ee1	d0 	. 
	cpi 05bh		;0ee2	fe 5b 	. [ 
	ret			;0ee4	c9 	. 
l0ee5h:
	push b			;0ee5	c5 	. 
	push d			;0ee6	d5 	. 
	push h			;0ee7	e5 	. 
	xchg			;0ee8	eb 	. 
	lhld 0273bh		;0ee9	2a 3b 27 	* ; ' 
	call CMPDEHL		;0eec	cd 9f 1f 	. . . 
	cc s0f1dh		;0eef	dc 1d 0f 	. . . 
	lhld 02741h		;0ef2	2a 41 27 	* A ' 
	inx h			;0ef5	23 	# 
	call CMPDEHL		;0ef6	cd 9f 1f 	. . . 
	cnc s0f1dh		;0ef9	d4 1d 0f 	. . . 
	lhld 0273dh		;0efc	2a 3d 27 	* = ' 
	inx h			;0eff	23 	# 
	call CMPDEHL		;0f00	cd 9f 1f 	. . . 
	jnc l0f0dh		;0f03	d2 0d 0f 	. . . 
	xchg			;0f06	eb 	. 
	lxi d,0273bh		;0f07	11 3b 27 	. ; ' 
	jmp l1e28h		;0f0a	c3 28 1e 	. ( . 
l0f0dh:
	lhld 0273fh		;0f0d	2a 3f 27 	* ? ' 
	call CMPDEHL		;0f10	cd 9f 1f 	. . . 
	cc s0f1dh		;0f13	dc 1d 0f 	. . . 
	xchg			;0f16	eb 	. 
	lxi d,0273fh		;0f17	11 3f 27 	. ? ' 
	jmp l1e05h		;0f1a	c3 05 1e 	. . . 
s0f1dh:
	call s0534h		;0f1d	cd 34 05 	. 4 . 
	db 'PUTCUR ERR',0
s0f2bh:
	call s18e3h		;0f2b	cd e3 18 	. . . 
	call s1660h		;0f2e	cd 60 16 	. ` . 
	dcx h			;0f31	2b 	+ 
	ana m			;0f32	a6 	. 
	mvi m,000h		;0f33	36 00 	6 . 
	jnz l0f64h		;0f35	c2 64 0f 	. d . 
	lxi h,l269dh+1		;0f38	21 9e 26 	. . & 
	mov a,m			;0f3b	7e 	~ 
	mvi m,000h		;0f3c	36 00 	6 . 
	ora a			;0f3e	b7 	. 
	push psw			;0f3f	f5 	. 
	cnz s16f0h		;0f40	c4 f0 16 	. . . 
	pop psw			;0f43	f1 	. 
	jnz l0f57h		;0f44	c2 57 0f 	. W . 
	lxi h,l2685h		;0f47	21 85 26 	. . & 
	mov a,m			;0f4a	7e 	~ 
	ora a			;0f4b	b7 	. 
	jp l0f54h		;0f4c	f2 54 0f 	. T . 
	xra a			;0f4f	af 	. 
	mov m,a			;0f50	77 	w 
	sta l269bh		;0f51	32 9b 26 	2 . & 
l0f54h:
	call s1674h		;0f54	cd 74 16 	. t . 
l0f57h:
	lda l2691h+2		;0f57	3a 93 26 	: . & 
	lhld 02695h		;0f5a	2a 95 26 	* . & 
	shld l2691h+2		;0f5d	22 93 26 	" . & 
	cmp l			;0f60	bd 	. 
	cnz s1358h		;0f61	c4 58 13 	. X . 
l0f64h:
	lda l269bh		;0f64	3a 9b 26 	: . & 
	ora a			;0f67	b7 	. 
	jnz l0fa4h		;0f68	c2 a4 0f 	. . . 
	call s1502h		;0f6b	cd 02 15 	. . . 
	call s01f2h		;0f6e	cd f2 01 	. . . 
	lhld l2685h		;0f71	2a 85 26 	* . & 
	call s17eah		;0f74	cd ea 17 	. . . 
	mvi a,07bh		;0f77	3e 7b 	> { 
	cc s07adh		;0f79	dc ad 07 	. . . 
	lxi d,00000h		;0f7c	11 00 00 	. . . 
l0f7fh:
	call s105dh		;0f7f	cd 5d 10 	. ] . 
	ani 0c0h		;0f82	e6 c0 	. . 
	jnz l1055h		;0f84	c2 55 10 	. U . 
	call s17fah		;0f87	cd fa 17 	. . . 
	jz l0f99h		;0f8a	ca 99 0f 	. . . 
	cpi LF		;0f8d	fe 0a 	. . 
	jnz l0f93h		;0f8f	c2 93 0f 	. . . 
	inx d			;0f92	13 	. 
l0f93h:
	call s24fch		;0f93	cd fc 24 	. . $ 
	jmp l0f7fh		;0f96	c3 7f 0f 	.  . 
l0f99h:
	xchg			;0f99	eb 	. 
	shld l2685h		;0f9a	22 85 26 	" . & 
	mvi a,0ffh		;0f9d	3e ff 	> . 
	sta l269bh		;0f9f	32 9b 26 	2 . & 
	mvi e,000h		;0fa2	1e 00 	. . 
l0fa4h:
	lda l269ch		;0fa4	3a 9c 26 	: . & 
	ora a			;0fa7	b7 	. 
	jnz l1039h		;0fa8	c2 39 10 	. 9 . 
	lhld l2691h		;0fab	2a 91 26 	* . & 
	call l020bh		;0fae	cd 0b 02 	. . . 
	call s186eh		;0fb1	cd 6e 18 	. n . 
	call s105dh		;0fb4	cd 5d 10 	. ] . 
	ani 0e8h		;0fb7	e6 e8 	. . 
	jnz l1055h		;0fb9	c2 55 10 	. U . 
l0fbch:
	call s187fh		;0fbc	cd 7f 18 	.  . 
	jc l0fd0h		;0fbf	da d0 0f 	. . . 
	cpi LF		;0fc2	fe 0a 	. . 
	jz 00fd4h		;0fc4	ca d4 0f 	. . . 
	call s2421h		;0fc7	cd 21 24 	. . $ 
	call l234eh		;0fca	cd 4e 23 	. N # 
	jmp l0fbch		;0fcd	c3 bc 0f 	. . . 
l0fd0h:
	call s241fh		;0fd0	cd 1f 24 	. . $ 
	lda 0032bh		;0fd3	3a 2b 03 	: + . 
	lda l269dh		;0fd6	3a 9d 26 	: . & 
	ora a			;0fd9	b7 	. 
	jnz l1039h		;0fda	c2 39 10 	. 9 . 
	call s105dh		;0fdd	cd 5d 10 	. ] . 
	ani 0e8h		;0fe0	e6 e8 	. . 
	jnz l1055h		;0fe2	c2 55 10 	. U . 
l0fe5h:
	push d			;0fe5	d5 	. 
	xchg			;0fe6	eb 	. 
	lhld l2689h		;0fe7	2a 89 26 	* . & 
	call CMPDEHL		;0fea	cd 9f 1f 	. . . 
	xchg			;0fed	eb 	. 
	pop d			;0fee	d1 	. 
	jnc l1006h		;0fef	d2 06 10 	. . . 
	call s105dh		;0ff2	cd 5d 10 	. ] . 
	ani 0d4h		;0ff5	e6 d4 	. . 
	jnz l1055h		;0ff7	c2 55 10 	. U . 
	call s187fh		;0ffa	cd 7f 18 	.  . 
	jc l1006h		;0ffd	da 06 10 	. . . 
	call s2421h		;1000	cd 21 24 	. . $ 
	jmp l0fe5h		;1003	c3 e5 0f 	. . . 
l1006h:
	lhld CURXPOS		;1006	2a 83 26 	* . & 
	push h			;1009	e5 	. 
l100ah:
	call s241fh		;100a	cd 1f 24 	. . $ 
	lda CURXPOS		;100d	3a 83 26 	: . & 
	lxi h,02697h		;1010	21 97 26 	. . & 
	cmp m			;1013	be 	. 
	jz l101ah		;1014	ca 1a 10 	. . . 
	jnc l1035h		;1017	d2 35 10 	. 5 . 
l101ah:
	lxi h,l2680h		;101a	21 80 26 	. . & 
	cmp m			;101d	be 	. 
	jnc l1035h		;101e	d2 35 10 	. 5 . 
	call s105dh		;1021	cd 5d 10 	. ] . 
	ani 080h		;1024	e6 80 	. . 
	pop h			;1026	e1 	. 
	jnz l1055h		;1027	c2 55 10 	. U . 
	push h			;102a	e5 	. 
	mvi a,LF		;102b	3e 0a 	> . 
	call s24fch		;102d	cd fc 24 	. . $ 
	mvi e,0ffh		;1030	1e ff 	. . 
	jmp l100ah		;1032	c3 0a 10 	. . . 
l1035h:
	pop h			;1035	e1 	. 
	shld 02697h		;1036	22 97 26 	" . & 
l1039h:
	lxi h,0ffffh		;1039	21 ff ff 	. . . 
	shld l269ch		;103c	22 9c 26 	" . & 
	mvi e,000h		;103f	1e 00 	. . 
	call s0208h		;1041	cd 08 02 	. . . 
	lxi h,l26a1h+2		;1044	21 a3 26 	. . & 
	mov a,m			;1047	7e 	~ 
	ora a			;1048	b7 	. 
	rz			;1049	c8 	. 
	inx h			;104a	23 	# 
	mvi m,0ffh		;104b	36 ff 	6 . 
	mvi a,03ch		;104d	3e 3c 	> < 
	call s2567h		;104f	cd 67 25 	. g % 
	jmp s0208h		;1052	c3 08 02 	. . . 
l1055h:
	mvi a,LF		;1055	3e 0a 	> . 
	call s24fch		;1057	cd fc 24 	. . $ 
	jmp s0208h		;105a	c3 08 02 	. . . 
s105dh:
	push b			;105d	c5 	. 
	push d			;105e	d5 	. 
	push h			;105f	e5 	. 
	call l234eh		;1060	cd 4e 23 	. N # 
	lxi b,FBASE		;1063	01 06 00 	. . . 
	lhld l25b7h		;1066	2a b7 25 	* . % 
	xchg			;1069	eb 	. 
	dcx d			;106a	1b 	. 
l106bh:
	dcr c			;106b	0d 	. 
	jz l1084h		;106c	ca 84 10 	. . . 
	inx d			;106f	13 	. 
	ldax d			;1070	1a 	. 
	inr a			;1071	3c 	< 
	jnz l107ah		;1072	c2 7a 10 	. z . 
	lxi d,l25b9h		;1075	11 b9 25 	. . % 
	ldax d			;1078	1a 	. 
	inr a			;1079	3c 	< 
l107ah:
	dcr a			;107a	3d 	= 
	jz l1084h		;107b	ca 84 10 	. . . 
	call s10bbh		;107e	cd bb 10 	. . . 
	jmp l106bh		;1081	c3 6b 10 	. k . 
l1084h:
	lda l26a1h+1		;1084	3a a2 26 	: . & 
	cpi 002h		;1087	fe 02 	. . 
	jc l1092h		;1089	da 92 10 	. . . 
	lda l269dh+2		;108c	3a 9f 26 	: . & 
	call s10bbh		;108f	cd bb 10 	. . . 
l1092h:
	lda l26a1h+2		;1092	3a a3 26 	: . & 
	ori 0f3h		;1095	f6 f3 	. . 
	ana b			;1097	a0 	. 
	mov b,a			;1098	47 	G 
	lda l268fh		;1099	3a 8f 26 	: . & 
	ora a			;109c	b7 	. 
	mov a,b			;109d	78 	x 
	jz l10a3h		;109e	ca a3 10 	. . . 
	ani 0bfh		;10a1	e6 bf 	. . 
l10a3h:
	mov b,a			;10a3	47 	G 
	rar			;10a4	1f 	. 
	mov a,b			;10a5	78 	x 
	jnc l10b8h		;10a6	d2 b8 10 	. . . 
	lda l268fh		;10a9	3a 8f 26 	: . & 
	adi 004h		;10ac	c6 04 	. . 
	lxi h,l2680h		;10ae	21 80 26 	. . & 
	cmp m			;10b1	be 	. 
	mov a,b			;10b2	78 	x 
	jc l10b8h		;10b3	da b8 10 	. . . 
	ani 0c3h		;10b6	e6 c3 	. . 
l10b8h:
	jmp POPHDB		;10b8	c3 cc 1f 	. . . 
s10bbh:
	cpi DEL		;10bb	fe 7f 	.  
	jnz l10c5h		;10bd	c2 c5 10 	. . . 
	mvi a,' '		;10c0	3e 20 	>   
	ora b			;10c2	b0 	. 
	mov b,a			;10c3	47 	G 
	ret			;10c4	c9 	. 
l10c5h:
	cpi ' '		;10c5	fe 20 	.   
	jc l10cch		;10c7	da cc 10 	. . . 
	mvi a,' '		;10ca	3e 20 	>   
l10cch:
	lxi h,l10d6h		;10cc	21 d6 10 	. . . 
	call BIDX		;10cf	cd 4a 0d 	. J . 
	mov a,m			;10d2	7e 	~ 
	ora b			;10d3	b0 	. 
	mov b,a			;10d4	47 	G 
	ret			;10d5	c9 	. 
l10d6h:
	nop			;10d6	00 	. 
	mov b,b			;10d7	40 	@ 
	nop			;10d8	00 	. 
	add b			;10d9	80 	. 
	nop			;10da	00 	. 
	add b			;10db	80 	. 
	nop			;10dc	00 	. 
	dw 0020h		;10dd	20 00 	  . 
l10dfh:
	db 8			;10df	08 	. 
	lxi b,00140h		;10e0	01 40 01 	. @ . 
	dcr b			;10e3	05 	. 
	dw 02030h		;10e4	30 20 	0   
	nop			;10e6	00 	. 
	nop			;10e7	00 	. 
	add b			;10e8	80 	. 
	nop			;10e9	00 	. 
	nop			;10ea	00 	. 
	nop			;10eb	00 	. 
	nop			;10ec	00 	. 
	nop			;10ed	00 	. 
	nop			;10ee	00 	. 
	dw 00130h		;10ef	30 01 	0 . 
	add b			;10f1	80 	. 
l10f2h:
	dw 00020h		;10f2	20 00 	  . 
l10f4h:
	nop			;10f4	00 	. 
	dw 00820h		;10f5	20 08 	  . 
s10f7h:
	lxi h,0ff00h		;10f7	21 00 ff 	. . . 
	shld l26a4h+1		;10fa	22 a5 26 	" . & 
	xra a			;10fd	af 	. 
	sta l26a1h+1		;10fe	32 a2 26 	2 . & 
	lda HITE		;1101	3a b9 01 	: . . 
	sui 002h		;1104	d6 02 	. . 
l1106h:
	sta l2680h		;1106	32 80 26 	2 . & 
	rar			;1109	1f 	. 
	ora a			;110a	b7 	. 
	rar			;110b	1f 	. 
	sta l2685h		;110c	32 85 26 	2 . & 
	call s1425h		;110f	cd 25 14 	. % . 
	call s0c07h		;1112	cd 07 0c 	. . . 
	jc l1125h		;1115	da 25 11 	. % . 
	cpi CR		;1118	fe 0d 	. . 
	jnz l1125h		;111a	c2 25 11 	. % . 
	call s187ch		;111d	cd 7c 18 	. | . 
	cpi LF		;1120	fe 0a 	. . 
	cz s1216h		;1122	cc 16 12 	. . . 
l1125h:
	call s0f2bh		;1125	cd 2b 0f 	. + . 
	xra a			;1128	af 	. 
	sta 0275fh		;1129	32 5f 27 	2 _ ' 
	lda l269dh+2		;112c	3a 9f 26 	: . & 
	lxi h,l26a0h		;112f	21 a0 26 	. . & 
	cmp m			;1132	be 	. 
	jnz l1138h		;1133	c2 38 11 	. 8 . 
	mvi a,0ffh		;1136	3e ff 	> . 
l1138h:
	mov m,a			;1138	77 	w 
	lxi h,l1125h		;1139	21 25 11 	. % . 
	push h			;113c	e5 	. 
	lxi h,l26a1h+1		;113d	21 a2 26 	. . & 
	dcr m			;1140	35 	5 
	jz l1147h		;1141	ca 47 11 	. G . 
	jp l1175h		;1144	f2 75 11 	. u . 
l1147h:
	mvi m,001h		;1147	36 01 	6 . 
l1149h:
	call s18b4h		;1149	cd b4 18 	. . . 
	sta l269dh+2		;114c	32 9f 26 	2 . & 
	cpi 000h		;114f	fe 00 	. . 
	jnz l1175h		;1151	c2 75 11 	. u . 
	lxi h,l26a1h+1		;1154	21 a2 26 	. . & 
	mov a,m			;1157	7e 	~ 
	push psw			;1158	f5 	. 
	mvi m,000h		;1159	36 00 	6 . 
	call l0f64h		;115b	cd 64 0f 	. d . 
	pop psw			;115e	f1 	. 
	lxi h,l26a1h+1		;115f	21 a2 26 	. . & 
	mvi m,004h		;1162	36 04 	6 . 
	cpi 004h		;1164	fe 04 	. . 
	jnz l116bh		;1166	c2 6b 11 	. k . 
	mvi m,010h		;1169	36 10 	6 . 
l116bh:
	cpi 010h		;116b	fe 10 	. . 
	jnz l1172h		;116d	c2 72 11 	. r . 
	mvi m,040h		;1170	36 40 	6 @ 
l1172h:
	jmp l1149h		;1172	c3 49 11 	. I . 
l1175h:
	lxi h,l26a4h		;1175	21 a4 26 	. . & 
	mov a,m			;1178	7e 	~ 
	mvi m,000h		;1179	36 00 	6 . 
	ora a			;117b	b7 	. 
	jz l11a8h		;117c	ca a8 11 	. . . 
	call s187ch		;117f	cd 7c 18 	. | . 
	jc l11a0h		;1182	da a0 11 	. . . 
	cpi CR		;1185	fe 0d 	. . 
	jz l11a0h		;1187	ca a0 11 	. . . 
	cpi TAB		;118a	fe 09 	. . 
	jz l11a0h		;118c	ca a0 11 	. . . 
	cpi DEL		;118f	fe 7f 	.  
	jnz l1196h		;1191	c2 96 11 	. . . 
	mvi a,07eh		;1194	3e 7e 	> ~ 
l1196h:
	cpi ' '		;1196	fe 20 	.   
	jnc l119dh		;1198	d2 9d 11 	. . . 
	mvi a,05eh		;119b	3e 5e 	> ^ 
l119dh:
	jmp l11a2h		;119d	c3 a2 11 	. . . 
l11a0h:
	mvi a,' '		;11a0	3e 20 	>   
l11a2h:
	call s2567h		;11a2	cd 67 25 	. g % 
	call s0208h		;11a5	cd 08 02 	. . . 
l11a8h:
	call l234eh		;11a8	cd 4e 23 	. N # 
	lda l269dh+2		;11ab	3a 9f 26 	: . & 
	cpi DEL		;11ae	fe 7f 	.  
	jz l1448h		;11b0	ca 48 14 	. H . 
	cpi ' '		;11b3	fe 20 	.   
	jnc l153ah		;11b5	d2 3a 15 	. : . 
	push psw			;11b8	f5 	. 
	add a			;11b9	87 	. 
	lxi h,l11c5h		;11ba	21 c5 11 	. . . 
	call BIDX		;11bd	cd 4a 0d 	. J . 
	call s1f95h		;11c0	cd 95 1f 	. . . 
	pop psw			;11c3	f1 	. 
s11c4h:
	pchl			;11c4	e9 	. 
l11c5h:
	mov b,l			;11c5	45 	E 
	inx d			;11c6	13 	. 
	mov l,l			;11c7	6d 	m 
	stax d			;11c8	12 	. 
	adc c			;11c9	89 	. 
	stax d			;11ca	12 	. 
	inr l			;11cb	2c 	, 
	inr d			;11cc	14 	. 
	mvi d,012h		;11cd	16 12 	. . 
	inx b			;11cf	03 	. 
	inx d			;11d0	13 	. 
	cmc			;11d1	3f 	? 
	stax d			;11d2	12 	. 
	cpe 02a14h		;11d3	ec 14 2a 	. . * 
	stax d			;11d6	12 	. 
	lda 05415h		;11d7	3a 15 54 	: . T 
	dad d			;11da	19 	. 
	db 0ddh,014h,016h	;11db	dd 14 16 	. . . 
	stax d			;11de	12 	. 
	xra e			;11df	ab 	. 
	dcr d			;11e0	15 	. 
	sub h			;11e1	94 	. 
	dcr d			;11e2	15 	. 
	dcx b			;11e3	0b 	. 
	dcr d			;11e4	15 	. 
	ldax d			;11e5	1a 	. 
	dcr d			;11e6	15 	. 
	ora m			;11e7	b6 	. 
	stax d			;11e8	12 	. 
	inr d			;11e9	14 	. 
	inr d			;11ea	14 	. 
	lhld 07612h		;11eb	2a 12 76 	* . v 
	inr d			;11ee	14 	. 
	cmp a			;11ef	bf 	. 
	inr d			;11f0	14 	. 
	mov b,l			;11f1	45 	E 
	inx d			;11f2	13 	. 
	dad b			;11f3	09 	. 
	inr d			;11f4	14 	. 
	pop b			;11f5	c1 	. 
	stax d			;11f6	12 	. 
	aci 014h		;11f7	ce 14 	. . 
	mov a,h			;11f9	7c 	| 
	inx d			;11fa	13 	. 
	dcr b			;11fb	05 	. 
	stax d			;11fc	12 	. 
	mov h,b			;11fd	60 	` 
	inr d			;11fe	14 	. 
	ldax d			;11ff	1a 	. 
	dcr d			;1200	15 	. 
	mov b,m			;1201	46 	F 
	inx d			;1202	13 	. 
	mov c,b			;1203	48 	H 
	inr d			;1204	14 	. 
	pop h			;1205	e1 	. 
	call s0806h		;1206	cd 06 08 	. . . 
s1209h:
	lxi h,l26a6h		;1209	21 a6 26 	. . & 
	mov a,m			;120c	7e 	~ 
	ora a			;120d	b7 	. 
	rz			;120e	c8 	. 
	inr m			;120f	34 	4 
	lhld l2680h		;1210	2a 80 26 	* . & 
	jmp l020bh		;1213	c3 0b 02 	. . . 
s1216h:
	call s1374h		;1216	cd 74 13 	. t . 
	call s1875h		;1219	cd 75 18 	. u . 
	call s164ch		;121c	cd 4c 16 	. L . 
	push psw			;121f	f5 	. 
	call l0ee5h		;1220	cd e5 0e 	. . . 
	pop psw			;1223	f1 	. 
	cpi CR		;1224	fe 0d 	. . 
	rnz			;1226	c0 	. 
	jmp s1216h		;1227	c3 16 12 	. . . 
l122ah:
	call s0c14h		;122a	cd 14 0c 	. . . 
	call s1656h		;122d	cd 56 16 	. V . 
	call l0ee5h		;1230	cd e5 0e 	. . . 
	call s0c07h		;1233	cd 07 0c 	. . . 
	cpi CR		;1236	fe 0d 	. . 
	rnz			;1238	c0 	. 
	call s1358h		;1239	cd 58 13 	. X . 
	jmp l122ah		;123c	c3 2a 12 	. * . 
	call s1374h		;123f	cd 74 13 	. t . 
	call s186eh		;1242	cd 6e 18 	. n . 
l1245h:
	call s187fh		;1245	cd 7f 18 	.  . 
	jc l0ee5h		;1248	da e5 0e 	. . . 
	call s0ed7h		;124b	cd d7 0e 	. . . 
	jc l1245h		;124e	da 45 12 	. E . 
	call s164ch		;1251	cd 4c 16 	. L . 
l1254h:
	call s187fh		;1254	cd 7f 18 	.  . 
	jc l0ee5h		;1257	da e5 0e 	. . . 
	call s164ch		;125a	cd 4c 16 	. L . 
	call s22fah		;125d	cd fa 22 	. . " 
	jz l1254h		;1260	ca 54 12 	. T . 
	dcx h			;1263	2b 	+ 
	jmp l0ee5h		;1264	c3 e5 0e 	. . . 
l1267h:
	call s1656h		;1267	cd 56 16 	. V . 
	call l0ee5h		;126a	cd e5 0e 	. . . 
	call s1358h		;126d	cd 58 13 	. X . 
	call s0c14h		;1270	cd 14 0c 	. . . 
	call s22fah		;1273	cd fa 22 	. . " 
	jz l1267h		;1276	ca 67 12 	. g . 
	call s1656h		;1279	cd 56 16 	. V . 
l127ch:
	call l0ee5h		;127c	cd e5 0e 	. . . 
	call s0c14h		;127f	cd 14 0c 	. . . 
	call s0ed7h		;1282	cd d7 0e 	. . . 
	jc l127ch		;1285	da 7c 12 	. | . 
	ret			;1288	c9 	. 
	call s15f3h		;1289	cd f3 15 	. . . 
	jz l12ach		;128c	ca ac 12 	. . . 
	xra a			;128f	af 	. 
	sta l268fh+1		;1290	32 90 26 	2 . & 
	lhld CURXPOS		;1293	2a 83 26 	* . & 
	push h			;1296	e5 	. 
	lxi h,0			;1297	21 00 00 	. . . 
	shld CURXPOS		;129a	22 83 26 	" . & 
	call s1816h		;129d	cd 16 18 	. . . 
	lhld 02756h		;12a0	2a 56 27 	* V ' 
	call l0ee5h		;12a3	cd e5 0e 	. . . 
	lda CURXPOS		;12a6	3a 83 26 	: . & 
	jmp l132eh		;12a9	c3 2e 13 	. . . 
l12ach:
	mvi d,0			;12ac	16 00 	. . 
	mvi a,240		;12ae	3e f0 	> . 
l12b0h:
	sta l268fh+1		;12b0	32 90 26 	2 . & 
	jmp l12f7h		;12b3	c3 f7 12 	. . . 
	mvi d,0			;12b6	16 00 	. . 
	lda l268fh+1		;12b8	3a 90 26 	: . & 
	ori 007h		;12bb	f6 07 	. . 
	inr a			;12bd	3c 	< 
	jmp l12b0h		;12be	c3 b0 12 	. . . 
	call s15ech		;12c1	cd ec 15 	. . . 
l12c4h:
	push psw			;12c4	f5 	. 
	lxi h,l268fh		;12c5	21 8f 26 	. . & 
	add m			;12c8	86 	. 
	lxi h,l2680h		;12c9	21 80 26 	. . & 
	sub m			;12cc	96 	. 
	jc l12f5h		;12cd	da f5 12 	. . . 
	inr a			;12d0	3c 	< 
	lxi h,l2685h		;12d1	21 85 26 	. . & 
	cmp m			;12d4	be 	. 
	jnc l12f5h		;12d5	d2 f5 12 	. . . 
	mov a,m			;12d8	7e 	~ 
	dcr a			;12d9	3d 	= 
	jm l12f5h		;12da	fa f5 12 	. . . 
	call s1660h		;12dd	cd 60 16 	. ` . 
	jz l12f5h		;12e0	ca f5 12 	. . . 
	lxi h,l26a1h+1		;12e3	21 a2 26 	. . & 
	mvi m,001h		;12e6	36 01 	6 . 
	call s137ch		;12e8	cd 7c 13 	. | . 
	call s1728h		;12eb	cd 28 17 	. ( . 
	call s0208h		;12ee	cd 08 02 	. . . 
	pop psw			;12f1	f1 	. 
	jmp l12c4h		;12f2	c3 c4 12 	. . . 
l12f5h:
	pop psw			;12f5	f1 	. 
s12f6h:
	mov d,a			;12f6	57 	W 
l12f7h:
	lhld CURXPOS		;12f7	2a 83 26 	* . & 
	push h			;12fa	e5 	. 
	mvi l,0			;12fb	2e 00 	. . 
	call s1374h		;12fd	cd 74 13 	. t . 
	jmp l1332h		;1300	c3 32 13 	. 2 . 
	call s15ech		;1303	cd ec 15 	. . . 
s1306h:
	call s1358h		;1306	cd 58 13 	. X . 
	lhld CURXPOS		;1309	2a 83 26 	* . & 
	push h			;130c	e5 	. 
	lxi h,00000h		;130d	21 00 00 	. . . 
	shld CURXPOS		;1310	22 83 26 	" . & 
	push psw			;1313	f5 	. 
	mov l,a			;1314	6f 	o 
	call s1816h		;1315	cd 16 18 	. . . 
	lhld 02756h		;1318	2a 56 27 	* V ' 
	call l0ee5h		;131b	cd e5 0e 	. . . 
	pop psw			;131e	f1 	. 
	mov b,a			;131f	47 	G 
	lxi h,l2685h		;1320	21 85 26 	. . & 
	call s13fch		;1323	cd fc 13 	. . . 
	lda CURXPOS		;1326	3a 83 26 	: . & 
	sub b			;1329	90 	. 
	jp l132eh		;132a	f2 2e 13 	. . . 
	xra a			;132d	af 	. 
l132eh:
	mov d,a			;132e	57 	W 
	lxi h,00000h		;132f	21 00 00 	. . . 
l1332h:
	shld CURXPOS		;1332	22 83 26 	" . & 
	call s186eh		;1335	cd 6e 18 	. n . 
	call s15feh		;1338	cd fe 15 	. . . 
	call s1612h		;133b	cd 12 16 	. . . 
	call l0ee5h		;133e	cd e5 0e 	. . . 
	pop h			;1341	e1 	. 
	shld CURXPOS		;1342	22 83 26 	" . & 
	ret			;1345	c9 	. 
	call s15f3h		;1346	cd f3 15 	. . . 
	jz l1362h		;1349	ca 62 13 	. b . 
	lhld l2685h		;134c	2a 85 26 	* . & 
	call s0771h		;134f	cd 71 07 	. q . 
	lxi h,00000h		;1352	21 00 00 	. . . 
	shld l2685h		;1355	22 85 26 	" . & 
s1358h:
	push psw			;1358	f5 	. 
	lda l269ch		;1359	3a 9c 26 	: . & 
	ora a			;135c	b7 	. 
	cz s1500h		;135d	cc 00 15 	. . . 
	pop psw			;1360	f1 	. 
	ret			;1361	c9 	. 
l1362h:
	push d			;1362	d5 	. 
	lhld l2685h+2		;1363	2a 87 26 	* . & 
	dcx h			;1366	2b 	+ 
	push h			;1367	e5 	. 
	call 00774h		;1368	cd 74 07 	. t . 
	pop d			;136b	d1 	. 
	lhld l2685h		;136c	2a 85 26 	* . & 
	dad d			;136f	19 	. 
	shld l2685h		;1370	22 85 26 	" . & 
	pop d			;1373	d1 	. 
s1374h:
	lda l269ch		;1374	3a 9c 26 	: . & 
	ora a			;1377	b7 	. 
	rnz			;1378	c0 	. 
	jmp s1425h		;1379	c3 25 14 	. % . 
s137ch:
	lda l26a1h+1		;137c	3a a2 26 	: . & 
	cpi 011h		;137f	fe 11 	. . 
	jnc l1401h		;1381	d2 01 14 	. . . 
	call s1660h		;1384	cd 60 16 	. ` . 
	jz l1401h		;1387	ca 01 14 	. . . 
l138ah:
	lda l2685h		;138a	3a 85 26 	: . & 
	dcr a			;138d	3d 	= 
	jp l13a2h		;138e	f2 a2 13 	. . . 
	call s1875h		;1391	cd 75 18 	. u . 
	mvi a,001h		;1394	3e 01 	> . 
	call s12f6h		;1396	cd f6 12 	. . . 
	call s1728h		;1399	cd 28 17 	. ( . 
	cc s1425h		;139c	dc 25 14 	. % . 
	jmp l138ah		;139f	c3 8a 13 	. . . 
l13a2h:
	call s1669h		;13a2	cd 69 16 	. i . 
	xra a			;13a5	af 	. 
	sta l2680h+2		;13a6	32 82 26 	2 . & 
	lhld 02697h		;13a9	2a 97 26 	* . & 
	call l020bh		;13ac	cd 0b 02 	. . . 
	lda CURXPOS		;13af	3a 83 26 	: . & 
	lxi h,0268dh		;13b2	21 8d 26 	. . & 
	add m			;13b5	86 	. 
	mov d,a			;13b6	57 	W 
	lhld l2689h+2		;13b7	2a 8b 26 	* . & 
	call s18a3h		;13ba	cd a3 18 	. . . 
l13bdh:
	call s187fh		;13bd	cd 7f 18 	.  . 
	jc l13eah		;13c0	da ea 13 	. . . 
	call s24fch		;13c3	cd fc 24 	. . $ 
	lda CURXPOS		;13c6	3a 83 26 	: . & 
	push h			;13c9	e5 	. 
	lxi h,l2680h+2		;13ca	21 82 26 	. . & 
	add m			;13cd	86 	. 
	pop h			;13ce	e1 	. 
	cmp d			;13cf	ba 	. 
	jc l13bdh		;13d0	da bd 13 	. . . 
l13d3h:
	call s187fh		;13d3	cd 7f 18 	.  . 
	jc l13eah		;13d6	da ea 13 	. . . 
	call s246ch		;13d9	cd 6c 24 	. l $ 
	jc l13eah		;13dc	da ea 13 	. . . 
	cpi LF		;13df	fe 0a 	. . 
	jz l13eah		;13e1	ca ea 13 	. . . 
	call s24fch		;13e4	cd fc 24 	. . $ 
	jmp l13d3h		;13e7	c3 d3 13 	. . . 
l13eah:
	lhld CURXPOS		;13ea	2a 83 26 	* . & 
	shld 02697h		;13ed	22 97 26 	" . & 
	call s16d4h		;13f0	cd d4 16 	. . . 
	call s1659h		;13f3	cd 59 16 	. Y . 
	lda 0268dh		;13f6	3a 8d 26 	: . & 
	lxi h,l2691h+2		;13f9	21 93 26 	. . & 
s13fch:
	cma			;13fc	2f 	/ 
	inr a			;13fd	3c 	< 
	add m			;13fe	86 	. 
	mov m,a			;13ff	77 	w 
	ret			;1400	c9 	. 
l1401h:
	call s15ech		;1401	cd ec 15 	. . . 
	cma			;1404	2f 	/ 
	inr a			;1405	3c 	< 
	jmp l140ch		;1406	c3 0c 14 	. . . 
	call s15ech		;1409	cd ec 15 	. . . 
l140ch:
	lxi h,l2685h		;140c	21 85 26 	. . & 
	add m			;140f	86 	. 
	mov m,a			;1410	77 	w 
	jmp l143fh		;1411	c3 3f 14 	. ? . 
	call s0c14h		;1414	cd 14 0c 	. . . 
	lhld l2685h		;1417	2a 85 26 	* . & 
	inx h			;141a	23 	# 
	call s0771h		;141b	cd 71 07 	. q . 
	lhld l2680h		;141e	2a 80 26 	* . & 
	dcx h			;1421	2b 	+ 
	shld l2685h		;1422	22 85 26 	" . & 
s1425h:
	push psw			;1425	f5 	. 
	xra a			;1426	af 	. 
	sta l269bh		;1427	32 9b 26 	2 . & 
	pop psw			;142a	f1 	. 
	ret			;142b	c9 	. 
	xra a			;142c	af 	. 
	sta l26a6h+1		;142d	32 a7 26 	2 . & 
	call s1875h		;1430	cd 75 18 	. u . 
	lhld l2685h		;1433	2a 85 26 	* . & 
	xchg			;1436	eb 	. 
	lhld l2685h+2		;1437	2a 87 26 	* . & 
	dad d			;143a	19 	. 
	xra a			;143b	af 	. 
	call l074eh		;143c	cd 4e 07 	. N . 
l143fh:
	call s1425h		;143f	cd 25 14 	. % . 
	mvi a,0ffh		;1442	3e ff 	> . 
	sta l269dh+1		;1444	32 9e 26 	2 . & 
	ret			;1447	c9 	. 
l1448h:
	call s0c14h		;1448	cd 14 0c 	. . . 
s144bh:
	call l0affh		;144b	cd ff 0a 	. . . 
	call s14c5h		;144e	cd c5 14 	. . . 
	call s1656h		;1451	cd 56 16 	. V . 
	call s0c07h		;1454	cd 07 0c 	. . . 
	cpi CR		;1457	fe 0d 	. . 
	jz l1448h		;1459	ca 48 14 	. H . 
	ret			;145c	c9 	. 
l145dh:
	call s144bh		;145d	cd 4b 14 	. K . 
	call s0c14h		;1460	cd 14 0c 	. . . 
	call s22fah		;1463	cd fa 22 	. . " 
	jz l145dh		;1466	ca 5d 14 	. ] . 
l1469h:
	call s144bh		;1469	cd 4b 14 	. K . 
	call s0c14h		;146c	cd 14 0c 	. . . 
	call s0ed7h		;146f	cd d7 0e 	. . . 
	jc l1469h		;1472	da 69 14 	. i . 
	ret			;1475	c9 	. 
	call s1875h		;1476	cd 75 18 	. u . 
	call s22fah		;1479	cd fa 22 	. . " 
	jnz l148eh		;147c	c2 8e 14 	. . . 
l147fh:
	call s14efh		;147f	cd ef 14 	. . . 
	call s1875h		;1482	cd 75 18 	. u . 
	call s22fah		;1485	cd fa 22 	. . " 
	jz l147fh		;1488	ca 7f 14 	.  . 
	jmp l14b2h		;148b	c3 b2 14 	. . . 
l148eh:
	call l14b2h		;148e	cd b2 14 	. . . 
	call s0c07h		;1491	cd 07 0c 	. . . 
	jc l149bh		;1494	da 9b 14 	. . . 
	call s22fah		;1497	cd fa 22 	. . " 
	rnz			;149a	c0 	. 
l149bh:
	call s1875h		;149b	cd 75 18 	. u . 
	cpi ' '		;149e	fe 20 	.   
	jz l14a6h		;14a0	ca a6 14 	. . . 
	cpi TAB		;14a3	fe 09 	. . 
	rnz			;14a5	c0 	. 
l14a6h:
	call s14efh		;14a6	cd ef 14 	. . . 
	jmp l149bh		;14a9	c3 9b 14 	. . . 
l14ach:
	call s14efh		;14ac	cd ef 14 	. . . 
	call s1875h		;14af	cd 75 18 	. u . 
l14b2h:
	call s0ed7h		;14b2	cd d7 0e 	. . . 
	jc l14ach		;14b5	da ac 14 	. . . 
	call s0ed2h		;14b8	cd d2 0e 	. . . 
	cnc s14efh		;14bb	d4 ef 14 	. . . 
	ret			;14be	c9 	. 
s14bfh:
	call s0806h		;14bf	cd 06 08 	. . . 
	call l0778h		;14c2	cd 78 07 	. x . 
s14c5h:
	mvi e,0ffh		;14c5	1e ff 	. . 
l14c7h:
	push psw			;14c7	f5 	. 
	xra a			;14c8	af 	. 
	sta l269ch		;14c9	32 9c 26 	2 . & 
	pop psw			;14cc	f1 	. 
	ret			;14cd	c9 	. 
	call s14bfh		;14ce	cd bf 14 	. . . 
	call s1500h		;14d1	cd 00 15 	. . . 
	call s077eh		;14d4	cd 7e 07 	. ~ . 
	push d			;14d7	d5 	. 
	call l0778h		;14d8	cd 78 07 	. x . 
	pop d			;14db	d1 	. 
	ret			;14dc	c9 	. 
l14ddh:
	call s1875h		;14dd	cd 75 18 	. u . 
	cpi CR		;14e0	fe 0d 	. . 
	rz			;14e2	c8 	. 
	cpi LF		;14e3	fe 0a 	. . 
	rz			;14e5	c8 	. 
	call s14efh		;14e6	cd ef 14 	. . . 
	jmp l14ddh		;14e9	c3 dd 14 	. . . 
l14ech:
	call s1875h		;14ec	cd 75 18 	. u . 
s14efh:
	call s14c5h		;14ef	cd c5 14 	. . . 
l14f2h:
	shld 0273fh		;14f2	22 3f 27 	" ? ' 
	call s1d9bh		;14f5	cd 9b 1d 	. . . 
	cpi CR		;14f8	fe 0d 	. . 
	jz l14ech		;14fa	ca ec 14 	. . . 
	cpi LF		;14fd	fe 0a 	. . 
	rnz			;14ff	c0 	. 
s1500h:
	mvi e,0ffh		;1500	1e ff 	. . 
s1502h:
	push h			;1502	e5 	. 
	lxi h,00000h		;1503	21 00 00 	. . . 
	shld l269ch		;1506	22 9c 26 	" . & 
	pop h			;1509	e1 	. 
	ret			;150a	c9 	. 
	lxi h,l26a1h+2		;150b	21 a3 26 	. . & 
	mov a,m			;150e	7e 	~ 
	cma			;150f	2f 	/ 
	mov m,a			;1510	77 	w 
l1511h:
	push h			;1511	e5 	. 
	lxi h,0ffffh		;1512	21 ff ff 	. . . 
	shld l2699h		;1515	22 99 26 	" . & 
	pop h			;1518	e1 	. 
	ret			;1519	c9 	. 
	call l0f64h		;151a	cd 64 0f 	. d . 
	call s18b4h		;151d	cd b4 18 	. . . 
	cpi EOF		;1520	fe 1a 	. . 
	jnz l1530h		;1522	c2 30 15 	. 0 . 
	call s0904h		;1525	cd 04 09 	. . . 
	call l080fh		;1528	cd 0f 08 	. . . 
	call s14c5h		;152b	cd c5 14 	. . . 
	mvi a,EOF		;152e	3e 1a 	> . 
l1530h:
	cpi CR		;1530	fe 0d 	. . 
	jz l15abh		;1532	ca ab 15 	. . . 
	cpi LF		;1535	fe 0a 	. . 
	cz s1425h		;1537	cc 25 14 	. % . 
l153ah:
	push psw			;153a	f5 	. 
	lhld l2691h		;153b	2a 91 26 	* . & 
	call l020bh		;153e	cd 0b 02 	. . . 
	pop psw			;1541	f1 	. 
	push d			;1542	d5 	. 
	mvi e,000h		;1543	1e 00 	. . 
	call s2421h		;1545	cd 21 24 	. . $ 
	pop d			;1548	d1 	. 
	call s0bach		;1549	cd ac 0b 	. . . 
	mov d,a			;154c	57 	W 
	call s187ch		;154d	cd 7c 18 	. | . 
	jc l1579h		;1550	da 79 15 	. y . 
	cpi CR		;1553	fe 0d 	. . 
	jz l1579h		;1555	ca 79 15 	. y . 
	mov b,a			;1558	47 	G 
	lda l26a1h+2		;1559	3a a3 26 	: . & 
	ora a			;155c	b7 	. 
	mov a,b			;155d	78 	x 
	jnz l14c7h		;155e	c2 c7 14 	. . . 
	cmp d			;1561	ba 	. 
	jz l1573h		;1562	ca 73 15 	. s . 
	cpi ' '		;1565	fe 20 	.   
	jc s14efh		;1567	da ef 14 	. . . 
	mov a,d			;156a	7a 	z 
	cpi ' '		;156b	fe 20 	.   
	cc l14c7h		;156d	dc c7 14 	. . . 
	jc l14f2h		;1570	da f2 14 	. . . 
l1573h:
	call l14f2h		;1573	cd f2 14 	. . . 
	jmp l1583h		;1576	c3 83 15 	. . . 
l1579h:
	lda CURXPOS		;1579	3a 83 26 	: . & 
	lxi h,l268fh		;157c	21 8f 26 	. . & 
	cmp m			;157f	be 	. 
	jnz l14c7h		;1580	c2 c7 14 	. . . 
l1583h:
	push h			;1583	e5 	. 
	call s1856h		;1584	cd 56 18 	. V . 
	shld l2691h		;1587	22 91 26 	" . & 
	lhld CURXPOS		;158a	2a 83 26 	* . & 
	shld l268fh		;158d	22 8f 26 	" . & 
	pop h			;1590	e1 	. 
	jmp l1511h		;1591	c3 11 15 	. . . 
	call s15ech		;1594	cd ec 15 	. . . 
l1597h:
	push psw			;1597	f5 	. 
	call s15e2h		;1598	cd e2 15 	. . . 
	lhld 0273dh		;159b	2a 3d 27 	* = ' 
	dcx h			;159e	2b 	+ 
	dcx h			;159f	2b 	+ 
	call l0ee5h		;15a0	cd e5 0e 	. . . 
	pop psw			;15a3	f1 	. 
	dcr a			;15a4	3d 	= 
	jnz l1597h		;15a5	c2 97 15 	. . . 
	jmp s1500h		;15a8	c3 00 15 	. . . 
l15abh:
	call s164fh		;15ab	cd 4f 16 	. O . 
	lda l26a1h+2		;15ae	3a a3 26 	: . & 
	ora a			;15b1	b7 	. 
	jz l15cbh		;15b2	ca cb 15 	. . . 
	call s187ch		;15b5	cd 7c 18 	. | . 
	jc l15c2h		;15b8	da c2 15 	. . . 
	cpi CR		;15bb	fe 0d 	. . 
	jz l15c2h		;15bd	ca c2 15 	. . . 
	mvi e,0ffh		;15c0	1e ff 	. . 
l15c2h:
	call s241fh		;15c2	cd 1f 24 	. . $ 
	call s0ba2h		;15c5	cd a2 0b 	. . . 
	jmp s14c5h		;15c8	c3 c5 14 	. . . 
l15cbh:
	call s1374h		;15cb	cd 74 13 	. t . 
	call s186eh		;15ce	cd 6e 18 	. n . 
l15d1h:
	call s187fh		;15d1	cd 7f 18 	.  . 
	jc l15dfh		;15d4	da df 15 	. . . 
	cpi LF		;15d7	fe 0a 	. . 
	jnz l15d1h		;15d9	c2 d1 15 	. . . 
	jmp l0ee5h		;15dc	c3 e5 0e 	. . . 
l15dfh:
	call l0ee5h		;15df	cd e5 0e 	. . . 
s15e2h:
	mvi a,CR		;15e2	3e 0d 	> . 
	call s0bach		;15e4	cd ac 0b 	. . . 
	mvi a,LF		;15e7	3e 0a 	> . 
	jmp s0bach		;15e9	c3 ac 0b 	. . . 
s15ech:
	lxi h,l26a1h+1		;15ec	21 a2 26 	. . & 
	mov a,m			;15ef	7e 	~ 
	mvi m,000h		;15f0	36 00 	6 . 
	ret			;15f2	c9 	. 
s15f3h:
	lxi h,l26a0h		;15f3	21 a0 26 	. . & 
	cmp m			;15f6	be 	. 
	ret			;15f7	c9 	. 
l15f8h:
	call s249ah		;15f8	cd 9a 24 	. . $ 
	call s164ch		;15fb	cd 4c 16 	. L . 
s15feh:
	lda CURXPOS		;15fe	3a 83 26 	: . & 
	cmp d			;1601	ba 	. 
	rnc 			;1602	d0 	. 
	call s187fh		;1603	cd 7f 18 	.  . 
	rc			;1606	d8 	. 
	call s185dh		;1607	cd 5d 18 	. ] . 
	jnc l15f8h		;160a	d2 f8 15 	. . . 
	dcx h			;160d	2b 	+ 
	inx b			;160e	03 	. 
	jmp s15feh		;160f	c3 fe 15 	. . . 
s1612h:
	lda l268fh+1		;1612	3a 90 26 	: . & 
	inr a			;1615	3c 	< 
	mov d,a			;1616	57 	W 
	lda CURYPOS		;1617	3a 84 26 	: . & 
	cmp d			;161a	ba 	. 
	rnc 			;161b	d0 	. 
	call s187fh		;161c	cd 7f 18 	.  . 
	rc			;161f	d8 	. 
	call s246ch		;1620	cd 6c 24 	. l $ 
	jc l1649h		;1623	da 49 16 	. I . 
l1626h:
	cpi LF		;1626	fe 0a 	. . 
	jz l1649h		;1628	ca 49 16 	. I . 
	cpi CR		;162b	fe 0d 	. . 
	jz l1649h		;162d	ca 49 16 	. I . 
	call s246ch		;1630	cd 6c 24 	. l $ 
	jc l1647h		;1633	da 47 16 	. G . 
	call s249ah		;1636	cd 9a 24 	. . $ 
	lda CURYPOS		;1639	3a 84 26 	: . & 
	cmp d			;163c	ba 	. 
	jnc l1649h		;163d	d2 49 16 	. I . 
	call s187fh		;1640	cd 7f 18 	.  . 
	rc			;1643	d8 	. 
	jmp l1626h		;1644	c3 26 16 	. & . 
l1647h:
	dcx h			;1647	2b 	+ 
	inx b			;1648	03 	. 
l1649h:
	dcx h			;1649	2b 	+ 
	inx b			;164a	03 	. 
	ret			;164b	c9 	. 
s164ch:
	cpi LF		;164c	fe 0a 	. . 
	rnz			;164e	c0 	. 
s164fh:
	push h			;164f	e5 	. 
	lxi h,l2685h		;1650	21 85 26 	. . & 
	inr m			;1653	34 	4 
	pop h			;1654	e1 	. 
	ret			;1655	c9 	. 
s1656h:
	cpi LF		;1656	fe 0a 	. . 
	rnz			;1658	c0 	. 
s1659h:
	push h			;1659	e5 	. 
	lxi h,l2685h		;165a	21 85 26 	. . & 
	dcr m			;165d	35 	5 
	pop h			;165e	e1 	. 
	ret			;165f	c9 	. 
s1660h:
	lxi h,l269dh		;1660	21 9d 26 	. . & 
	mov a,m			;1663	7e 	~ 
	dcx h			;1664	2b 	+ 
	ana m			;1665	a6 	. 
	dcx h			;1666	2b 	+ 
	ana m			;1667	a6 	. 
	ret			;1668	c9 	. 
s1669h:
	lda l2699h		;1669	3a 99 26 	: . & 
	ora a			;166c	b7 	. 
	rz			;166d	c8 	. 
	jmp s1674h		;166e	c3 74 16 	. t . 
l1671h:
	call s1425h		;1671	cd 25 14 	. % . 
s1674h:
	call s1728h		;1674	cd 28 17 	. ( . 
	jnc l1722h		;1677	d2 22 17 	. " . 
	call s1659h		;167a	cd 59 16 	. Y . 
	jp l16a9h		;167d	f2 a9 16 	. . . 
l1680h:
	call s0534h		;1680	cd 34 05 	. 4 . 
	db 'FILE LINE LONGER THAN ENTIRE SCRN ???',0
l16a9h:
	lda l269bh		;16a9	3a 9b 26 	: . & 
	lxi h,0268dh		;16ac	21 8d 26 	. . & 
	ana m			;16af	a6 	. 
	jz l1671h		;16b0	ca 71 16 	. q . 
	lda l268fh		;16b3	3a 8f 26 	: . & 
	lxi h,l2680h		;16b6	21 80 26 	. . & 
	sub m			;16b9	96 	. 
	jc l16c7h		;16ba	da c7 16 	. . . 
	jnz l1671h		;16bd	c2 71 16 	. q . 
	lda l268fh+1		;16c0	3a 90 26 	: . & 
	ora a			;16c3	b7 	. 
	jnz l1671h		;16c4	c2 71 16 	. q . 
l16c7h:
	call s16d0h		;16c7	cd d0 16 	. . . 
	call s1502h		;16ca	cd 02 15 	. . . 
	jmp s1674h		;16cd	c3 74 16 	. t . 
s16d0h:
	xra a			;16d0	af 	. 
	sta l2680h+2		;16d1	32 82 26 	2 . & 
s16d4h:
	mvi a,LF		;16d4	3e 0a 	> . 
	call s24fch		;16d6	cd fc 24 	. . $ 
	lda l2680h+2		;16d9	3a 82 26 	: . & 
	ora a			;16dc	b7 	. 
	jz l16e8h		;16dd	ca e8 16 	. . . 
	lxi h,02697h		;16e0	21 97 26 	. . & 
	dcr m			;16e3	35 	5 
	jp l16e8h		;16e4	f2 e8 16 	. . . 
	inr m			;16e7	34 	4 
l16e8h:
	lxi h,0268dh		;16e8	21 8d 26 	. . & 
	cmp m			;16eb	be 	. 
	jc s16d4h		;16ec	da d4 16 	. . . 
	ret			;16ef	c9 	. 
s16f0h:
	lxi h,l2685h		;16f0	21 85 26 	. . & 
	mov a,m			;16f3	7e 	~ 
l16f4h:
	ora a			;16f4	b7 	. 
	jp l170dh		;16f5	f2 0d 17 	. . . 
	push h			;16f8	e5 	. 
	call s187ch		;16f9	cd 7c 18 	. | . 
	pop h			;16fc	e1 	. 
	jnc l1705h		;16fd	d2 05 17 	. . . 
	mvi m,000h		;1700	36 00 	6 . 
	jmp l170dh		;1702	c3 0d 17 	. . . 
l1705h:
	xra a			;1705	af 	. 
	sub m			;1706	96 	. 
	call s12f6h		;1707	cd f6 12 	. . . 
	jmp s16f0h		;170a	c3 f0 16 	. . . 
l170dh:
	call s1728h		;170d	cd 28 17 	. ( . 
	jnc l1722h		;1710	d2 22 17 	. " . 
	push psw			;1713	f5 	. 
	lda l2685h		;1714	3a 85 26 	: . & 
	ora a			;1717	b7 	. 
	jz l1680h		;1718	ca 80 16 	. . . 
	pop psw			;171b	f1 	. 
	call s1306h		;171c	cd 06 13 	. . . 
	jmp l170dh		;171f	c3 0d 17 	. . . 
l1722h:
	lxi h,l2699h		;1722	21 99 26 	. . & 
	mvi m,000h		;1725	36 00 	6 . 
	ret			;1727	c9 	. 
s1728h:
	push b			;1728	c5 	. 
	push d			;1729	d5 	. 
	push h			;172a	e5 	. 
	lhld CURXPOS		;172b	2a 83 26 	* . & 
	push h			;172e	e5 	. 
	xra a			;172f	af 	. 
	sta 0268dh		;1730	32 8d 26 	2 . & 
	lda l2685h		;1733	3a 85 26 	: . & 
	dcr a			;1736	3d 	= 
	call s1801h		;1737	cd 01 18 	. . . 
	inr a			;173a	3c 	< 
	jc l1785h		;173b	da 85 17 	. . . 
	lxi h,00000h		;173e	21 00 00 	. . . 
	shld l2685h+2		;1741	22 87 26 	" . & 
	shld CURXPOS		;1744	22 83 26 	" . & 
	lhld l2685h		;1747	2a 85 26 	* . & 
	call s1816h		;174a	cd 16 18 	. . . 
	shld l2691h		;174d	22 91 26 	" . & 
	mov a,d			;1750	7a 	z 
	sta 0268dh		;1751	32 8d 26 	2 . & 
	lhld CURXPOS		;1754	2a 83 26 	* . & 
	shld l268fh		;1757	22 8f 26 	" . & 
	lhld 02756h		;175a	2a 56 27 	* V ' 
	xchg			;175d	eb 	. 
	lhld 0273dh		;175e	2a 3d 27 	* = ' 
	call s1f7ch		;1761	cd 7c 1f 	. | . 
	inr h			;1764	24 	$ 
	shld 02745h		;1765	22 45 27 	" E ' 
	call s178ch		;1768	cd 8c 17 	. . . 
	xchg			;176b	eb 	. 
	lhld CURXPOS		;176c	2a 83 26 	* . & 
	shld 02695h		;176f	22 95 26 	" . & 
	xchg			;1772	eb 	. 
	call s179eh		;1773	cd 9e 17 	. . . 
	shld l2689h		;1776	22 89 26 	" . & 
	call s1899h		;1779	cd 99 18 	. . . 
	shld l2689h+2		;177c	22 8b 26 	" . & 
	lda 02695h		;177f	3a 95 26 	: . & 
	call s1801h		;1782	cd 01 18 	. . . 
l1785h:
	pop h			;1785	e1 	. 
	shld CURXPOS		;1786	22 83 26 	" . & 
	jmp POPHDB		;1789	c3 cc 1f 	. . . 
s178ch:
	call s186eh		;178c	cd 6e 18 	. n . 
l178fh:
	call s187fh		;178f	cd 7f 18 	.  . 
	rc			;1792	d8 	. 
	cpi LF		;1793	fe 0a 	. . 
	jz l1649h		;1795	ca 49 16 	. I . 
	call s249ah		;1798	cd 9a 24 	. . $ 
	jmp l178fh		;179b	c3 8f 17 	. . . 
s179eh:
	call s1807h		;179e	cd 07 18 	. . . 
	jc l17c9h		;17a1	da c9 17 	. . . 
	call s187fh		;17a4	cd 7f 18 	.  . 
	rc			;17a7	d8 	. 
	cpi LF		;17a8	fe 0a 	. . 
	cz s17e3h		;17aa	cc e3 17 	. . . 
	cpi ' '		;17ad	fe 20 	.   
	jc l17c3h		;17af	da c3 17 	. . . 
	push h			;17b2	e5 	. 
	lxi h,CURYPOS		;17b3	21 84 26 	. . & 
	inr m			;17b6	34 	4 
	lda WID		;17b7	3a ba 01 	: . . 
	dcr a			;17ba	3d 	= 
	cmp m			;17bb	be 	. 
	pop h			;17bc	e1 	. 
	jnc s179eh		;17bd	d2 9e 17 	. . . 
	dcx h			;17c0	2b 	+ 
	mov a,m			;17c1	7e 	~ 
	inx h			;17c2	23 	# 
l17c3h:
	call s249ah		;17c3	cd 9a 24 	. . $ 
	jmp s179eh		;17c6	c3 9e 17 	. . . 
l17c9h:
	call s187fh		;17c9	cd 7f 18 	.  . 
	rc			;17cc	d8 	. 
	call s246ch		;17cd	cd 6c 24 	. l $ 
	jc l1649h		;17d0	da 49 16 	. I . 
	cpi LF		;17d3	fe 0a 	. . 
	jz l17deh		;17d5	ca de 17 	. . . 
	call s249ah		;17d8	cd 9a 24 	. . $ 
	jmp l17c9h		;17db	c3 c9 17 	. . . 
l17deh:
	dcx h			;17de	2b 	+ 
	inx b			;17df	03 	. 
	cpi LF		;17e0	fe 0a 	. . 
	rnz			;17e2	c0 	. 
s17e3h:
	push h			;17e3	e5 	. 
	lxi h,l2685h+2		;17e4	21 87 26 	. . & 
	inr m			;17e7	34 	4 
	pop h			;17e8	e1 	. 
	ret			;17e9	c9 	. 
s17eah:
	push d			;17ea	d5 	. 
	call s0de3h		;17eb	cd e3 0d 	. . . 
	push psw			;17ee	f5 	. 
	xchg			;17ef	eb 	. 
	lhld 0273dh		;17f0	2a 3d 27 	* = ' 
	call s1f85h		;17f3	cd 85 1f 	. . . 
	xchg			;17f6	eb 	. 
	pop psw			;17f7	f1 	. 
	pop d			;17f8	d1 	. 
	ret			;17f9	c9 	. 
s17fah:
	mov a,b			;17fa	78 	x 
	ora c			;17fb	b1 	. 
	rz			;17fc	c8 	. 
	mov a,m			;17fd	7e 	~ 
	inx h			;17fe	23 	# 
	dcx b			;17ff	0b 	. 
	ret			;1800	c9 	. 
s1801h:
	dcr a			;1801	3d 	= 
	sta CURXPOS		;1802	32 83 26 	2 . & 
	ora a			;1805	b7 	. 
	rm			;1806	f8 	. 
s1807h:
	lda CURXPOS		;1807	3a 83 26 	: . & 
	inr a			;180a	3c 	< 
	inr a			;180b	3c 	< 
	mov d,a			;180c	57 	W 
	lda l2680h		;180d	3a 80 26 	: . & 
	sub d			;1810	92 	. 
	rnc 			;1811	d0 	. 
	cma			;1812	2f 	/ 
	inr a			;1813	3c 	< 
	stc			;1814	37 	7 
	ret			;1815	c9 	. 
s1816h:
	call s17eah		;1816	cd ea 17 	. . . 
	mvi a,07bh		;1819	3e 7b 	> { 
	cc s2497h		;181b	dc 97 24 	. . $ 
	mvi d,000h		;181e	16 00 	. . 
l1820h:
	call s17fah		;1820	cd fa 17 	. . . 
	jz s1856h		;1823	ca 56 18 	. V . 
	call s249ah		;1826	cd 9a 24 	. . $ 
	cpi LF		;1829	fe 0a 	. . 
	jnz l1820h		;182b	c2 20 18 	.   . 
	lda CURXPOS		;182e	3a 83 26 	: . & 
	mov d,a			;1831	57 	W 
l1832h:
	mov a,b			;1832	78 	x 
	ora c			;1833	b1 	. 
	jz s1856h		;1834	ca 56 18 	. V . 
	mov a,m			;1837	7e 	~ 
	inx h			;1838	23 	# 
	dcx b			;1839	0b 	. 
	cpi ' '		;183a	fe 20 	.   
	jc l1850h		;183c	da 50 18 	. P . 
	push h			;183f	e5 	. 
	lxi h,CURYPOS		;1840	21 84 26 	. . & 
	inr m			;1843	34 	4 
	lda WID		;1844	3a ba 01 	: . . 
	dcr a			;1847	3d 	= 
	cmp m			;1848	be 	. 
	pop h			;1849	e1 	. 
	jnc l1832h		;184a	d2 32 18 	. 2 . 
	dcx h			;184d	2b 	+ 
	mov a,m			;184e	7e 	~ 
	inx h			;184f	23 	# 
l1850h:
	call s249ah		;1850	cd 9a 24 	. . $ 
	jmp l1832h		;1853	c3 32 18 	. 2 . 
s1856h:
	call s187ch		;1856	cd 7c 18 	. | . 
	lhld CURXPOS		;1859	2a 83 26 	* . & 
	rc			;185c	d8 	. 
s185dh:
	call s246ch		;185d	cd 6c 24 	. l $ 
	rnc 			;1860	d0 	. 
	push h			;1861	e5 	. 
	lhld CURXPOS		;1862	2a 83 26 	* . & 
	inr l			;1865	2c 	, 
	mvi h,002h		;1866	26 02 	& . 
	shld CURXPOS		;1868	22 83 26 	" . & 
	pop h			;186b	e1 	. 
	stc			;186c	37 	7 
	ret			;186d	c9 	. 
s186eh:
	call s1ef8h		;186e	cd f8 1e 	. . . 
	call s1f95h		;1871	cd 95 1f 	. . . 
	ret			;1874	c9 	. 
s1875h:
	call s187ch		;1875	cd 7c 18 	. | . 
	rnc 			;1878	d0 	. 
	inx sp			;1879	33 	3 
	inx sp			;187a	33 	3 
	ret			;187b	c9 	. 
s187ch:
	call s186eh		;187c	cd 6e 18 	. n . 
s187fh:
	mov a,b			;187f	78 	x 
	ora c			;1880	b1 	. 
	jnz l1895h		;1881	c2 95 18 	. . . 
	call s1899h		;1884	cd 99 18 	. . . 
	push h			;1887	e5 	. 
	call s1c0bh		;1888	cd 0b 1c 	. . . 
	pop h			;188b	e1 	. 
	push psw			;188c	f5 	. 
	call l234eh		;188d	cd 4e 23 	. N # 
	call s18a3h		;1890	cd a3 18 	. . . 
	pop psw			;1893	f1 	. 
	rc			;1894	d8 	. 
l1895h:
	mov a,m			;1895	7e 	~ 
	dcx b			;1896	0b 	. 
	inx h			;1897	23 	# 
	ret			;1898	c9 	. 
s1899h:
	push d			;1899	d5 	. 
	xchg			;189a	eb 	. 
	lhld 0273fh		;189b	2a 3f 27 	* ? ' 
	call s1f7bh		;189e	cd 7b 1f 	. { . 
	pop d			;18a1	d1 	. 
	ret			;18a2	c9 	. 
s18a3h:
	push d			;18a3	d5 	. 
	xchg			;18a4	eb 	. 
	call s1ef8h		;18a5	cd f8 1e 	. . . 
	call s1f95h		;18a8	cd 95 1f 	. . . 
	dad d			;18ab	19 	. 
	mov a,c			;18ac	79 	y 
	sub e			;18ad	93 	. 
	mov c,a			;18ae	4f 	O 
	mov a,b			;18af	78 	x 
	sbb d			;18b0	9a 	. 
	mov b,a			;18b1	47 	G 
	pop d			;18b2	d1 	. 
	ret			;18b3	c9 	. 
s18b4h:
	call s18beh		;18b4	cd be 18 	. . . 
	rnc 			;18b7	d0 	. 
	call s0f2bh		;18b8	cd 2b 0f 	. + . 
	jmp s18b4h		;18bb	c3 b4 18 	. . . 
s18beh:
	push b			;18be	c5 	. 
	push d			;18bf	d5 	. 
	push h			;18c0	e5 	. 
l18c1h:
	lda 02749h		;18c1	3a 49 27 	: I ' 
	ora a			;18c4	b7 	. 
	jnz l18ddh		;18c5	c2 dd 18 	. . . 
	call s18e3h		;18c8	cd e3 18 	. . . 
	jc POPHDB		;18cb	da cc 1f 	. . . 
	mvi d,0c9h		;18ce	16 c9 	. . 
l18d0h:
	dcr d			;18d0	15 	. 
	jz l18c1h		;18d1	ca c1 18 	. . . 
	call l234eh		;18d4	cd 4e 23 	. N # 
	jz l18d0h		;18d7	ca d0 18 	. . . 
	call s18e3h		;18da	cd e3 18 	. . . 
l18ddh:
	cnc s2322h		;18dd	d4 22 23 	. " # 
	jmp POPHDB		;18e0	c3 cc 1f 	. . . 
s18e3h:
	lda 02749h		;18e3	3a 49 27 	: I ' 
	ora a			;18e6	b7 	. 
	rnz			;18e7	c0 	. 
	lda 0f7ffh		;18e8	3a ff f7 	: . . 
	lxi h,0274ah		;18eb	21 4a 27 	. J ' 
	cmp m			;18ee	be 	. 
	rz			;18ef	c8 	. 
	lda 02749h		;18f0	3a 49 27 	: I ' 
	ora a			;18f3	b7 	. 
	rnz			;18f4	c0 	. 
	call s1425h		;18f5	cd 25 14 	. % . 
	call s01f2h		;18f8	cd f2 01 	. . . 
	lxi h,l25a9h		;18fb	21 a9 25 	. . % 
	shld l25a7h		;18fe	22 a7 25 	" . % 
s1901h:
	lda 02749h		;1901	3a 49 27 	: I ' 
	ora a			;1904	b7 	. 
	rnz			;1905	c0 	. 
	lda 0f7ffh		;1906	3a ff f7 	: . . 
	push psw			;1909	f5 	. 
	rar			;190a	1f 	. 
	push psw			;190b	f5 	. 
	mvi a,050h		;190c	3e 50 	> P 
	jnc l1913h		;190e	d2 13 19 	. . . 
	mvi a,028h		;1911	3e 28 	> ( 
l1913h:
	sta WID		;1913	32 ba 01 	2 . . 
	pop psw			;1916	f1 	. 
	rar			;1917	1f 	. 
	mvi a,018h		;1918	3e 18 	> . 
	jnc l191fh		;191a	d2 1f 19 	. . . 
	mvi a,00ch		;191d	3e 0c 	> . 
l191fh:
	sta HITE		;191f	32 b9 01 	2 . . 
	sui 002h		;1922	d6 02 	. . 
	sta l2680h		;1924	32 80 26 	2 . & 
	mvi a,015h		;1927	3e 15 	> . 
	sta EREOL		;1929	32 bb 01 	2 . . 
	pop psw			;192c	f1 	. 
	sta 0274ah		;192d	32 4a 27 	2 J ' 
	stc			;1930	37 	7 
	ret			;1931	c9 	. 
l1932h:
	db 0,'WM      ','HLP'
	nop			;193e	00 	. 
	nop			;193f	00 	. 
	nop			;1940	00 	. 
	nop			;1941	00 	. 
	nop			;1942	00 	. 
	nop			;1943	00 	. 
	nop			;1944	00 	. 
	nop			;1945	00 	. 
	nop			;1946	00 	. 
	nop			;1947	00 	. 
	nop			;1948	00 	. 
	nop			;1949	00 	. 
	nop			;194a	00 	. 
	nop			;194b	00 	. 
	nop			;194c	00 	. 
	nop			;194d	00 	. 
	nop			;194e	00 	. 
	nop			;194f	00 	. 
	nop			;1950	00 	. 
	nop			;1951	00 	. 
	nop			;1952	00 	. 
	nop			;1953	00 	. 
l1954h:
	call s01f2h		;1954	cd f2 01 	. . . 
l1957h:
	call s1425h		;1957	cd 25 14 	. % . 
	lxi h,l1932h		;195a	21 32 19 	. 2 . 
	lxi d,l26f6h+2		;195d	11 f8 26 	. . & 
	lxi b,00021h		;1960	01 21 00 	. . . 
	call MEMCPY		;1963	cd 0c 1f 	. . . 
	lxi d,l26f6h+2		;1966	11 f8 26 	. . & 
	call s1ff4h		;1969	cd f4 1f 	. . . 
	jnz 019a2h		;196c	c2 a2 19 	. . . 
	mvi a,001h		;196f	3e 01 	> . 
	sta l26f6h+2		;1971	32 f8 26 	2 . & 
	lxi d,l26f6h+2		;1974	11 f8 26 	. . & 
	call s1ff4h		;1977	cd f4 1f 	. . . 
	jnz 019a2h		;197a	c2 a2 19 	. . . 
	call s0534h		;197d	cd 34 05 	. 4 . 
	db 'WM.HLP NOT FOUND ON DEFAULT OR A:',0	;1980
	lxi d,026f8h		;19a2	11 f8 26
	lxi h,027e9h		;19a5	21 e9 27
	call s20a9h		;19a8	cd a9 20 	. .   
	jc l19deh		;19ab	da de 19 	. . . 
	lxi d,l26f6h+2		;19ae	11 f8 26 	. . & 
	lxi h,027e9h		;19b1	21 e9 27 	. . ' 
	call s1c8fh		;19b4	cd 8f 1c 	. . . 
	lxi h,027e9h		;19b7	21 e9 27 	. . ' 
l19bah:
	push psw			;19ba	f5 	. 
	mov a,m			;19bb	7e 	~ 
	inx h			;19bc	23 	# 
	cpi 011h		;19bd	fe 11 	. . 
	jz l19cdh		;19bf	ca cd 19 	. . . 
	call s24fch		;19c2	cd fc 24 	. . $ 
l19c5h:
	pop psw			;19c5	f1 	. 
	dcr a			;19c6	3d 	= 
	jnz l19bah		;19c7	c2 ba 19 	. . . 
	jmp 019a2h		;19ca	c3 a2 19 	. . . 
l19cdh:
	call s18b4h		;19cd	cd b4 18 	. . . 
	cpi LF		;19d0	fe 0a 	. . 
l19d2h:
	jnz l19ddh		;19d2	c2 dd 19 	. . . 
	push h			;19d5	e5 	. 
l19d6h:
	call s01f2h		;19d6	cd f2 01 	. . . 
l19d9h:
	pop h			;19d9	e1 	. 
	jmp l19c5h		;19da	c3 c5 19 	. . . 
l19ddh:
	pop psw			;19dd	f1 	. 
l19deh:
	ret			;19de	c9 	. 
l19dfh:
	lxi d,l26f6h+2		;19df	11 f8 26 	. . & 
	call s0966h		;19e2	cd 66 09 	. f . 
	call s1ff4h		;19e5	cd f4 1f 	. . . 
	jnz l19fdh		;19e8	c2 fd 19 	. . . 
	call s0534h		;19eb	cd 34 05 	. 4 . 
	db 'FILE NOT FOUND',0
l19fdh:
	call s23d6h		;19fd	cd d6 23 	. . # 
	lxi d,l26f6h+2		;1a00	11 f8 26 	. . & 
	lxi h,027e9h		;1a03	21 e9 27 	. . ' 
	call s20a9h		;1a06	cd a9 20 	. .   
	jc l1a1dh		;1a09	da 1d 1a 	. . . 
	call s1c8fh		;1a0c	cd 8f 1c 	. . . 
	push psw			;1a0f	f5 	. 
	mvi b,000h		;1a10	06 00 	. . 
	mov c,a			;1a12	4f 	O 
	lxi d,027e9h		;1a13	11 e9 27 	. . ' 
	call l0c29h		;1a16	cd 29 0c 	. ) . 
	pop psw			;1a19	f1 	. 
	jc l19fdh		;1a1a	da fd 19 	. . . 
l1a1dh:
	jmp s081ah		;1a1d	c3 1a 08 	. . . 
l1a20h:
	call s0828h		;1a20	cd 28 08 	. ( . 
	jz l051dh		;1a23	ca 1d 05 	. . . 
	lxi d,l26f6h		;1a26	11 f6 26 	. . & 
	xra a			;1a29	af 	. 
	stax d			;1a2a	12 	. 
	inx d			;1a2b	13 	. 
	inx d			;1a2c	13 	. 
	call s0966h		;1a2d	cd 66 09 	. f . 
	lxi d,00000h		;1a30	11 00 00 	. . . 
l1a33h:
	lxi h,0273fh		;1a33	21 3f 27 	. ? ' 
	call s1ebch		;1a36	cd bc 1e 	. . . 
	call s1f95h		;1a39	cd 95 1f 	. . . 
	call s1f7ch		;1a3c	cd 7c 1f 	. | . 
	xchg			;1a3f	eb 	. 
	call s0e04h		;1a40	cd 04 0e 	. . . 
l1a43h:
	lhld 02756h		;1a43	2a 56 27 	* V ' 
	xchg			;1a46	eb 	. 
	push h			;1a47	e5 	. 
	lxi b,00081h		;1a48	01 81 00 	. . . 
	dad b			;1a4b	09 	. 
	call CMPDEHL		;1a4c	cd 9f 1f 	. . . 
	dcx h			;1a4f	2b 	+ 
	xthl			;1a50	e3 	. 
	jc l1a5eh		;1a51	da 5e 1a 	. ^ . 
	lxi d,l26f6h+2		;1a54	11 f8 26 	. . & 
	call s211ah		;1a57	cd 1a 21 	. . ! 
	pop d			;1a5a	d1 	. 
	jmp l1a43h		;1a5b	c3 43 1a 	. C . 
l1a5eh:
	call s1f7bh		;1a5e	cd 7b 1f 	. { . 
	pop b			;1a61	c1 	. 
	xchg			;1a62	eb 	. 
	call s0828h		;1a63	cd 28 08 	. ( . 
	jz l1a74h		;1a66	ca 74 1a 	. t . 
	push h			;1a69	e5 	. 
	lhld 02756h		;1a6a	2a 56 27 	* V ' 
	call l1a87h		;1a6d	cd 87 1a 	. . . 
	pop h			;1a70	e1 	. 
	jnc l1a33h		;1a71	d2 33 1a 	. 3 . 
l1a74h:
	mov a,e			;1a74	7b 	{ 
	call s1bebh		;1a75	cd eb 1b 	. . . 
	lxi d,l26f6h+2		;1a78	11 f8 26 	. . & 
	call s2132h		;1a7b	cd 32 21 	. 2 ! 
	call s200fh		;1a7e	cd 0f 20 	. .   
	lhld 02756h		;1a81	2a 56 27 	* V ' 
	jmp l0ee5h		;1a84	c3 e5 0e 	. . . 
l1a87h:
	call s23d6h		;1a87	cd d6 23 	. . # 
	call l0ee5h		;1a8a	cd e5 0e 	. . . 
l1a8dh:
	call s0828h		;1a8d	cd 28 08 	. ( . 
	rz			;1a90	c8 	. 
s1a91h:
	call s23d6h		;1a91	cd d6 23 	. . # 
	lda 0275ch		;1a94	3a 5c 27 	: \ ' 
	ora a			;1a97	b7 	. 
	jm s1cc1h		;1a98	fa c1 1c 	. . . 
	jmp s1c4dh		;1a9b	c3 4d 1c 	. M . 
s1a9eh:
	lda 0275ch		;1a9e	3a 5c 27 	: \ ' 
	ora a			;1aa1	b7 	. 
	stc			;1aa2	37 	7 
	cm s1b5eh		;1aa3	fc 5e 1b 	. ^ . 
	rnc 			;1aa6	d0 	. 
	call s1addh		;1aa7	cd dd 1a 	. . . 
	rnc 			;1aaa	d0 	. 
	jmp s1b5eh		;1aab	c3 5e 1b 	. ^ . 
s1aaeh:
	call s1abeh		;1aae	cd be 1a 	. . . 
	rc			;1ab1	d8 	. 
	call s1a91h		;1ab2	cd 91 1a 	. . . 
	rc			;1ab5	d8 	. 
	call s1abeh		;1ab6	cd be 1a 	. . . 
	cnc s1a91h		;1ab9	d4 91 1a 	. . . 
	ora a			;1abc	b7 	. 
	ret			;1abd	c9 	. 
s1abeh:
	push b			;1abe	c5 	. 
	push d			;1abf	d5 	. 
	push h			;1ac0	e5 	. 
	lda 0275ch		;1ac1	3a 5c 27 	: \ ' 
	ora a			;1ac4	b7 	. 
	lxi d,0273bh		;1ac5	11 3b 27 	. ; ' 
	jm l1aceh		;1ac8	fa ce 1a 	. . . 
	lxi d,0273fh		;1acb	11 3f 27 	. ? ' 
l1aceh:
	lhld 0272fh		;1ace	2a 2f 27 	* / ' 
	xchg			;1ad1	eb 	. 
	call s1efbh		;1ad2	cd fb 1e 	. . . 
	mov h,b			;1ad5	60 	` 
	mov l,c			;1ad6	69 	i 
	call CMPDEHL		;1ad7	cd 9f 1f 	. . . 
	jmp POPHDB		;1ada	c3 cc 1f 	. . . 
s1addh:
	push b			;1add	c5 	. 
	push d			;1ade	d5 	. 
	push h			;1adf	e5 	. 
	call s1b0ch		;1ae0	cd 0c 1b 	. . . 
	jc POPHDB		;1ae3	da cc 1f 	. . . 
	lxi h,0275fh		;1ae6	21 5f 27 	. _ ' 
	inr m			;1ae9	34 	4 
	mov b,m			;1aea	46 	F 
	lxi h,0272eh		;1aeb	21 2e 27 	. . ' 
	xra a			;1aee	af 	. 
l1aefh:
	add m			;1aef	86 	. 
	dcr b			;1af0	05 	. 
	jnz l1aefh		;1af1	c2 ef 1a 	. . . 
	dcr a			;1af4	3d 	= 
	ori 007h		;1af5	f6 07 	. . 
	mov c,a			;1af7	4f 	O 
	lda 0272dh		;1af8	3a 2d 27 	: - ' 
	dcr a			;1afb	3d 	= 
	mov b,a			;1afc	47 	G 
l1afdh:
	call s1b0ch		;1afd	cd 0c 1b 	. . . 
	dcr b			;1b00	05 	. 
	jz l1b08h		;1b01	ca 08 1b 	. . . 
	dcr c			;1b04	0d 	. 
	jnz l1afdh		;1b05	c2 fd 1a 	. . . 
l1b08h:
	ora a			;1b08	b7 	. 
	jmp POPHDB		;1b09	c3 cc 1f 	. . . 
s1b0ch:
	lhld 0273bh		;1b0c	2a 3b 27 	* ; ' 
	call s1d73h		;1b0f	cd 73 1d 	. s . 
	rc			;1b12	d8 	. 
	push h			;1b13	e5 	. 
	lhld l26cdh		;1b14	2a cd 26 	* . & 
	xchg			;1b17	eb 	. 
	lhld 0271eh+2		;1b18	2a 20 27 	*   ' 
	call CMPDEHL		;1b1b	cd 9f 1f 	. . . 
	pop h			;1b1e	e1 	. 
	lxi d,TMPFCB		;1b1f	11 ab 26 	. . & 
	jnc l1b2bh		;1b22	d2 2b 1b 	. + . 
	call s2135h		;1b25	cd 35 21 	. 5 ! 
	jmp l1b38h		;1b28	c3 38 1b 	. 8 . 
l1b2bh:
	call s212fh		;1b2b	cd 2f 21 	. / ! 
	xchg			;1b2e	eb 	. 
	lhld 02720h		;1b2f	2a 20 27 	*   ' 
	inx h			;1b32	23 	# 
	shld 02720h		;1b33	22 20 27 	"   ' 
	xchg			;1b36	eb 	. 
	inr b			;1b37	04 	. 
l1b38h:
	lxi d,DBUFF		;1b38	11 80 00 	. . . 
	dad d			;1b3b	19 	. 
	shld 0273bh		;1b3c	22 3b 27 	" ; ' 
	ret			;1b3f	c9 	. 
s1b40h:
	push b			;1b40	c5 	. 
	push d			;1b41	d5 	. 
	push h			;1b42	e5 	. 
	lxi h,00081h		;1b43	21 81 00 	. . . 
	shld 02745h		;1b46	22 45 27 	" E ' 
	lxi h,02743h		;1b49	21 43 27 	. C ' 
	call s1e49h		;1b4c	cd 49 1e 	. I . 
	lhld 02741h		;1b4f	2a 41 27 	* A ' 
	call l0ee5h		;1b52	cd e5 0e 	. . . 
l1b55h:
	call s1b0ch		;1b55	cd 0c 1b 	. . . 
	jnc l1b55h		;1b58	d2 55 1b 	. U . 
	jmp POPHDB		;1b5b	c3 cc 1f 	. . . 
s1b5eh:
	push b			;1b5e	c5 	. 
	push d			;1b5f	d5 	. 
	push h			;1b60	e5 	. 
	call s1b7eh		;1b61	cd 7e 1b 	. ~ . 
	jc POPHDB		;1b64	da cc 1f 	. . . 
	lda 0272dh		;1b67	3a 2d 27 	: - ' 
	mov b,a			;1b6a	47 	G 
	dcr b			;1b6b	05 	. 
	add a			;1b6c	87 	. 
	mov c,a			;1b6d	4f 	O 
	dcr c			;1b6e	0d 	. 
l1b6fh:
	call s1b7eh		;1b6f	cd 7e 1b 	. ~ . 
	dcr b			;1b72	05 	. 
	jz l1b7ah		;1b73	ca 7a 1b 	. z . 
	dcr c			;1b76	0d 	. 
	jnz l1b6fh		;1b77	c2 6f 1b 	. o . 
l1b7ah:
	ora a			;1b7a	b7 	. 
	jmp POPHDB		;1b7b	c3 cc 1f 	. . . 
s1b7eh:
	lhld 02741h		;1b7e	2a 41 27 	* A ' 
	call s1caah		;1b81	cd aa 1c 	. . . 
	jnz l1b96h		;1b84	c2 96 1b 	. . . 
	lda l26f5h		;1b87	3a f5 26 	: . & 
	mov d,a			;1b8a	57 	W 
	call s0d3ch		;1b8b	cd 3c 0d 	. < . 
	push h			;1b8e	e5 	. 
	mov a,d			;1b8f	7a 	z 
	call s1bebh		;1b90	cd eb 1b 	. . . 
	jmp l1b9bh		;1b93	c3 9b 1b 	. . . 
l1b96h:
	lxi d,0ff80h		;1b96	11 80 ff 	. . . 
	dad d			;1b99	19 	. 
	push h			;1b9a	e5 	. 
l1b9bh:
	xthl			;1b9b	e3 	. 
	call s1d83h		;1b9c	cd 83 1d 	. . . 
	xthl			;1b9f	e3 	. 
	jnc l1ba5h		;1ba0	d2 a5 1b 	. . . 
	pop h			;1ba3	e1 	. 
	ret			;1ba4	c9 	. 
l1ba5h:
	push h			;1ba5	e5 	. 
	lhld 02722h		;1ba6	2a 22 27 	* " ' 
	xchg			;1ba9	eb 	. 
	call s1de3h		;1baa	cd e3 1d 	. . . 
	call CMPDEHL		;1bad	cd 9f 1f 	. . . 
	pop h			;1bb0	e1 	. 
	jnc l1bbdh		;1bb1	d2 bd 1b 	. . . 
	lxi d,0005ch		;1bb4	11 5c 00 	. \ . 
	call s213ch		;1bb7	cd 3c 21 	. < ! 
	jmp l1be2h		;1bba	c3 e2 1b 	. . . 
l1bbdh:
	push h			;1bbd	e5 	. 
	lhld 02724h		;1bbe	2a 24 27 	* $ ' 
	xchg			;1bc1	eb 	. 
	call s1de3h		;1bc2	cd e3 1d 	. . . 
	call CMPDEHL		;1bc5	cd 9f 1f 	. . . 
	pop h			;1bc8	e1 	. 
	lxi d,BAKFCB+FCBFN		;1bc9	11 d1 26 	. . & 
	jnc l1bd5h		;1bcc	d2 d5 1b 	. . . 
	call s2135h		;1bcf	cd 35 21 	. 5 ! 
	jmp l1be2h		;1bd2	c3 e2 1b 	. . . 
l1bd5h:
	call s212fh		;1bd5	cd 2f 21 	. / ! 
	push h			;1bd8	e5 	. 
	lhld 02724h		;1bd9	2a 24 27 	* $ ' 
	dcx h			;1bdc	2b 	+ 
	shld 02724h		;1bdd	22 24 27 	" $ ' 
	pop h			;1be0	e1 	. 
	inr b			;1be1	04 	. 
l1be2h:
	call s1caah		;1be2	cd aa 1c 	. . . 
	ora a			;1be5	b7 	. 
	pop h			;1be6	e1 	. 
	shld 02741h		;1be7	22 41 27 	" A ' 
	ret			;1bea	c9 	. 
s1bebh:
	push b			;1beb	c5 	. 
	push d			;1bec	d5 	. 
	lxi d,027e9h		;1bed	11 e9 27 	. . ' 
	push d			;1bf0	d5 	. 
	mov b,a			;1bf1	47 	G 
	mvi c,082h		;1bf2	0e 82 	. . 
	inr b			;1bf4	04 	. 
	dcx h			;1bf5	2b 	+ 
	dcx d			;1bf6	1b 	. 
l1bf7h:
	mov a,m			;1bf7	7e 	~ 
	stax d			;1bf8	12 	. 
	inx h			;1bf9	23 	# 
	inx d			;1bfa	13 	. 
	dcr c			;1bfb	0d 	. 
	dcr b			;1bfc	05 	. 
	jnz l1bf7h		;1bfd	c2 f7 1b 	. . . 
l1c00h:
	mvi a,EOF		;1c00	3e 1a 	> . 
	stax d			;1c02	12 	. 
	inx d			;1c03	13 	. 
	dcr c			;1c04	0d 	. 
	jnz l1c00h		;1c05	c2 00 1c 	. . . 
	jmp POPHDB		;1c08	c3 cc 1f 	. . . 
s1c0bh:
	call s1caah		;1c0b	cd aa 1c 	. . . 
	rz			;1c0e	c8 	. 
	push b			;1c0f	c5 	. 
	push d			;1c10	d5 	. 
	push h			;1c11	e5 	. 
	call s1e95h		;1c12	cd 95 1e 	. . . 
	lxi h,DBUFF		;1c15	21 80 00 	. . . 
	call CMPDEHL		;1c18	cd 9f 1f 	. . . 
	xchg			;1c1b	eb 	. 
	lhld 02728h		;1c1c	2a 28 27 	* ( ' 
	jnc l1c29h		;1c1f	d2 29 1c 	. ) . 
	call s1addh		;1c22	cd dd 1a 	. . . 
	jc l1d1ah		;1c25	da 1a 1d 	. . . 
	xchg			;1c28	eb 	. 
l1c29h:
	xchg			;1c29	eb 	. 
	lxi h,02743h		;1c2a	21 43 27 	. C ' 
	call s1d0dh		;1c2d	cd 0d 1d 	. . . 
	jmp l1c5ch		;1c30	c3 5c 1c 	. \ . 
s1c33h:
	push b			;1c33	c5 	. 
	push d			;1c34	d5 	. 
	push h			;1c35	e5 	. 
	lxi h,02743h		;1c36	21 43 27 	. C ' 
	lxi d,07800h		;1c39	11 00 78 	. . x 
	call s1d3dh		;1c3c	cd 3d 1d 	. = . 
	call s1caah		;1c3f	cd aa 1c 	. . . 
	jz l1c73h		;1c42	ca 73 1c 	. s . 
	mov a,c			;1c45	79 	y 
	ora a			;1c46	b7 	. 
	jnz l1c5ch		;1c47	c2 5c 1c 	. \ . 
	jmp POPHDB		;1c4a	c3 cc 1f 	. . . 
s1c4dh:
	push b			;1c4d	c5 	. 
	push d			;1c4e	d5 	. 
	push h			;1c4f	e5 	. 
	call s1caah		;1c50	cd aa 1c 	. . . 
	jz l1c73h		;1c53	ca 73 1c 	. s . 
	lxi h,02743h		;1c56	21 43 27 	. C ' 
	call s1cf3h		;1c59	cd f3 1c 	. . . 
l1c5ch:
	lhld 02741h		;1c5c	2a 41 27 	* A ' 
l1c5fh:
	call s1c76h		;1c5f	cd 76 1c 	. v . 
	cnc s1c8fh		;1c62	d4 8f 1c 	. . . 
	jc l1c6bh		;1c65	da 6b 1c 	. k . 
	sta l26f5h		;1c68	32 f5 26 	2 . & 
l1c6bh:
	shld 02741h		;1c6b	22 41 27 	" A ' 
	dcr c			;1c6e	0d 	. 
	jnz l1c5fh		;1c6f	c2 5f 1c 	. _ . 
	ora a			;1c72	b7 	. 
l1c73h:
	jmp POPHDB		;1c73	c3 cc 1f 	. . . 
s1c76h:
	call s23d6h		;1c76	cd d6 23 	. . # 
	lxi d,BAKFCB		;1c79	11 d0 26 	. . & 
	ldax d			;1c7c	1a 	. 
	inx d			;1c7d	13 	. 
	ora a			;1c7e	b7 	. 
	stc			;1c7f	37 	7 
	cnz s20b0h		;1c80	c4 b0 20 	. .   
	rnc 			;1c83	d0 	. 
	lxi d,0005bh		;1c84	11 5b 00 	. [ . 
	ldax d			;1c87	1a 	. 
	inx d			;1c88	13 	. 
	ora a			;1c89	b7 	. 
	stc			;1c8a	37 	7 
	cnz s20a9h		;1c8b	c4 a9 20 	. .   
	ret			;1c8e	c9 	. 
s1c8fh:
	mvi b,080h		;1c8f	06 80 	. . 
l1c91h:
	mov a,m			;1c91	7e 	~ 
	ani 07fh		;1c92	e6 7f 	.  
	mov m,a			;1c94	77 	w 
	cpi EOF		;1c95	fe 1a 	. . 
	jz l1ca3h		;1c97	ca a3 1c 	. . . 
	inx h			;1c9a	23 	# 
	dcr b			;1c9b	05 	. 
	jnz l1c91h		;1c9c	c2 91 1c 	. . . 
	stc			;1c9f	37 	7 
	mvi a,080h		;1ca0	3e 80 	> . 
	ret			;1ca2	c9 	. 
l1ca3h:
	call s20c5h		;1ca3	cd c5 20 	. .   
	mvi a,080h		;1ca6	3e 80 	> . 
	sub b			;1ca8	90 	. 
	ret			;1ca9	c9 	. 
s1caah:
	push h			;1caa	e5 	. 
	lda 0005bh		;1cab	3a 5b 00 	: [ . 
	lxi h,BAKFCB		;1cae	21 d0 26 	. . & 
	ora m			;1cb1	b6 	. 
	stc			;1cb2	37 	7 
	pop h			;1cb3	e1 	. 
	ret			;1cb4	c9 	. 
s1cb5h:
	lda 0275ch		;1cb5	3a 5c 27 	: \ ' 
	ora a			;1cb8	b7 	. 
	lda l26aah		;1cb9	3a aa 26 	: . & 
	cp s1caah		;1cbc	f4 aa 1c 	. . . 
	ora a			;1cbf	b7 	. 
	ret			;1cc0	c9 	. 
s1cc1h:
	push b			;1cc1	c5 	. 
	push d			;1cc2	d5 	. 
	push h			;1cc3	e5 	. 
	lda l26aah		;1cc4	3a aa 26 	: . & 
	ora a			;1cc7	b7 	. 
	stc			;1cc8	37 	7 
	jz l1cf0h		;1cc9	ca f0 1c 	. . . 
	lxi d,s1b5eh		;1ccc	11 5e 1b 	. ^ . 
	lxi h,0273bh		;1ccf	21 3b 27 	. ; ' 
	call s1cf6h		;1cd2	cd f6 1c 	. . . 
	lhld 0273bh		;1cd5	2a 3b 27 	* ; ' 
l1cd8h:
	call s23d6h		;1cd8	cd d6 23 	. . # 
	lxi d,0ff80h		;1cdb	11 80 ff 	. . . 
	dad d			;1cde	19 	. 
	lxi d,TMPFCB		;1cdf	11 ab 26 	. . & 
	call s20b0h		;1ce2	cd b0 20 	. .   
	jc l1cefh		;1ce5	da ef 1c 	. . . 
	shld 0273bh		;1ce8	22 3b 27 	" ; ' 
	dcr c			;1ceb	0d 	. 
	jnz l1cd8h		;1cec	c2 d8 1c 	. . . 
l1cefh:
	ora a			;1cef	b7 	. 
l1cf0h:
	jmp POPHDB		;1cf0	c3 cc 1f 	. . . 
s1cf3h:
	lxi d,s1addh		;1cf3	11 dd 1a 	. . . 
s1cf6h:
	push h			;1cf6	e5 	. 
	lhld 02728h		;1cf7	2a 28 27 	* ( ' 
	push d			;1cfa	d5 	. 
	call s1e95h		;1cfb	cd 95 1e 	. . . 
	call CMPDEHL		;1cfe	cd 9f 1f 	. . . 
	pop h			;1d01	e1 	. 
	cc s11c4h		;1d02	dc c4 11 	. . . 
	lhld 02726h		;1d05	2a 26 27 	* & ' 
	dad h			;1d08	29 	) 
	inr h			;1d09	24 	$ 
	inr h			;1d0a	24 	$ 
	xchg			;1d0b	eb 	. 
	pop h			;1d0c	e1 	. 
s1d0dh:
	call s1d3dh		;1d0d	cd 3d 1d 	. = . 
	cpi 008h		;1d10	fe 08 	. . 
	jc l1d18h		;1d12	da 18 1d 	. . . 
	sui 004h		;1d15	d6 04 	. . 
	mov c,a			;1d17	4f 	O 
l1d18h:
	ora a			;1d18	b7 	. 
	rnz			;1d19	c0 	. 
l1d1ah:
	call s0534h		;1d1a	cd 34 05 	. 4 . 
	db 'MEM SHORTAGE, TRY CLEARING QBUF',0
s1d3dh:
	call s1e49h		;1d3d	cd 49 1e 	. I . 
	push d			;1d40	d5 	. 
	call s1e95h		;1d41	cd 95 1e 	. . . 
	pop h			;1d44	e1 	. 
	call s1fbdh		;1d45	cd bd 1f 	. . . 
	xchg			;1d48	eb 	. 
	dad h			;1d49	29 	) 
l1d4ah:
	jc l1d4ah		;1d4a	da 4a 1d 	. J . 
	xchg			;1d4d	eb 	. 
	mov a,d			;1d4e	7a 	z 
	mov c,d			;1d4f	4a 	J 
	ret			;1d50	c9 	. 
s1d51h:
	call s1c33h		;1d51	cd 33 1c 	. 3 . 
l1d54h:
	call s1b40h		;1d54	cd 40 1b 	. @ . 
	call s1c33h		;1d57	cd 33 1c 	. 3 . 
	jnc l1d54h		;1d5a	d2 54 1d 	. T . 
	lxi h,0273bh		;1d5d	21 3b 27 	. ; ' 
	call s1efbh		;1d60	cd fb 1e 	. . . 
	call s1f95h		;1d63	cd 95 1f 	. . . 
	mov a,c			;1d66	79 	y 
	call s1bebh		;1d67	cd eb 1b 	. . . 
	lxi d,TMPFCB		;1d6a	11 ab 26 	. . & 
	call s212fh		;1d6d	cd 2f 21 	. / ! 
	jmp s200fh		;1d70	c3 0f 20 	. .   
s1d73h:
	push h			;1d73	e5 	. 
	lhld 02745h		;1d74	2a 45 27 	* E ' 
	xchg			;1d77	eb 	. 
	lhld 0273dh		;1d78	2a 3d 27 	* = ' 
	call s1f7ch		;1d7b	cd 7c 1f 	. | . 
	xchg			;1d7e	eb 	. 
	pop h			;1d7f	e1 	. 
l1d80h:
	jmp CMPDEHL		;1d80	c3 9f 1f 	. . . 
s1d83h:
	push h			;1d83	e5 	. 
	lhld 02747h		;1d84	2a 47 27 	* G ' 
	xchg			;1d87	eb 	. 
	lhld 0273fh		;1d88	2a 3f 27 	* ? ' 
	dad d			;1d8b	19 	. 
	pop d			;1d8c	d1 	. 
	call CMPDEHL		;1d8d	cd 9f 1f 	. . . 
	xchg			;1d90	eb 	. 
	ret			;1d91	c9 	. 
s1d92h:
	push d			;1d92	d5 	. 
	lxi d,0273dh		;1d93	11 3d 27 	. = ' 
	call CMPDEHL		;1d96	cd 9f 1f 	. . . 
	pop d			;1d99	d1 	. 
	rnz			;1d9a	c0 	. 
s1d9bh:
	push b			;1d9b	c5 	. 
	push d			;1d9c	d5 	. 
	push h			;1d9d	e5 	. 
	push psw			;1d9e	f5 	. 
	lxi h,0273bh		;1d9f	21 3b 27 	. ; ' 
	call s1df4h		;1da2	cd f4 1d 	. . . 
	lhld l26cdh		;1da5	2a cd 26 	* . & 
	dad d			;1da8	19 	. 
	xchg			;1da9	eb 	. 
	lhld 02720h		;1daa	2a 20 27 	*   ' 
	call CMPDEHL		;1dad	cd 9f 1f 	. . . 
	xchg			;1db0	eb 	. 
	jnc l1db7h		;1db1	d2 b7 1d 	. . . 
	shld 02720h		;1db4	22 20 27 	"   ' 
l1db7h:
	lxi h,0273fh		;1db7	21 3f 27 	. ? ' 
	call s1df4h		;1dba	cd f4 1d 	. . . 
	call s1de3h		;1dbd	cd e3 1d 	. . . 
	call s1f7ch		;1dc0	cd 7c 1f 	. | . 
	xchg			;1dc3	eb 	. 
	lhld 02722h		;1dc4	2a 22 27 	* " ' 
	call CMPDEHL		;1dc7	cd 9f 1f 	. . . 
	xchg			;1dca	eb 	. 
	jc l1dd1h		;1dcb	da d1 1d 	. . . 
	shld 02722h		;1dce	22 22 27 	" " ' 
l1dd1h:
	xchg			;1dd1	eb 	. 
	lhld 02724h		;1dd2	2a 24 27 	* $ ' 
	call CMPDEHL		;1dd5	cd 9f 1f 	. . . 
	xchg			;1dd8	eb 	. 
	jc l1ddfh		;1dd9	da df 1d 	. . . 
	shld 02724h		;1ddc	22 24 27 	" $ ' 
l1ddfh:
	pop psw			;1ddf	f1 	. 
	jmp POPHDB		;1de0	c3 cc 1f 	. . . 
s1de3h:
	push d			;1de3	d5 	. 
	lhld l26f3h		;1de4	2a f3 26 	* . & 
	xchg			;1de7	eb 	. 
	lhld 0007eh		;1de8	2a 7e 00 	* ~ . 
	call s1f7ch		;1deb	cd 7c 1f 	. | . 
	lxi d,07fffh		;1dee	11 ff 7f 	. .  
	dad d			;1df1	19 	. 
	pop d			;1df2	d1 	. 
	ret			;1df3	c9 	. 
s1df4h:
	call s1efbh		;1df4	cd fb 1e 	. . . 
	mov h,b			;1df7	60 	` 
	mov l,c			;1df8	69 	i 
	dad h			;1df9	29 	) 
	mov e,h			;1dfa	5c 	\ 
	mvi d,000h		;1dfb	16 00 	. . 
	jnc l1e01h		;1dfd	d2 01 1e 	. . . 
	inr d			;1e00	14 	. 
l1e01h:
	ret			;1e01	c9 	. 
	push b			;1e02	c5 	. 
	push d			;1e03	d5 	. 
	push h			;1e04	e5 	. 
l1e05h:
	xchg			;1e05	eb 	. 
	push h			;1e06	e5 	. 
	call s1f95h		;1e07	cd 95 1f 	. . . 
	xchg			;1e0a	eb 	. 
	call s1f85h		;1e0b	cd 85 1f 	. . . 
	pop h			;1e0e	e1 	. 
	push h			;1e0f	e5 	. 
	call s1f8fh		;1e10	cd 8f 1f 	. . . 
	call MEMCPY		;1e13	cd 0c 1f 	. . . 
	xthl			;1e16	e3 	. 
	dcx h			;1e17	2b 	+ 
	dcx h			;1e18	2b 	+ 
	mov m,e			;1e19	73 	s 
	inx h			;1e1a	23 	# 
	mov m,d			;1e1b	72 	r 
	inx h			;1e1c	23 	# 
	pop d			;1e1d	d1 	. 
	mov m,e			;1e1e	73 	s 
	inx h			;1e1f	23 	# 
	mov m,d			;1e20	72 	r 
	ora a			;1e21	b7 	. 
	jmp POPHDB		;1e22	c3 cc 1f 	. . . 
	push b			;1e25	c5 	. 
	push d			;1e26	d5 	. 
	push h			;1e27	e5 	. 
l1e28h:
	xchg			;1e28	eb 	. 
	inx h			;1e29	23 	# 
	inx h			;1e2a	23 	# 
	push h			;1e2b	e5 	. 
	call s1f95h		;1e2c	cd 95 1f 	. . . 
	call s1f85h		;1e2f	cd 85 1f 	. . . 
	xthl			;1e32	e3 	. 
	inx h			;1e33	23 	# 
	inx h			;1e34	23 	# 
	mov e,m			;1e35	5e 	^ 
	inx h			;1e36	23 	# 
	mov d,m			;1e37	56 	V 
	xthl			;1e38	e3 	. 
	call s1f2eh		;1e39	cd 2e 1f 	. . . 
	xthl			;1e3c	e3 	. 
	mov m,d			;1e3d	72 	r 
	dcx h			;1e3e	2b 	+ 
	mov m,e			;1e3f	73 	s 
	pop d			;1e40	d1 	. 
	dcx h			;1e41	2b 	+ 
	mov m,d			;1e42	72 	r 
	dcx h			;1e43	2b 	+ 
	mov m,e			;1e44	73 	s 
	ora a			;1e45	b7 	. 
	jmp POPHDB		;1e46	c3 cc 1f 	. . . 
s1e49h:
	push b			;1e49	c5 	. 
	push d			;1e4a	d5 	. 
	push h			;1e4b	e5 	. 
l1e4ch:
	xchg			;1e4c	eb 	. 
	lxi h,02733h		;1e4d	21 33 27 	. 3 ' 
	call CMPDEHL		;1e50	cd 9f 1f 	. . . 
	cc s1e86h		;1e53	dc 86 1e 	. . . 
	lxi h,02744h		;1e56	21 44 27 	. D ' 
	call CMPDEHL		;1e59	cd 9f 1f 	. . . 
	cnc s1e86h		;1e5c	d4 86 1e 	. . . 
	lxi h,02733h		;1e5f	21 33 27 	. 3 ' 
l1e62h:
	inx h			;1e62	23 	# 
	call CMPDEHL		;1e63	cd 9f 1f 	. . . 
	dcx h			;1e66	2b 	+ 
	jc l1e73h		;1e67	da 73 1e 	. s . 
	call s1ebch		;1e6a	cd bc 1e 	. . . 
	call s1each		;1e6d	cd ac 1e 	. . . 
	jmp l1e62h		;1e70	c3 62 1e 	. b . 
l1e73h:
	lxi h,02741h		;1e73	21 41 27 	. A ' 
l1e76h:
	call CMPDEHL		;1e76	cd 9f 1f 	. . . 
	jnc POPHDB		;1e79	d2 cc 1f 	. . . 
	call s1ed8h		;1e7c	cd d8 1e 	. . . 
	dcx h			;1e7f	2b 	+ 
	dcx h			;1e80	2b 	+ 
	dcx h			;1e81	2b 	+ 
	dcx h			;1e82	2b 	+ 
	jmp l1e76h		;1e83	c3 76 1e 	. v . 
s1e86h:
	call s1fe7h		;1e86	cd e7 1f 	. . . 
	db 'PUTHOLE ERR',0
s1e95h:
	push b			;1e95	c5 	. 
	push h			;1e96	e5 	. 
	lxi d,00000h		;1e97	11 00 00 	. . . 
	lxi h,02731h		;1e9a	21 31 27 	. 1 ' 
l1e9dh:
	call s1efbh		;1e9d	cd fb 1e 	. . . 
	xchg			;1ea0	eb 	. 
	dad b			;1ea1	09 	. 
	xchg			;1ea2	eb 	. 
	call s1each		;1ea3	cd ac 1e 	. . . 
	jnc l1e9dh		;1ea6	d2 9d 1e 	. . . 
	pop h			;1ea9	e1 	. 
	pop b			;1eaa	c1 	. 
	ret			;1eab	c9 	. 
s1each:
	push d			;1eac	d5 	. 
	inx h			;1ead	23 	# 
	inx h			;1eae	23 	# 
	inx h			;1eaf	23 	# 
	inx h			;1eb0	23 	# 
	lxi d,02742h		;1eb1	11 42 27 	. B ' 
	call CMPDEHL		;1eb4	cd 9f 1f 	. . . 
	pop d			;1eb7	d1 	. 
	ret			;1eb8	c9 	. 
s1eb9h:
	lxi h,02737h		;1eb9	21 37 27 	. 7 ' 
s1ebch:
	push b			;1ebc	c5 	. 
	push d			;1ebd	d5 	. 
	push h			;1ebe	e5 	. 
	call s1efbh		;1ebf	cd fb 1e 	. . . 
	call s1f8fh		;1ec2	cd 8f 1f 	. . . 
	xthl			;1ec5	e3 	. 
	mov m,e			;1ec6	73 	s 
	inx h			;1ec7	23 	# 
	mov m,d			;1ec8	72 	r 
	dcx h			;1ec9	2b 	+ 
	xthl			;1eca	e3 	. 
	call MEMCPY		;1ecb	cd 0c 1f 	. . . 
	pop h			;1ece	e1 	. 
	push h			;1ecf	e5 	. 
	inx h			;1ed0	23 	# 
	inx h			;1ed1	23 	# 
	mov m,e			;1ed2	73 	s 
	inx h			;1ed3	23 	# 
	mov m,d			;1ed4	72 	r 
	jmp POPHDB		;1ed5	c3 cc 1f 	. . . 
s1ed8h:
	push b			;1ed8	c5 	. 
	push d			;1ed9	d5 	. 
	push h			;1eda	e5 	. 
	push h			;1edb	e5 	. 
	dcx h			;1edc	2b 	+ 
	dcx h			;1edd	2b 	+ 
	call s1efbh		;1ede	cd fb 1e 	. . . 
	pop h			;1ee1	e1 	. 
	push h			;1ee2	e5 	. 
	call s1f91h		;1ee3	cd 91 1f 	. . . 
	xchg			;1ee6	eb 	. 
	xthl			;1ee7	e3 	. 
	inx h			;1ee8	23 	# 
	mov m,d			;1ee9	72 	r 
	dcx h			;1eea	2b 	+ 
	mov m,e			;1eeb	73 	s 
	xthl			;1eec	e3 	. 
	call s1f2eh		;1eed	cd 2e 1f 	. . . 
	pop h			;1ef0	e1 	. 
	dcx h			;1ef1	2b 	+ 
	mov m,d			;1ef2	72 	r 
	dcx h			;1ef3	2b 	+ 
	mov m,e			;1ef4	73 	s 
	jmp POPHDB		;1ef5	c3 cc 1f 	. . . 
s1ef8h:
	lxi h,0273fh		;1ef8	21 3f 27 	. ? ' 
s1efbh:
	push d			;1efb	d5 	. 
	push h			;1efc	e5 	. 
	call s1f91h		;1efd	cd 91 1f 	. . . 
	call s1f85h		;1f00	cd 85 1f 	. . . 
	pop h			;1f03	e1 	. 
	pop d			;1f04	d1 	. 
	ret			;1f05	c9 	. 
s1f06h:
	lxi h,l1f53h		;1f06	21 53 1f 	. S . 
s1f09h:
	lxi b,00003h		;1f09	01 03 00 	. . . 

;
;	INPUT:
;	HL=SOURCE ADDRESS
;	DE=DESTINATION ADDRESS
;	BC=COUNT
;	OUTPUT:
;	HL=HL+COUNT
;	DE=DE+COUNT
;	BC=0
MEMCPY:
	call s1f56h		;1f0c	cd 56 1f 	. V . 
	call CMPDEHL		;1f0f	cd 9f 1f 	. . . 
	jnz MEMCPY$0		;1f12	c2 1c 1f 	. . . 
	dad b			;HL=HL+BC
	xchg			;DE=HL,HL=DE
	dad b			;HL=HL+BC
	lxi b,00000h		;CLEAR BC
	ret			;1f1b	c9 	. 
MEMCPY$0:
	inr b			;1f1c	04 	. 
	inr c			;1f1d	0c 	. 
	jmp MEMCPY$2		;1f1e	c3 25 1f 	. % . 
MEMCPY$1:
	mov a,m			;1f21	7e 	~ 
	stax d			;1f22	12 	. 
	inx h			;1f23	23 	# 
	inx d			;1f24	13 	. 
MEMCPY$2:
	dcr c			;1f25	0d 	. 
	jnz MEMCPY$1		;1f26	c2 21 1f 	. . . 
	dcr b			;1f29	05 	. 
	jnz MEMCPY$1		;1f2a	c2 21 1f 	. . . 
	ret			;1f2d	c9 	. 

;MEMCPY REVERSE
s1f2eh:
	call s1f56h		;1f2e	cd 56 1f 	. V . 
	call CMPDEHL		;1f31	cd 9f 1f 	. . . 
	jnz l1f41h		;1f34	c2 41 1f 	. A . 
	call s1f71h		;1f37	cd 71 1f 	. q . 
	dad b			;1f3a	09 	. 
	xchg			;1f3b	eb 	. 
	dad b			;1f3c	09 	. 
	lxi b,00000h		;1f3d	01 00 00 	. . . 
	ret			;1f40	c9 	. 
l1f41h:
	inr b			;1f41	04 	. 
	inr c			;1f42	0c 	. 
	jmp l1f4ah		;1f43	c3 4a 1f 	. J . 
l1f46h:
	dcx h			;1f46	2b 	+ 
	dcx d			;1f47	1b 	. 
	mov a,m			;1f48	7e 	~ 
	stax d			;1f49	12 	. 
l1f4ah:
	dcr c			;1f4a	0d 	. 
	jnz l1f46h		;1f4b	c2 46 1f 	. F . 
	dcr b			;1f4e	05 	. 
	jnz l1f46h		;1f4f	c2 46 1f 	. F . 
	ret			;1f52	c9 	. 
l1f53h:
	db 'BAK'

s1f56h:
	mov a,b			;1f56	78 	x 
	ori 017h		;1f57	f6 17 	. . 
	inr a			;1f59	3c 	< 
	rnz			;1f5a	c0 	. 
	call s0534h		;1f5b	cd 34 05 	. 4 . 
	db 'COPY ERR',0
s1f67h:
	push psw			;1f67	f5 	. 
	xra a			;1f68	af 	. 
	sub l			;1f69	95 	. 
	mov l,a			;1f6a	6f 	o 
	mvi a,000h		;1f6b	3e 00 	> . 
	sbb h			;1f6d	9c 	. 
	mov h,a			;1f6e	67 	g 
	pop psw			;1f6f	f1 	. 
	ret			;1f70	c9 	. 
s1f71h:
	push psw			;1f71	f5 	. 
	xra a			;1f72	af 	. 
	sub c			;1f73	91 	. 
	mov c,a			;1f74	4f 	O 
	mvi a,000h		;1f75	3e 00 	> . 
	sbb b			;1f77	98 	. 
	mov b,a			;1f78	47 	G 
	pop psw			;1f79	f1 	. 
	ret			;1f7a	c9 	. 
s1f7bh:
	xchg			;1f7b	eb 	. 
s1f7ch:
	push d			;1f7c	d5 	. 
	xchg			;1f7d	eb 	. 
	call s1f67h		;1f7e	cd 67 1f 	. g . 
	xchg			;1f81	eb 	. 
	dad d			;1f82	19 	. 
	pop d			;1f83	d1 	. 
	ret			;1f84	c9 	. 
s1f85h:
	push h			;1f85	e5 	. 
	call s1f7ch		;1f86	cd 7c 1f 	. | . 
	mov b,h			;1f89	44 	D 
	mov c,l			;1f8a	4d 	M 
	pop h			;1f8b	e1 	. 
	mov a,b			;1f8c	78 	x 
	ora c			;1f8d	b1 	. 
	ret			;1f8e	c9 	. 
s1f8fh:
	dcx h			;1f8f	2b 	+ 
	dcx h			;1f90	2b 	+ 
s1f91h:
	mov e,m			;1f91	5e 	^ 
	inx h			;1f92	23 	# 
	mov d,m			;1f93	56 	V 
	inx h			;1f94	23 	# 
s1f95h:
	mov a,m			;1f95	7e 	~ 
	inx h			;1f96	23 	# 
	mov h,m			;1f97	66 	f 
	mov l,a			;1f98	6f 	o 
	ret			;1f99	c9 	. 
	dcx h			;1f9a	2b 	+ 
	dcx h			;1f9b	2b 	+ 
	jmp s1f95h		;1f9c	c3 95 1f 	. . . 
;
;	COMPARE DE AND HL (DE-HL)
;	RETURN NO CARRY IF HL < DE
;	RETURN CARRY IF HL > DE
;	RETURN NO ZERO IF HL <> DE
;	RETURN ZERO IF HL = DE
CMPDEHL:
	push b			;SAVE B
	mov b,a			;SAVE A
	mov a,d			;A = D
	sub h			;A = D-H
	jnz l1fa8h		;JMP IF H <> D
	mov a,e			;A = E
	sub l			;A = E-L
l1fa8h:
	mov a,b			;RESTORE A
l1fa9h:
	pop b			;RESTORE B
	ret			;RETURN WITH ZERO/CARRY CHANGED

s1fabh:
	call CMPDEHL		;1fab	cd 9f 1f 	. . . 
	jc l1fb6h		;1fae	da b6 1f 	. . . 
l1fb1h:
	lxi h,00000h		;1fb1	21 00 00 	. . . 
	ora a			;1fb4	b7 	. 
	ret			;1fb5	c9 	. 
l1fb6h:
	push h			;1fb6	e5 	. 
	call s1f7ch		;1fb7	cd 7c 1f 	. | . 
	pop d			;1fba	d1 	. 
	stc			;1fbb	37 	7 
	ret			;1fbc	c9 	. 
;
s1fbdh:
	call CMPDEHL		;1fbd	cd 9f 1f 	. . . 
	jz l1fb1h		;1fc0	ca b1 1f 	. . . 
	jc l1fb1h		;1fc3	da b1 1f 	. . . 
	xchg			;1fc6	eb 	. 
	call s1f7ch		;1fc7	cd 7c 1f 	. | . 
	stc			;1fca	37 	7 
	ret			;1fcb	c9 	. 
POPHDB:
	pop h			;1fcc	e1 	. 
	pop d			;1fcd	d1 	. 
	pop b			;1fce	c1 	. 
	ret			;1fcf	c9 	. 
s1fd0h:
	push d			;1fd0	d5 	. 
	lxi d,DBUFF		;1fd1	11 80 00 	. . . 
	call s1fd9h		;1fd4	cd d9 1f 	. . . 
	ldax d			;1fd7	1a 	. 
	pop d			;1fd8	d1 	. 
s1fd9h:
	xthl			;1fd9	e3 	. 
	push b			;1fda	c5 	. 
	mov c,m			;1fdb	4e 	N 
	inx h			;1fdc	23 	# 
	push d			;1fdd	d5 	. 
	push h			;1fde	e5 	. 
	call BDOS		;1fdf	cd 05 00 	. . . 
	pop h			;1fe2	e1 	. 
	pop d			;1fe3	d1 	. 
	pop b			;1fe4	c1 	. 
	xthl			;1fe5	e3 	. 
	ret			;1fe6	c9 	. 
s1fe7h:
	call s24e3h		;1fe7	cd e3 24 	. . $ 
	pop d			;1fea	d1 	. 
	call PRINT		;1feb	cd 0e 23 	. . # 
	call s24e3h		;1fee	cd e3 24 	. . $ 
	jmp l0463h		;1ff1	c3 63 04 	. c . 
s1ff4h:
	call s1fd0h		;1ff4	cd d0 1f 	. . . 
	rrc			;1ff7	0f 	. 
	inr a			;1ff8	3c 	< 
s1ff9h:
	push h			;1ff9	e5 	. 
	lxi h,00023h		;1ffa	21 23 00 	. # . 
	dad d			;1ffd	19 	. 
	mvi m,000h		;1ffe	36 00 	6 . 
	dcx h			;2000	2b 	+ 
	mvi m,000h		;2001	36 00 	6 . 
	pop h			;2003	e1 	. 
	ret			;2004	c9 	. 
s2005h:
	lxi d,BAKFCB+FCBFN		;2005	11 d1 26 	. . & 
s2008h:
	dcx d			;2008	1b 	. 
	dcx d			;2009	1b 	. 
	ldax d			;200a	1a 	. 
	inx d			;200b	13 	. 
	inx d			;200c	13 	. 
	ora a			;200d	b7 	. 
	rz			;200e	c8 	. 
s200fh:
	call s1fd0h		;200f	cd d0 1f 	. . . 
	dw 03c10h		;2012	10 3c 	. < 
	rnz			;2014	c0 	. 
	call s1fe7h		;2015	cd e7 1f 	. . . 
	db 'CLOSE ERR',0
s2022h:
	lxi d,BAKFCB+FCBFN		;2022	11 d1 26 	. . & 
	call s2008h		;2025	cd 08 20 	. .   
s2028h:
	call s1fd0h		;2028	cd d0 1f 	. . . 
	inx d			;202b	13 	. 
	dcx d			;202c	1b 	. 
	call s20c5h		;202d	cd c5 20 	. .   
	inx d			;2030	13 	. 
	jmp s20c5h		;2031	c3 c5 20 	. .   
s2034h:
	call s2028h		;2034	cd 28 20 	. (   
	call s1ff9h		;2037	cd f9 1f 	. . . 
s203ah:
	call s1fd0h		;203a	cd d0 1f 	. . . 
	mvi d,03ch		;203d	16 3c 	. < 
	rnz			;203f	c0 	. 
l2040h:
	call s1fe7h		;2040	cd e7 1f 	. . . 
	db 'DIRECTORY FULL',0
s2052h:
	push d			;2052	d5 	. 
	mov e,a			;2053	5f 	_ 
	lda 0274bh		;2054	3a 4b 27 	: K ' 
	ora a			;2057	b7 	. 
	mov a,e			;2058	7b 	{ 
	jnz l2062h		;2059	c2 62 20 	. b   
	call s1fd9h		;205c	cd d9 1f 	. . . 
	mvi e,0d1h		;205f	1e d1 	. . 
	ret			;2061	c9 	. 
l2062h:
	pop d			;2062	d1 	. 
	ora a			;2063	b7 	. 
l2064h:
	jnz l206ch		;2064	c2 6c 20 	. l   
	call s1fd9h		;2067	cd d9 1f 	. . . 
	dad d			;206a	19 	. 
	inr a			;206b	3c 	< 
l206ch:
	ret			;206c	c9 	. 
s206dh:
	push b			;206d	c5 	. 
	push d			;206e	d5 	. 
	push h			;206f	e5 	. 
	push d			;2070	d5 	. 
	lxi h,00010h		;2071	21 10 00 	. . . 
	dad d			;2074	19 	. 
	lxi b,00010h		;2075	01 10 00 	. . . 
	xchg			;2078	eb 	. 
	call MEMCPY		;2079	cd 0c 1f 	. . . 
	mvi m,000h		;207c	36 00 	6 . 
	pop d			;207e	d1 	. 
	lxi h,00019h		;207f	21 19 00 	. . . 
	dad d			;2082	19 	. 
	xchg			;2083	eb 	. 
	pop h			;2084	e1 	. 
	push h			;2085	e5 	. 
	call s1f09h		;2086	cd 09 1f 	. . . 
	pop h			;2089	e1 	. 
	pop d			;208a	d1 	. 
	pop b			;208b	c1 	. 
	call s1fd0h		;208c	cd d0 1f 	. . . 
	ral			;208f	17 	. 
	inr a			;2090	3c 	< 
	rnz			;2091	c0 	. 
	lda 0274bh		;2092	3a 4b 27 	: K ' 
	ora a			;2095	b7 	. 
	rnz			;2096	c0 	. 
	call s1fe7h		;2097	cd e7 1f 	. . . 
	db 'RENAME FAILURE',0
s20a9h:
	call s20b4h		;20a9	cd b4 20 	. .   
	cnc s2135h		;20ac	d4 35 21 	. 5 ! 
	ret			;20af	c9 	. 
s20b0h:
	call s213ch		;20b0	cd 3c 21 	. < ! 
	rc			;20b3	d8 	. 
s20b4h:
	call l234eh		;20b4	cd 4e 23 	. N # 
	xra a			;20b7	af 	. 
	call s2167h		;20b8	cd 67 21 	. g ! 
	jc s20c5h		;20bb	da c5 20 	. .   
	call s1fd9h		;20be	cd d9 1f 	. . . 
	inr d			;20c1	14 	. 
	ora a			;20c2	b7 	. 
	dcr a			;20c3	3d 	= 
	rnz			;20c4	c0 	. 
s20c5h:
	dcx d			;20c5	1b 	. 
	xra a			;20c6	af 	. 
	stax d			;20c7	12 	. 
	inx d			;20c8	13 	. 
	stc			;20c9	37 	7 
	ret			;20ca	c9 	. 
s20cbh:
	push h			;20cb	e5 	. 
	mov h,d			;20cc	62 	b 
	mov l,e			;20cd	6b 	k 
	dcx h			;20ce	2b 	+ 
	dcx h			;20cf	2b 	+ 
	mov a,m			;20d0	7e 	~ 
	ora a			;20d1	b7 	. 
	cz s2034h		;20d2	cc 34 20 	. 4   
	mvi m,0ffh		;20d5	36 ff 	6 . 
	pop h			;20d7	e1 	. 
	call s210ah		;20d8	cd 0a 21 	. . ! 
	rnz			;20db	c0 	. 
	push psw			;20dc	f5 	. 
	push h			;20dd	e5 	. 
	lxi h,BAKFCB+FCBFN		;20de	21 d1 26 	. . & 
	call CMPDEHL		;20e1	cd 9f 1f 	. . . 
	pop h			;20e4	e1 	. 
	jz l20f7h		;20e5	ca f7 20 	. .   
l20e8h:
	lda BAKFCB		;20e8	3a d0 26 	: . & 
	ora a			;20eb	b7 	. 
	push d			;20ec	d5 	. 
	cz s2022h		;20ed	cc 22 20 	. "   
	pop d			;20f0	d1 	. 
	pop psw			;20f1	f1 	. 
	call s210ah		;20f2	cd 0a 21 	. . ! 
	rnz			;20f5	c0 	. 
	push psw			;20f6	f5 	. 
l20f7h:
	pop psw			;20f7	f1 	. 
	cpi 002h		;20f8	fe 02 	. . 
	jnz l2040h		;20fa	c2 40 20 	. @   
	call s0534h		;20fd	cd 34 05 	. 4 . 
	db 'DISK FULL',0
s210ah:
	call s2165h		;210a	cd 65 21 	. e ! 
	call s1fd9h		;210d	cd d9 1f 	. . . 
	dcr d			;2110	15 	. 
	cpi 001h		;2111	fe 01 	. . 
	rz			;2113	c8 	. 
	cpi 0ffh		;2114	fe ff 	. . 
	rz			;2116	c8 	. 
	cpi 002h		;2117	fe 02 	. . 
	ret			;2119	c9 	. 
s211ah:
	push b			;211a	c5 	. 
	push h			;211b	e5 	. 
	push d			;211c	d5 	. 
	lxi b,080h		;211d	01 80 00 	. . . 
	lxi d,027e9h		;2120	11 e9 27 	. . ' 
	push d			;2123	d5 	. 
	call MEMCPY		;2124	cd 0c 1f 	. . . 
	pop h			;2127	e1 	. 
	pop d			;2128	d1 	. 
	call s2132h		;2129	cd 32 21 	. 2 ! 
	pop h			;212c	e1 	. 
	pop b			;212d	c1 	. 
	ret			;212e	c9 	. 
s212fh:
	call s23d6h		;212f	cd d6 23 	. . # 
s2132h:
	call s20cbh		;2132	cd cb 20 	. .   
s2135h:
	push b			;2135	c5 	. 
	lxi b,00001h		;2136	01 01 00 	. . . 
	jmp l2140h		;2139	c3 40 21 	. @ ! 
s213ch:
	push b			;213c	c5 	. 
	lxi b,0ffffh		;213d	01 ff ff 	. . . 
l2140h:
	push d			;2140	d5 	. 
	push h			;2141	e5 	. 
	dcx d			;2142	1b 	. 
	mvi a,0ffh		;2143	3e ff 	> . 
	stax d			;2145	12 	. 
	lxi h,00023h		;2146	21 23 00 	. # . 
	dad d			;2149	19 	. 
	push h			;214a	e5 	. 
	call s1f95h		;214b	cd 95 1f 	. . . 
	dad b			;214e	09 	. 
	mov a,h			;214f	7c 	| 
	ana l			;2150	a5 	. 
	inr a			;2151	3c 	< 
	jz l215eh		;2152	ca 5e 21 	. ^ ! 
	xchg			;2155	eb 	. 
	pop h			;2156	e1 	. 
	mov m,e			;2157	73 	s 
	inx h			;2158	23 	# 
	mov m,d			;2159	72 	r 
	ora a			;215a	b7 	. 
	jmp POPHDB		;215b	c3 cc 1f 	. . . 
l215eh:
	xra a			;215e	af 	. 
	stax d			;215f	12 	. 
	stc			;2160	37 	7 
	pop h			;2161	e1 	. 
	jmp POPHDB		;2162	c3 cc 1f 	. . . 
s2165h:
	mvi a,0ffh		;2165	3e ff 	> . 
s2167h:
	push b			;2167	c5 	. 
	mov b,a			;2168	47 	G 
	push h			;2169	e5 	. 
	lxi h,00022h		;216a	21 22 00 	. " . 
	dad d			;216d	19 	. 
	call s1f95h		;216e	cd 95 1f 	. . . 
	mov a,l			;2171	7d 	} 
	ani 07fh		;2172	e6 7f 	.  
	mov c,a			;2174	4f 	O 
	dad h			;2175	29 	) 
	mov a,h			;2176	7c 	| 
	lxi h,0000ch		;2177	21 0c 00 	. . . 
	dad d			;217a	19 	. 
	cmp m			;217b	be 	. 
	jz l21a0h		;217c	ca a0 21 	. . ! 
	push h			;217f	e5 	. 
	push psw			;2180	f5 	. 
	lxi h,00021h		;2181	21 21 00 	. . . 
	dad d			;2184	19 	. 
	mov a,m			;2185	7e 	~ 
	ora a			;2186	b7 	. 
	cnz s200fh		;2187	c4 0f 20 	. .   
	mvi m,000h		;218a	36 00 	6 . 
	pop psw			;218c	f1 	. 
	pop h			;218d	e1 	. 
	mov m,a			;218e	77 	w 
	call s1fd0h		;218f	cd d0 1f 	. . . 
	rrc			;2192	0f 	. 
	inr a			;2193	3c 	< 
	jnz l21a0h		;2194	c2 a0 21 	. . . 
	mov a,b			;2197	78 	x 
	ora a			;2198	b7 	. 
	stc			;2199	37 	7 
	jz l21a9h		;219a	ca a9 21 	. . . 
	call s203ah		;219d	cd 3a 20 	. :   
l21a0h:
	lxi h,00020h		;21a0	21 20 00 	.   . 
	dad d			;21a3	19 	. 
	mov m,c			;21a4	71 	q 
	inx h			;21a5	23 	# 
	mov a,m			;21a6	7e 	~ 
	ora b			;21a7	b0 	. 
	mov m,a			;21a8	77 	w 
l21a9h:
	pop h			;21a9	e1 	. 
	push psw			;21aa	f5 	. 
	xchg			;21ab	eb 	. 
	call s1fd9h		;21ac	cd d9 1f 	. . . 
	ldax d			;21af	1a 	. 
	xchg			;21b0	eb 	. 
	pop psw			;21b1	f1 	. 
	pop b			;21b2	c1 	. 
	ret			;21b3	c9 	. 
s21b4h:
	lxi d,0275ch		;21b4	11 5c 27 	. \ ' 
	xra a			;21b7	af 	. 
	stax d			;21b8	12 	. 
l21b9h:
	call s22f1h		;21b9	cd f1 22 	. . " 
	inx h			;21bc	23 	# 
	cpi '+'		;21bd	fe 2b 	. + 
	jz l21b9h		;21bf	ca b9 21 	. . . 
	cpi '-'		;21c2	fe 2d 	. - 
	jnz l21cdh		;21c4	c2 cd 21 	. . . 
	mvi a,0ffh		;21c7	3e ff 	> . 
	stax d			;21c9	12 	. 
	jmp l21b9h		;21ca	c3 b9 21 	. . . 
l21cdh:
	dcx h			;21cd	2b 	+ 
	lxi d,0ffffh		;21ce	11 ff ff 	. . . 
	xchg			;21d1	eb 	. 
	shld 0275ah		;21d2	22 5a 27 	" Z ' 
	xchg			;21d5	eb 	. 
	call s22f1h		;21d6	cd f1 22 	. . " 
	cpi 023h		;21d9	fe 23 	. # 
	inx h			;21db	23 	# 
	jz l220fh		;21dc	ca 0f 22 	. . " 
	dcx h			;21df	2b 	+ 
	call s2229h		;21e0	cd 29 22 	. ) " 
	lxi d,00001h		;21e3	11 01 00 	. . . 
	jnc l220fh		;21e6	d2 0f 22 	. . " 
	dcx d			;21e9	1b 	. 
l21eah:
	mov a,m			;21ea	7e 	~ 
	call s2229h		;21eb	cd 29 22 	. ) " 
	jnc l220ah		;21ee	d2 0a 22 	. . " 
	inx h			;21f1	23 	# 
	push h			;21f2	e5 	. 
	mov h,d			;21f3	62 	b 
	mov l,e			;21f4	6b 	k 
	dad d			;21f5	19 	. 
	dad h			;21f6	29 	) 
	dad d			;21f7	19 	. 
	dad h			;21f8	29 	) 
	jc l2215h		;21f9	da 15 22 	. . " 
	sui '0'		;21fc	d6 30 	. 0 
	mov e,a			;21fe	5f 	_ 
	mvi d,000h		;21ff	16 00 	. . 
	dad d			;2201	19 	. 
	jc l2215h		;2202	da 15 22 	. . " 
	xchg			;2205	eb 	. 
	pop h			;2206	e1 	. 
	jmp l21eah		;2207	c3 ea 21 	. . . 
l220ah:
	xchg			;220a	eb 	. 
	shld 0275ah		;220b	22 5a 27 	" Z ' 
	xchg			;220e	eb 	. 
l220fh:
	xchg			;220f	eb 	. 
	shld 02758h		;2210	22 58 27 	" X ' 
	xchg			;2213	eb 	. 
	ret			;2214	c9 	. 
l2215h:
	call s0534h		;2215	cd 34 05 	. 4 . 
	db 'NUMBER TOO LARGE',0
s2229h:
	cpi '0'		;2229	fe 30 	. 0 
	cmc			;222b	3f 	? 
	rnc 			;222c	d0 	. 
	cpi ':'		;222d	fe 3a 	. : 
	ret			;222f	c9 	. 
s2230h:
	push b			;2230	c5 	. 
	push d			;2231	d5 	. 
	lxi h,l26a4h+1		;2232	21 a5 26 	. . & 
	mov a,m			;2235	7e 	~ 
	push psw			;2236	f5 	. 
	xra a			;2237	af 	. 
	mov m,a			;2238	77 	w 
	lxi h,0286ch		;2239	21 6c 28 	. l ( 
	push h			;223c	e5 	. 
	mov c,a			;223d	4f 	O 
l223eh:
	mov a,c			;223e	79 	y 
	cpi 07fh		;223f	fe 7f 	.  
	jnc l22dah		;2241	d2 da 22 	. . " 
	pop d			;2244	d1 	. 
	push d			;2245	d5 	. 
	call s18beh		;2246	cd be 18 	. . . 
	push psw			;2249	f5 	. 
	cc l07beh		;224a	dc be 07 	. . . 
	pop psw			;224d	f1 	. 
	jc l223eh		;224e	da 3e 22 	. > " 
	call s24f0h		;2251	cd f0 24 	. . $ 
	cpi CR		;2254	fe 0d 	. . 
	jz l22dfh		;2256	ca df 22 	. . " 
	cpi LF		;2259	fe 0a 	. . 
	jz l22d5h		;225b	ca d5 22 	. . " 
	push h			;225e	e5 	. 
	lxi h,l223eh		;225f	21 3e 22 	. > " 
	xthl			;2262	e3 	. 
	cpi 01fh		;2263	fe 1f 	. . 
	jz l226dh		;2265	ca 6d 22 	. m " 
	cpi 07fh		;2268	fe 7f 	.  
	jnz l2274h		;226a	c2 74 22 	. t " 
l226dh:
	call s22cdh		;226d	cd cd 22 	. . " 
l2270h:
	rz			;2270	c8 	. 
	jmp s24fch		;2271	c3 fc 24 	. . $ 
l2274h:
	cpi 018h		;2274	fe 18 	. . 
	jz l227eh		;2276	ca 7e 22 	. ~ " 
	cpi 015h		;2279	fe 15 	. . 
	jnz l2283h		;227b	c2 83 22 	. . " 
l227eh:
	xchg			;227e	eb 	. 
	mvi c,000h		;227f	0e 00 	. . 
	mvi a,005h		;2281	3e 05 	> . 
l2283h:
	cpi 005h		;2283	fe 05 	. . 
	jz s24e3h		;2285	ca e3 24 	. . $ 
	cpi 008h		;2288	fe 08 	. . 
	jnz l2294h		;228a	c2 94 22 	. . " 
	call s22cdh		;228d	cd cd 22 	. . " 
	rz			;2290	c8 	. 
	jmp s0bb9h		;2291	c3 b9 0b 	. . . 
l2294h:
	cpi 01ch		;2294	fe 1c 	. . 
	jnz l22b9h		;2296	c2 b9 22 	. . " 
l2299h:
	call s22cdh		;2299	cd cd 22 	. . " 
	rz			;229c	c8 	. 
	mov b,a			;229d	47 	G 
	call s0bb9h		;229e	cd b9 0b 	. . . 
	mov a,b			;22a1	78 	x 
	call s22fah		;22a2	cd fa 22 	. . " 
	jz l2299h		;22a5	ca 99 22 	. . " 
l22a8h:
	call s22cdh		;22a8	cd cd 22 	. . " 
	rz			;22ab	c8 	. 
	mov b,a			;22ac	47 	G 
	call s0ed7h		;22ad	cd d7 0e 	. . . 
	jnc l22cah		;22b0	d2 ca 22 	. . " 
	call s0bb9h		;22b3	cd b9 0b 	. . . 
	jmp l22a8h		;22b6	c3 a8 22 	. . " 
l22b9h:
	cpi 012h		;22b9	fe 12 	. . 
	jnz l22c4h		;22bb	c2 c4 22 	. . " 
	call s24e3h		;22be	cd e3 24 	. . $ 
	jmp l07beh		;22c1	c3 be 07 	. . . 
l22c4h:
	cpi 003h		;22c4	fe 03 	. . 
	jz l04b3h		;22c6	ca b3 04 	. . . 
	mov m,a			;22c9	77 	w 
l22cah:
	inx h			;22ca	23 	# 
	inr c			;22cb	0c 	. 
	ret			;22cc	c9 	. 
s22cdh:
	mov a,c			;22cd	79 	y 
	ora a			;22ce	b7 	. 
	rz			;22cf	c8 	. 
	dcr c			;22d0	0d 	. 
	inr a			;22d1	3c 	< 
	dcx h			;22d2	2b 	+ 
	mov a,m			;22d3	7e 	~ 
	ret			;22d4	c9 	. 
l22d5h:
	mvi b,CR		;22d5	06 0d 	. . 
	jmp l22e1h		;22d7	c3 e1 22 	. . " 
l22dah:
	mvi a,CR		;22da	3e 0d 	> . 
	call s24fch		;22dc	cd fc 24 	. . $ 
l22dfh:
	mvi b,LF		;22df	06 0a 	. . 
l22e1h:
	sta 0286bh		;22e1	32 6b 28 	2 k ( 
	mvi m,000h		;22e4	36 00 	6 . 
	mov a,b			;22e6	78 	x 
	call s24fch		;22e7	cd fc 24 	. . $ 
	pop h			;22ea	e1 	. 
	pop psw			;22eb	f1 	. 
	sta l26a4h+1		;22ec	32 a5 26 	2 . & 
	pop d			;22ef	d1 	. 
	pop b			;22f0	c1 	. 
s22f1h:
	mov a,m			;22f1	7e 	~ 
	call s22fah		;22f2	cd fa 22 	. . " 
	rnz			;22f5	c0 	. 
	inx h			;22f6	23 	# 
	jmp s22f1h		;22f7	c3 f1 22 	. . " 
s22fah:
	cpi TAB		;22fa	fe 09 	. . 
	rz			;22fc	c8 	. 
	cpi ' '		;22fd	fe 20 	.   
	rz			;22ff	c8 	. 
	cpi LF		;2300	fe 0a 	. . 
	rz			;2302	c8 	. 
	cpi CR		;2303	fe 0d 	. . 
	ret			;2305	c9 	. 
PRINTSP:
	xthl			;2306	e3 	. 
	push psw			;2307	f5 	. 
	call PRINT$0		;2308	cd 18 23 	. . # 
	pop psw			;230b	f1 	. 
	xthl			;230c	e3 	. 
	ret			;230d	c9 	. 
PRINT:
	push psw			;230e	f5 	. 
	xchg			;230f	eb 	. 
	push h			;2310	e5 	. 
	call PRINT$0		;2311	cd 18 23 	. . # 
	pop h			;2314	e1 	. 
	xchg			;2315	eb 	. 
	pop psw			;2316	f1 	. 
	ret			;2317	c9 	. 
PRINT$0:
	mov a,m			;2318	7e 	~ 
	ora a			;2319	b7 	. 
	rz			;231a	c8 	. 
	call s24fch		;231b	cd fc 24 	. . $ 
	inx h			;231e	23 	# 
	jmp PRINT$0		;231f	c3 18 23 	. . # 
s2322h:
	push h			;2322	e5 	. 
	lhld l25b7h		;2323	2a b7 25 	* . % 
	mov a,m			;2326	7e 	~ 
	ora a			;2327	b7 	. 
	jz l2340h		;2328	ca 40 23 	. @ # 
	push psw			;232b	f5 	. 
	mvi m,000h		;232c	36 00 	6 . 
	inx h			;232e	23 	# 
	mov a,m			;232f	7e 	~ 
	inr a			;2330	3c 	< 
	jnz l2337h		;2331	c2 37 23 	. 7 # 
	lxi h,l25b9h		;2334	21 b9 25 	. . % 
l2337h:
	shld l25b7h		;2337	22 b7 25 	" . % 
	call l234eh		;233a	cd 4e 23 	. N # 
	pop psw			;233d	f1 	. 
	pop h			;233e	e1 	. 
	ret			;233f	c9 	. 
l2340h:
	pop h			;2340	e1 	. 
l2341h:
	call s23b4h		;2341	cd b4 23 	. . # 
	ani 07fh		;2344	e6 7f 	.  
	call s239ch		;2346	cd 9c 23 	. . # 
	jz l2341h		;2349	ca 41 23 	. A # 
	ora a			;234c	b7 	. 
	ret			;234d	c9 	. 
l234eh:
	push h			;234e	e5 	. 
l234fh:
	call s23c0h		;234f	cd c0 23 	. . # 
	jz l2395h		;2352	ca 95 23 	. . # 
l2355h:
	lhld l25b5h		;2355	2a b5 25 	* . % 
	mov a,m			;2358	7e 	~ 
	ora a			;2359	b7 	. 
	jnz l2383h		;235a	c2 83 23 	. . # 
	call s23b4h		;235d	cd b4 23 	. . # 
	ani 07fh		;2360	e6 7f 	.  
	cnz s239ch		;2362	c4 9c 23 	. . # 
	jz l234fh		;2365	ca 4f 23 	. O # 
	mov m,a			;2368	77 	w 
	inx h			;2369	23 	# 
	mov a,m			;236a	7e 	~ 
	inr a			;236b	3c 	< 
	jnz l2372h		;236c	c2 72 23 	. r # 
	lxi h,l25b9h		;236f	21 b9 25 	. . % 
l2372h:
	shld l25b5h		;2372	22 b5 25 	" . % 
	mov a,m			;2375	7e 	~ 
	ora a			;2376	b7 	. 
	jnz l2383h		;2377	c2 83 23 	. . # 
	call s23c0h		;237a	cd c0 23 	. . # 
	jnz l2355h		;237d	c2 55 23 	. U # 
	jmp l2391h		;2380	c3 91 23 	. . # 
l2383h:
	mvi a,021h		;2383	3e 21 	> ! 
	call s24fch		;2385	cd fc 24 	. . $ 
	mvi a,007h		;2388	3e 07 	> . 
	call s259bh		;238a	cd 9b 25 	. . % 
	xra a			;238d	af 	. 
	sta l269bh		;238e	32 9b 26 	2 . & 
l2391h:
	pop h			;2391	e1 	. 
	ori 001h		;2392	f6 01 	. . 
	ret			;2394	c9 	. 
l2395h:
	lhld l25b7h		;2395	2a b7 25 	* . % 
	mov a,m			;2398	7e 	~ 
	ora a			;2399	b7 	. 
	pop h			;239a	e1 	. 
	ret			;239b	c9 	. 
s239ch:
	cpi 016h		;239c	fe 16 	. . 
	rnz			;239e	c0 	. 
	call s23b4h		;239f	cd b4 23 	. . # 
	push psw			;23a2	f5 	. 
	call s259bh		;23a3	cd 9b 25 	. . % 
	pop psw			;23a6	f1 	. 
	cpi ESC		;23a7	fe 1b 	. . 
	jnz l23b2h		;23a9	c2 b2 23 	. . # 
	call s23b4h		;23ac	cd b4 23 	. . # 
	call s259bh		;23af	cd 9b 25 	. . % 
l23b2h:
	xra a			;23b2	af 	. 
	ret			;23b3	c9 	. 
s23b4h:
	mvi a,006h		;23b4	3e 06 	> . 
	call BIOS		;23b6	cd c7 23 	. . # 
	cpi 003h		;23b9	fe 03 	. . 
	rnz			;23bb	c0 	. 
	sta l26a6h+1		;23bc	32 a7 26 	2 . & 
	ret			;23bf	c9 	. 
s23c0h:
	mvi a,003h		;23c0	3e 03 	> . 
	call BIOS		;23c2	cd c7 23 	. . # 
	ora a			;23c5	b7 	. 
	ret			;23c6	c9 	. 
;
BIOS:
	push b			;SAVE B
	push d			;SAVE D
	push h			;SAVE H
	lxi h,POPHDB		;23ca	21 cc 1f 	. . . 
	push h			;PUSH POPHDB ON STACK
	lhld BIOSJVT		;HL = BIOS JUMP VECTOR TABLE 
	call BIDX		;HL=HL+A
	mov a,c			;23d4	79 	y 
	pchl			;JUMP TO BIOS - RETURN TO POPHDB
;
s23d6h:
	push psw			;23d6	f5 	. 
	lda l26a4h+1		;23d7	3a a5 26 	: . & 
	ora a			;23da	b7 	. 
	jnz l23e3h		;23db	c2 e3 23 	. . # 
	call l234eh		;23de	cd 4e 23 	. N # 
	pop psw			;23e1	f1 	. 
	ret			;23e2	c9 	. 
l23e3h:
	call s23fah		;23e3	cd fa 23 	. . # 
	jnz l23ebh		;23e6	c2 eb 23 	. . # 
	pop psw			;23e9	f1 	. 
	ret			;23ea	c9 	. 
l23ebh:
	call s0534h		;23eb	cd 34 05 	. 4 . 
	db 'INTERRUPTED',0
s23fah:
	call l234eh		;23fa	cd 4e 23 	. N # 
	push h			;23fd	e5 	. 
	lxi h,l26a6h+1		;23fe	21 a7 26 	. . & 
	mov a,m			;2401	7e 	~ 
	mvi m,000h		;2402	36 00 	6 . 
	ora a			;2404	b7 	. 
	pop h			;2405	e1 	. 
	rz			;2406	c8 	. 
	push b			;2407	c5 	. 
	push h			;2408	e5 	. 
	lxi h,l25b9h		;2409	21 b9 25 	. . % 
	shld l25b5h		;240c	22 b5 25 	" . % 
	shld l25b7h		;240f	22 b7 25 	" . % 
	mvi c,064h		;2412	0e 64 	. d 
	xra a			;2414	af 	. 
l2415h:
	mov m,a			;2415	77 	w 
	inx h			;2416	23 	# 
	dcr c			;2417	0d 	. 
	jnz l2415h		;2418	c2 15 24 	. . $ 
	dcr c			;241b	0d 	. 
	pop h			;241c	e1 	. 
	pop b			;241d	c1 	. 
	ret			;241e	c9 	. 
s241fh:
	mvi a,CR		;241f	3e 0d 	> . 
s2421h:
	cpi CR		;2421	fe 0d 	. . 
	cz s2430h		;2423	cc 30 24 	. 0 $ 
	call s2465h		;2426	cd 65 24 	. e $ 
	rnc 			;2429	d0 	. 
	call s2430h		;242a	cd 30 24 	. 0 $ 
	jmp s24fch		;242d	c3 fc 24 	. . $ 
s2430h:
	push psw			;2430	f5 	. 
	inr e			;2431	1c 	. 
	jz l243fh		;2432	ca 3f 24 	. ? $ 
	dcr e			;2435	1d 	. 
	nop			;2436	00 	. 
	nop			;2437	00 	. 
	nop			;2438	00 	. 
	inr e			;2439	1c 	. 
	cpi TAB		;243a	fe 09 	. . 
	jnz l2462h		;243c	c2 62 24 	. b $ 
l243fh:
	lda l26a1h		;243f	3a a1 26 	: . & 
	cpi CR		;2442	fe 0d 	. . 
	jz l2462h		;2444	ca 62 24 	. b $ 
	call l234eh		;2447	cd 4e 23 	. N # 
	lda EREOL		;244a	3a bb 01 	: . . 
	ora a			;244d	b7 	. 
	cnz ERAEOL		;244e	c4 39 02 	. 9 . 
	jnz l2462h		;2451	c2 62 24 	. b $ 
l2454h:
	mvi a,' '		;2454	3e 20 	>   
	call s2465h		;2456	cd 65 24 	. e $ 
	jc l2462h		;2459	da 62 24 	. b $ 
	call l234eh		;245c	cd 4e 23 	. N # 
	jmp l2454h		;245f	c3 54 24 	. T $ 
l2462h:
	dcr e			;2462	1d 	. 
	pop psw			;2463	f1 	. 
	ret			;2464	c9 	. 
s2465h:
	call s246ch		;2465	cd 6c 24 	. l $ 
	rc			;2468	d8 	. 
	jmp s24fch		;2469	c3 fc 24 	. . $ 
s246ch:
	cpi CR		;246c	fe 0d 	. . 
	rz			;246e	c8 	. 
	cpi LF		;246f	fe 0a 	. . 
	rz			;2471	c8 	. 
	push b			;2472	c5 	. 
	mov b,a			;2473	47 	G 
	lda CURYPOS		;2474	3a 84 26 	: . & 
	mov c,a			;2477	4f 	O 
	mov a,b			;2478	78 	x 
	cpi TAB		;2479	fe 09 	. . 
	jnz l2483h		;247b	c2 83 24 	. . $ 
	mov a,c			;247e	79 	y 
	ori 007h		;247f	f6 07 	. . 
	mov c,a			;2481	4f 	O 
	cma			;2482	2f 	/ 
l2483h:
	cpi ' '		;2483	fe 20 	.   
	jnc l248eh		;2485	d2 8e 24 	. . $ 
	cpi ESC		;2488	fe 1b 	. . 
	jz l248eh		;248a	ca 8e 24 	. . $ 
	inr c			;248d	0c 	. 
l248eh:
	inr c			;248e	0c 	. 
	lda WID		;248f	3a ba 01 	: . . 
	inr c			;2492	0c 	. 
	cmp c			;2493	b9 	. 
	mov a,b			;2494	78 	x 
	pop b			;2495	c1 	. 
	ret			;2496	c9 	. 
s2497h:
	call s249ah		;2497	cd 9a 24 	. . $ 
s249ah:
	push h			;249a	e5 	. 
l249bh:
	push psw			;249b	f5 	. 
	lxi h,CURYPOS		;249c	21 84 26 	. . & 
	cpi ' '		;249f	fe 20 	.   
	jc l24b7h		;24a1	da b7 24 	. . $ 
l24a4h:
	inr m			;24a4	34 	4 
	lda WID		;24a5	3a ba 01 	: . . 
	cmp m			;24a8	be 	. 
	jz l24afh		;24a9	ca af 24 	. . $ 
	jnc l24d1h		;24ac	d2 d1 24 	. . $ 
l24afh:
	mvi m,002h		;24af	36 02 	6 . 
	dcx h			;24b1	2b 	+ 
	inr m			;24b2	34 	4 
	pop psw			;24b3	f1 	. 
	jmp l249bh		;24b4	c3 9b 24 	. . $ 
l24b7h:
	cpi ESC		;24b7	fe 1b 	. . 
	jz l24a4h		;24b9	ca a4 24 	. . $ 
	cpi LF		;24bc	fe 0a 	. . 
	jz l24cfh		;24be	ca cf 24 	. . $ 
	cpi CR		;24c1	fe 0d 	. . 
	jz l24d4h		;24c3	ca d4 24 	. . $ 
	cpi TAB		;24c6	fe 09 	. . 
	jz l24dch		;24c8	ca dc 24 	. . $ 
	inr m			;24cb	34 	4 
	jmp l24a4h		;24cc	c3 a4 24 	. . $ 
l24cfh:
	dcx h			;24cf	2b 	+ 
	inr m			;24d0	34 	4 
l24d1h:
	pop psw			;24d1	f1 	. 
	pop h			;24d2	e1 	. 
	ret			;24d3	c9 	. 
l24d4h:
	call l234eh		;24d4	cd 4e 23 	. N # 
	mvi m,000h		;24d7	36 00 	6 . 
	pop psw			;24d9	f1 	. 
	pop h			;24da	e1 	. 
	ret			;24db	c9 	. 
l24dch:
	mov a,m			;24dc	7e 	~ 
	ori 007h		;24dd	f6 07 	. . 
	mov m,a			;24df	77 	w 
	jmp l24a4h		;24e0	c3 a4 24 	. . $ 
s24e3h:
	push psw			;24e3	f5 	. 
	mvi a,CR		;24e4	3e 0d 	> . 
	call s24fch		;24e6	cd fc 24 	. . $ 
	mvi a,LF		;24e9	3e 0a 	> . 
	call s24fch		;24eb	cd fc 24 	. . $ 
	pop psw			;24ee	f1 	. 
	ret			;24ef	c9 	. 
s24f0h:
	cpi BS			;24f0	fe 08 	. . 
	rz			;24f2	c8 	. 
	cpi DEL		;24f3	fe 7f 	.  
	rz			;24f5	c8 	. 
	cpi 01fh		;24f6	fe 1f 	. . 
	rz			;24f8	c8 	. 
	cpi 01ch		;24f9	fe 1c 	. . 
	rz			;24fb	c8 	. 
s24fch:
	push psw			;24fc	f5 	. 
	push h			;24fd	e5 	. 
	cpi ESC		;24fe	fe 1b 	. . 
	jnz l2505h		;2500	c2 05 25 	. . % 
	mvi a,'$'		;2503	3e 24 	> $ 
l2505h:
	cpi DEL		;2505	fe 7f 	.  
	jnz l250ch		;2507	c2 0c 25 	. . % 
	mvi a,07eh		;250a	3e 7e 	> ~ 
l250ch:
	push psw			;250c	f5 	. 
	call s246ch		;250d	cd 6c 24 	. l $ 
	jnc l251bh		;2510	d2 1b 25 	. . % 
	call PRINTSP		;2513	cd 06 23 	. . # 
	db CR,LF,'>>',0

l251bh:
	lhld l25a7h		;251b	2a a7 25 	* . % 
	cpi CR		;251e	fe 0d 	. . 
	jnz l2529h		;2520	c2 29 25 	. ) % 
	lxi h,l25a9h		;2523	21 a9 25 	. . % 
	jmp l255bh		;2526	c3 5b 25 	. [ % 
l2529h:
	cpi TAB		;2529	fe 09 	. . 
	jnz l2548h		;252b	c2 48 25 	. H % 
	inx h			;252e	23 	# 
	mov a,m			;252f	7e 	~ 
	inr a			;2530	3c 	< 
	jnz l2535h		;2531	c2 35 25 	. 5 % 
	dcx h			;2534	2b 	+ 
l2535h:
	mvi m,000h		;2535	36 00 	6 . 
l2537h:
	mvi a,' '		;2537	3e 20 	>   
	call s2567h		;2539	cd 67 25 	. g % 
	inr m			;253c	34 	4 
	lda CURYPOS		;253d	3a 84 26 	: . & 
	ani 007h		;2540	e6 07 	. . 
	jnz l2537h		;2542	c2 37 25 	. 7 % 
	jmp l255bh		;2545	c3 5b 25 	. [ % 
l2548h:
	cpi ' '		;2548	fe 20 	.   
	jnc l255bh		;254a	d2 5b 25 	. [ % 
	cpi LF		;254d	fe 0a 	. . 
	jz l255bh		;254f	ca 5b 25 	. [ % 
	mvi a,'^'		;2552	3e 5e 	> ^ 
	call s2567h		;2554	cd 67 25 	. g % 
	pop psw			;2557	f1 	. 
	adi 040h		;2558	c6 40 	. @ 
	push psw			;255a	f5 	. 
l255bh:
	shld l25a7h		;255b	22 a7 25 	" . % 
	pop psw			;255e	f1 	. 
	pop h			;255f	e1 	. 
	cpi TAB		;2560	fe 09 	. . 
	cnz s2567h		;2562	c4 67 25 	. g % 
	pop psw			;2565	f1 	. 
	ret			;2566	c9 	. 
s2567h:
	push psw			;2567	f5 	. 
	call s259bh		;2568	cd 9b 25 	. . % 
	pop psw			;256b	f1 	. 
	push psw			;256c	f5 	. 
	push h			;256d	e5 	. 
	call s2574h		;256e	cd 74 25 	. t % 
	pop h			;2571	e1 	. 
	pop psw			;2572	f1 	. 
	ret			;2573	c9 	. 
s2574h:
	lxi h,CURYPOS		;2574	21 84 26 	. . & 
	cpi LF		;2577	fe 0a 	. . 
	jz l2586h		;2579	ca 86 25 	. . % 
	cpi CR		;257c	fe 0d 	. . 
	jnz l2584h		;257e	c2 84 25 	. . % 
	mvi m,000h		;2581	36 00 	6 . 
	ret			;2583	c9 	. 
l2584h:
	inr m			;2584	34 	4 
	ret			;2585	c9 	. 
l2586h:
	dcx h			;2586	2b 	+ 
	inr m			;2587	34 	4 
	lda HITE		;2588	3a b9 01 	: . . 
	dcr a			;258b	3d 	= 
	cmp m			;258c	be 	. 
	rnc 			;258d	d0 	. 
	dcr m			;258e	35 	5 
	dcx h			;258f	2b 	+ 
	inr m			;2590	34 	4 
	ret			;2591	c9 	. 
l2592h:
	push psw		;SAVE PSW
	xra a			;A=0
	sta l26a1h		;26A1=0
	pop psw			;RESTORE A
	jmp l259eh		;2598	c3 9e 25 	. . % 
s259bh:
	sta l26a1h		;259b	32 a1 26 	2 . & 
l259eh:
	push b			;SAVE B
	mov c,a			;C=A CHAR TO PRINT
	mvi a,009h		;BIOS CONOUT (3)
	call BIOS		;25a2	cd c7 23 	. . # 
	pop b			;RESTORE B
	ret			;25a6	c9 	. 
l25a7h:
	xra c			;25a7	a9 	. 
	dcr h			;25a8	25 	% 
l25a9h:
	rst 7			;25a9	ff 	. 
	nop			;25aa	00 	. 
	nop			;25ab	00 	. 
	nop			;25ac	00 	. 
	nop			;25ad	00 	. 
	nop			;25ae	00 	. 
	nop			;25af	00 	. 
l25b0h:
	nop			;25b0	00 	. 
	nop			;25b1	00 	. 
	nop			;25b2	00 	. 
l25b3h:
	nop			;25b3	00 	. 
l25b4h:
	rst 7			;25b4	ff 	. 
l25b5h:
	cmp c			;25b5	b9 	. 
	dcr h			;25b6	25 	% 
l25b7h:
	cmp c			;25b7	b9 	. 
l25b8h:
	dcr h			;25b8	25 	% 
l25b9h:
	nop			;25b9	00 	. 
	nop			;25ba	00 	. 
	nop			;25bb	00 	. 
	nop			;25bc	00 	. 
	nop			;25bd	00 	. 
	nop			;25be	00 	. 
	nop			;25bf	00 	. 
	nop			;25c0	00 	. 
	nop			;25c1	00 	. 
	nop			;25c2	00 	. 
	nop			;25c3	00 	. 
	nop			;25c4	00 	. 
	nop			;25c5	00 	. 
	nop			;25c6	00 	. 
	nop			;25c7	00 	. 
	nop			;25c8	00 	. 
l25c9h:
	nop			;25c9	00 	. 
l25cah:
	nop			;25ca	00 	. 
l25cbh:
	nop			;25cb	00 	. 
	nop			;25cc	00 	. 
l25cdh:
	nop			;25cd	00 	. 
	nop			;25ce	00 	. 
l25cfh:
	nop			;25cf	00 	. 
l25d0h:
	nop			;25d0	00 	. 
	nop			;25d1	00 	. 
l25d2h:
	nop			;25d2	00 	. 
l25d3h:
	nop			;25d3	00 	. 
	nop			;25d4	00 	. 
	nop			;25d5	00 	. 
	nop			;25d6	00 	. 
	nop			;25d7	00 	. 
	nop			;25d8	00 	. 
	nop			;25d9	00 	. 
	nop			;25da	00 	. 
	nop			;25db	00 	. 
	nop			;25dc	00 	. 
	nop			;25dd	00 	. 
	nop			;25de	00 	. 
l25dfh:
	nop			;25df	00 	. 
	nop			;25e0	00 	. 
	nop			;25e1	00 	. 
	nop			;25e2	00 	. 
	nop			;25e3	00 	. 
	nop			;25e4	00 	. 
	nop			;25e5	00 	. 
	nop			;25e6	00 	. 
	nop			;25e7	00 	. 
	nop			;25e8	00 	. 
	nop			;25e9	00 	. 
	nop			;25ea	00 	. 
	nop			;25eb	00 	. 
	nop			;25ec	00 	. 
	nop			;25ed	00 	. 
	nop			;25ee	00 	. 
	nop			;25ef	00 	. 
	nop			;25f0	00 	. 
	nop			;25f1	00 	. 
	nop			;25f2	00 	. 
l25f3h:
	nop			;25f3	00 	. 
l25f4h:
	nop			;25f4	00 	. 
	nop			;25f5	00 	. 
l25f6h:
	nop			;25f6	00 	. 
	nop			;25f7	00 	. 
l25f8h:
	nop			;25f8	00 	. 
	nop			;25f9	00 	. 
	nop			;25fa	00 	. 
	nop			;25fb	00 	. 
	nop			;25fc	00 	. 
	nop			;25fd	00 	. 
	nop			;25fe	00 	. 
	nop			;25ff	00 	. 
	nop			;2600	00 	. 
	nop			;2601	00 	. 
	nop			;2602	00 	. 
	nop			;2603	00 	. 
	nop			;2604	00 	. 
	nop			;2605	00 	. 
	nop			;2606	00 	. 
	nop			;2607	00 	. 
	nop			;2608	00 	. 
	nop			;2609	00 	. 
	nop			;260a	00 	. 
	nop			;260b	00 	. 
	nop			;260c	00 	. 
	nop			;260d	00 	. 
	nop			;260e	00 	. 
	nop			;260f	00 	. 
	nop			;2610	00 	. 
	nop			;2611	00 	. 
	nop			;2612	00 	. 
	nop			;2613	00 	. 
	nop			;2614	00 	. 
	nop			;2615	00 	. 
	nop			;2616	00 	. 
	nop			;2617	00 	. 
	nop			;2618	00 	. 
l2619h:
	nop			;2619	00 	. 
	nop			;261a	00 	. 
l261bh:
	nop			;261b	00 	. 
	nop			;261c	00 	. 
l261dh:
	rst 7			;261d	ff 	. 
	nop			;261e	00 	. 
	nop			;261f	00 	. 
	nop			;2620	00 	. 
	nop			;2621	00 	. 
	nop			;2622	00 	. 
	nop			;2623	00 	. 
	nop			;2624	00 	. 
	nop			;2625	00 	. 
	nop			;2626	00 	. 
	nop			;2627	00 	. 
	nop			;2628	00 	. 
	nop			;2629	00 	. 
	nop			;262a	00 	. 
	nop			;262b	00 	. 
	nop			;262c	00 	. 
	nop			;262d	00 	. 
	nop			;262e	00 	. 
	nop			;262f	00 	. 
	nop			;2630	00 	. 
	nop			;2631	00 	. 
	nop			;2632	00 	. 
	nop			;2633	00 	. 
	nop			;2634	00 	. 
	nop			;2635	00 	. 
	nop			;2636	00 	. 
	nop			;2637	00 	. 
	nop			;2638	00 	. 
	nop			;2639	00 	. 
	nop			;263a	00 	. 
	nop			;263b	00 	. 
	nop			;263c	00 	. 
	nop			;263d	00 	. 
	nop			;263e	00 	. 
	nop			;263f	00 	. 
l2640h:
	nop			;2640	00 	. 
	nop			;2641	00 	. 
	nop			;2642	00 	. 
	nop			;2643	00 	. 
	nop			;2644	00 	. 
	nop			;2645	00 	. 
	nop			;2646	00 	. 
s2647h:
	nop			;2647	00 	. 
	nop			;2648	00 	. 
	nop			;2649	00 	. 
	nop			;264a	00 	. 
	nop			;264b	00 	. 
	nop			;264c	00 	. 
	nop			;264d	00 	. 
	nop			;264e	00 	. 
	nop			;264f	00 	. 
	nop			;2650	00 	. 
	nop			;2651	00 	. 
	nop			;2652	00 	. 
	nop			;2653	00 	. 
	nop			;2654	00 	. 
	nop			;2655	00 	. 
	nop			;2656	00 	. 
	nop			;2657	00 	. 
	nop			;2658	00 	. 
	nop			;2659	00 	. 
s265ah:
	nop			;265a	00 	. 
	nop			;265b	00 	. 
	nop			;265c	00 	. 
	nop			;265d	00 	. 
	nop			;265e	00 	. 
	nop			;265f	00 	. 
s2660h:
	nop			;2660	00 	. 
	nop			;2661	00 	. 
	nop			;2662	00 	. 
	nop			;2663	00 	. 
	nop			;2664	00 	. 
	nop			;2665	00 	. 
	nop			;2666	00 	. 
	nop			;2667	00 	. 
	nop			;2668	00 	. 
	nop			;2669	00 	. 
	nop			;266a	00 	. 
	nop			;266b	00 	. 
s266ch:
	nop			;266c	00 	. 
	nop			;266d	00 	. 
	nop			;266e	00 	. 
	nop			;266f	00 	. 
	nop			;2670	00 	. 
	nop			;2671	00 	. 
s2672h:
	nop			;2672	00 	. 
	nop			;2673	00 	. 
	nop			;2674	00 	. 
	nop			;2675	00 	. 
	nop			;2676	00 	. 
	nop			;2677	00 	. 
l2678h:
	nop			;2678	00 	. 
	nop			;2679	00 	. 
	nop			;267a	00 	. 
	nop			;267b	00 	. 
	nop			;267c	00 	. 
	nop			;267d	00 	. 
	nop			;267e	00 	. 
	nop			;267f	00 	. 
l2680h:
	push	h			;2680	e5 	. 
	push	h			;2681	e5 	. 
l2682h:
	push	h			;2682	e5 	. 
CURXPOS:
	push	h			;2683	e5 	. 
CURYPOS:
	push	h			;2684	e5 	. 
l2685h:
	push	h			;2685	e5 	. 
	push	h			;2686	e5 	. 
l2687h:
	push	h			;2687	e5 	. 
	push	h			;2688	e5 	. 
l2689h:
	push	h			;2689	e5 	. 
	push	h			;268a	e5 	. 
l268bh:
	push	h			;268b	e5 	. 
	push	h			;268c	e5 	. 
l268dh:
	push	h			;268d	e5 	. 
	push	h			;268e	e5 	. 
l268fh:
	push	h			;268f	e5 	. 
l2690h:
	push	h			;2690	e5 	. 
l2691h:
	push	h			;2691	e5 	. 
	push	h			;2692	e5 	. 
l2693h:
	push	h			;2693	e5 	. 
	push	h			;2694	e5 	. 
l2695h:
	push	h			;2695	e5 	. 
	push	h			;2696	e5 	. 
l2697h:
	push	h			;2697	e5 	. 
	push	h			;2698	e5 	. 
l2699h:
	push	h			;2699	e5 	. 
	push	h			;269a	e5 	. 
l269bh:
	push	h			;269b	e5 	. 
l269ch:
	push	h			;269c	e5 	. 
l269dh:
	push	h			;269d	e5 	. 
l269eh:
	push	h			;269e	e5 	. 
l269fh:
	push	h			;269f	e5 	. 
l26a0h:
	push	h			;26a0	e5 	. 
l26a1h:
	push	h			;26a1	e5 	. 
l26a2h:
	push	h			;26a2	e5 	. 
l26a3h:
	push	h			;26a3	e5 	. 
l26a4h:
	push	h			;26a4	e5 	. 
l26a5h:
	push	h			;26a5	e5 	. 
l26a6h:
	push	h			;26a6	e5 	. 
l26a7h:
	push	h			;26a7	e5 	. 
l26a8h:
	push	h			;26a8	e5 	. 
	push	h			;26a9	e5 	. 
l26aah:
	push	h			;26aa	e5 	. 
TMPFCB:
	push	h			;26ab	e5 	. 
	push	h			;26ac	e5 	. 
	push	h			;26ad	e5 	. 
	push	h			;26ae	e5 	. 
	push	h			;26af	e5 	. 
	push	h			;26b0	e5 	. 
	push	h			;26b1	e5 	. 
	push	h			;26b2	e5 	. 
	push	h			;26b3	e5 	. 
l26b4h:
	push	h			;26b4	e5 	. 
	push	h			;26b5	e5 	. 
	push	h			;26b6	e5 	. 
	push	h			;26b7	e5 	. 
	push	h			;26b8	e5 	. 
	push	h			;26b9	e5 	. 
	push	h			;26ba	e5 	. 
	push	h			;26bb	e5 	. 
	push	h			;26bc	e5 	. 
	push	h			;26bd	e5 	. 
	push	h			;26be	e5 	. 
	push	h			;26bf	e5 	. 
	push	h			;26c0	e5 	. 
	push	h			;26c1	e5 	. 
	push	h			;26c2	e5 	. 
	push	h			;26c3	e5 	. 
	push	h			;26c4	e5 	. 
	push	h			;26c5	e5 	. 
	push	h			;26c6	e5 	. 
	push	h			;26c7	e5 	. 
	push	h			;26c8	e5 	. 
	push	h			;26c9	e5 	. 
	push	h			;26ca	e5 	. 
	push	h			;26cb	e5 	. 
	push	h			;26cc	e5 	. 
l26cdh:
	push	h			;26cd	e5 	. 
	push	h			;26ce	e5 	. 
	push	h			;26cf	e5 	. 
BAKFCB:
	push	h			;26d0	e5 	. 
l26d1h:
	push	h			;26d1	e5 	. 
	push	h			;26d2	e5 	. 
	push	h			;26d3	e5 	. 
	push	h			;26d4	e5 	. 
	push	h			;26d5	e5 	. 
	push	h			;26d6	e5 	. 
	push	h			;26d7	e5 	. 
	push	h			;26d8	e5 	. 
	push	h			;26d9	e5 	. 
	push	h			;26da	e5 	. 
	push	h			;26db	e5 	. 
	push	h			;26dc	e5 	. 
	push	h			;26dd	e5 	. 
	push	h			;26de	e5 	. 
	push	h			;26df	e5 	. 
	push	h			;26e0	e5 	. 
	push	h			;26e1	e5 	. 
	push	h			;26e2	e5 	. 
	push	h			;26e3	e5 	. 
	push	h			;26e4	e5 	. 
	push	h			;26e5	e5 	. 
	push	h			;26e6	e5 	. 
	push	h			;26e7	e5 	. 
	push	h			;26e8	e5 	. 
	push	h			;26e9	e5 	. 
	push	h			;26ea	e5 	. 
	push	h			;26eb	e5 	. 
	push	h			;26ec	e5 	. 
	push	h			;26ed	e5 	. 
	push	h			;26ee	e5 	. 
	push	h			;26ef	e5 	. 
	push	h			;26f0	e5 	. 
	push	h			;26f1	e5 	. 
	push	h			;26f2	e5 	. 
l26f3h:
	push	h			;26f3	e5 	. 
	push	h			;26f4	e5 	. 
l26f5h:
	push	h			;26f5	e5 	. 
l26f6h:
	push	h			;26f6	e5 	. 
	push	h			;26f7	e5 	. 
l26f8h:
	push	h			;26f8	e5 	. 
	push	h			;26f9	e5 	. 
	push	h			;26fa	e5 	. 
	push	h			;26fb	e5 	. 
	push	h			;26fc	e5 	. 
	push	h			;26fd	e5 	. 
	push	h			;26fe	e5 	. 
	push	h			;26ff	e5 	. 
