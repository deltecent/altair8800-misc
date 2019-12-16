sboot	equ	07dh
wait	equ	0fch
sect	equ	0fah
dcom	equ	0f8h
ddata	equ	0fbh
dstat	equ	0f8h

	org	0100h

boot:	in	wait		; Wait for home
	xra	a		; Complete
	mov	l,a		; Set L=0
	mov	h,a		; Set H=0
	inr	a		; Set A=1
	out	sect		; Sector = 1
	mvi	a,8ch		; Read Sector
	out	dcom

rloop:	in	wait		; Wait for DRQ or INTRQ
	ora	a		; Set Flags
	jp	rdone		; Done if INTRQ
	in	ddata		; Read a Byte of Data
	mov	m,a		; Put in to Memory
	inx	h		; Increment Pointer
	jmp	rloop		; Do it Again

rdone:	in	dstat		; Read Disk Status
	ora	a		; Set Flags
	jz	sboot		; If Zero, Go to sboot
	hlt

