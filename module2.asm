	DEVICE ZXSPECTRUM48
	LABELSLIST "c:/zx/US0376/user.l"
	org #6000
start: 

	call screen.prepare
	ld a,0
.loop2

	ld de,#4000
	ld ix,map_1
	ld c,a
	ld b,0
	add ix,bc
	push af
.loop1
	ld a,(ix)
;	out (#fe),a
	inc ix
	ld b,8
	push de
	call screen.out_4x4
	pop de
	ld a,e
	add a,#04
	ld e,a	
	and %00011111
	jr nz,.loop1
	ld a,e
	add 3*#20
	ld e,a
	jr nz,.loop1
	ld a,d
	add 8
	ld d,a
	cp #58
	jr nz,.loop1
	halt
	pop af

	and #07
	jr .loop2
map_1
;	defs 48,0
	db 0,0,0,0,0,0,0,0
	db 4,5,6,7,4,8,9,4
	db 1,3,1,1,1,1,1,10
	db 12,13,13,13,14,7,2,10
	db 10,0,0,0,0,14,2,11
	db 7,0,4,8,9,4,2,10
	module screen
; a номер фона
; de адрес в экране
out_4x4:
	ld l,0
	sra a
	rr l
	ld h,a
	ld a,d
	push hl
	ld bc,graff
	add hl,bc
	exx
	pop hl
	sra h
	rr l
	sra h
	rr l
	sra h
	rr l
	ld bc,graff_attr
	add hl,bc
	rrca
	rrca
	rrca
	and %0000011
	add a,#58
	ld d,a
	exx
// вывод фона 4 на 4 без атрибутов в сетку 8 на 6 экрана
// hl - спрайт
// de - адресс в экране
// hl' - атрибуты спрайта
// de' - адрес атрибутов
	ld b,d
	ld c,#80
.loop1
	ld a,e
	dup 7
	ldi	
	ldi
	ldi
	ldi
	dec de
	ld e,a
	inc d
	edup
	ldi	
	ldi
	ldi
	ldi
	dec de
	exx
	ld e,a
	ldi
	ldi
	ldi
	ldi
	dec de
	exx
	add #20
	ld e,a
	ld d,b
	and %01100000
	jp nz,.loop1
	ret

prepare 
	ld hl,graff
	ld de,graff_attr
	ld ix,graff
	ld c,0+(graff_attr-graff)/(4*4*9)
.loop3
	ld b,#80
.loop1
	ld a,(hl)
	ld (ix),a
	inc hl
	inc ix
	djnz .loop1
	ld b,#10
.loop2
	ld a,(hl)
	ld (de),a
	inc hl
	inc de
	djnz .loop2
	dec c
	jr nz,.loop3
	ret
	endmodule
;----------------------
graff
	incbin "riskd1.bin"
graff_attr
	defs 48*16

	display $-start
	savesna "spr_bump.sna",start

