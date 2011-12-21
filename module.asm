//macros
	macro down hr, lr
	inc hr
	ld a,hr
	and %00000111
	jr nz,.end
	ld a,hr
	sub #08
	ld hr,a
	ld a,lr
	add #20
	ld lr,a
	jr nc,.end
	ld a,hr
	add a,#08
	ld hr,a
.end
	endm
// end macros
//---------------------------------
	module sprite
get_spr:
	push de
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	ld de,spr_table
	add hl,de
	ld b,(hl)
	inc hl
	ld c,(hl)
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	push de
	pop hl
	pop de
	ret
// de coordinates d-y e-x
// hl sprite mask-data
// bc b-high c-len
out_mask_pixel:
	push hl
	pop ix
	call addr_cal
// ix sprite gorizontal data,mask
// de screen addr
// bc b-high c-len
.loop1
	push bc
	push de
	ld a,c
.loop
	exa
	ld l,(ix)
	inc ix
	ld b,(hl)
	inc h
	ld c,(hl)
	dec h
	ld l,(ix)
	inc ix
	ld a,(de)
	or (hl)
	inc h
	xor b
	ld (de),a
	inc e
	ld a,(de)
	or (hl)
	dec h
	xor c
	ld (de),a
	exa
	dec a
	jr nz,.loop
	pop de
	down d,e
	pop bc
	djnz .loop1
	ret
//---------------------
// de coordinates d-y e-x
// hl sprite data
// bc b-high c-len
out_pixel:
	push hl
	pop ix
	call addr_cal
// ix sprite gorizontal data
// de screen addr
// bc b-high c-len
.loop1
	push bc,de
.loop
	ld l,(ix)
	inc ix
	ld a,(de)
	or (hl)
	inc h
	ld (de),a
	inc e
	ld a,(de)
	or (hl)
	dec h
	ld (de),a
	dec c
	jr nz,.loop
	pop de,bc
	down d,e
	djnz .loop1
	ret
//--------------------
// in  :d -y e -x
// out :de addr
//      h table + offset
addr_cal:
	ld a,e
	add a
	and %00001110
	add high mask_table
	ld h,a
	ld a,e
	and %11111000
	rrca
	rrca
	rrca
	ld e,a
	ld a,d
	rlca
	rlca
	and %11100000
	add e
	ld e,a
	ld a,d
	and %00000111
	ld l,a
	ld a,d
	rrca
	rrca
	rrca
	and %00011100
	add l
	add #40
	ld d,a
	ret
//--------------------
prepare:
	ld l,0	
.loop
	ld h,high mask_table
	ld c,#00
	ld a,l
	ld b,8
.loop1
	ld (hl),a
	inc h
	ld (hl),c
	inc h
	srl a
	rr c
	djnz .loop1
	inc l
	jr nz,.loop
//---------------
	ld hl,starwars1000
	ld bc,#20*3*#100+8
	ld de,#0000
.loop2
	add hl,de
	push bc
.loop3
	ld a,(hl)
	cpl
	inc hl
	xor (hl)
	dec hl
	ld (hl),a
	inc hl
	inc hl
	djnz .loop3
	pop bc
	dec c
	jr nz,.loop2
	ret
//------------
spr_table:
	dw	#320,starwars1000
	dw	#320,starwars1001
	dw	#320,starwars1002
	dw	#320,starwars1003
	dw	#320,starwars1004
	dw	#320,starwars1005
	dw	#320,starwars1006
	dw	#320,starwars1007	
	endmodule
//-----------------------------
	MODULE key
get:
	push bc,hl
	ld bc,#7ffe
	ld hl,data
.loop
	in a,(c)
	cpl
	and %00011111
	ld (hl),a
	inc hl
	rrc b
	jr c,.loop
	pop hl,bc
	ret
data:
	defs 8,0
old:
	defs 8,0
time:
	byte 0
input:
	push hl,bc
	ld hl,data
	ld c,1
.loop2
	ld b,5
	ld a,(hl)
.loop
	rrca
	jr c,.exit
	inc c
	djnz .loop
	inc hl
	ld a,c
	sub 41
	jr nz,.loop2
	ld c,a
.exit
	ld a,c
	pop bc,hl
	ret
stat:	
	push bc,hl
	ld c,a
	ld b,0
	ld hl,.table
	add hl,bc
	ld a,(hl)
	or #47
	ld (.bit),a
	ld a,(hl)
	and %00000111
	ld c,a
	ld hl,data
	add hl,bc
	ld a,(hl)
.bit	equ $+1
	bit 0,a
	pop hl,bc
	ret z
	scf
	ret
.table
	db 0,8,16,24,32
	db 1,9,17,25,33
	db 2,10,18,26,34
	db 3,11,19,27,35
	db 4,12,20,28,36
	db 5,13,21,29,37
	db 6,14,22,30,38
	db 7,15,22,31,39
print:
	ld hl,.table
.loop1
	dec a
	jp z,.label
	push af
	xor a
.loop
	ld a,(hl)
	inc hl
	or a
	jr nz,.loop
	pop af
	jr .loop1
.label
	ret	
.table	db "space",0,"symbol",0,"m",0,"n",0,"b",0
	db "enter",0,"l",0,"k",0,"j",0,"h",0	
	db "p",0,"o",0,"i",0,"u",0,"y",0
	db "0",0,"9",0,"8",0,"7",0,"6",0
	db "1",0,"2",0,"3",0,"4",0,"5",0
	db "q",0,"w",0,"e",0,"r",0,"t",0
	db "a",0,"s",0,"d",0,"f",0,"g",0
	db "caps",0,"z",0,"x",0,"c",0,"v",0
	endmodule
//----------------------------
@zxscreen:	equ	#4000
@zxscreen_attr:	equ	#5800

	module print
menu:
.loop
	ld a,(hl)
	inc hl
	or a
	ret z
	call char
	jr .loop
after:
	ld (.hl_mem),hl
	pop hl
.loop
	ld a,(hl)
	ld c,a
	inc hl
	and %01111111
	call char
	ld a,c
	cp #80
	jr c,.loop
	push hl
	ld hl,(.hl_mem)
	ret
.hl_mem	dw 0
//-------------
char:
	push de,af
	ld e,a
	ld a,(status)
	cp status_at_x
	jr z,.set_x
	cp status_at_y
	jr z,.set_y
	cp status_color
	jr z,.set_color
	ld a,e
	cp #20
	jr c,.code
	call _8x8_out
	call attr_out
	ld a,(x_pos)
	inc a
	ld (x_pos),a
	cp #20
	jr c,.same
	sub #20
	ld (x_pos),a
	ld a,(y_pos)
	inc a
	ld (y_pos),a
	cp #18
	jr c,.same
	sub #18
	ld (y_pos),a
	jr .same
.set_x
	ld a,e
	ld (x_pos),a
	xor a
	ld (status),a
	jr .same
.set_y
	ld a,e
	ld (y_pos),a
	ld a,status_at_x
	ld (status),a
	jr .same
.set_color
	ld a,e
	ld (color),a
	xor a
	ld (status),a
	jr .same	
.code
	cp at
	jr nz,.code2
	ld a,status_at_y
	ld (status),a
	jr .same
.code2
	cp attr_set
	jr nz,.code3
	ld a,status_color
	ld (status),a
	jr .same
.code3	
.same
	pop af,de
	ret
status_at_y:	equ 1
status_at_x:	equ 2
status_color:	equ 3
at:	equ 1
attr_set:	equ 2
//-------------
_8x8_out:
	push hl,de,af
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,(chrset)
	add hl,de
	ld a,(y_pos)
	ld e,a
	and %00011000
	add high zxscreen
	ld d,a
	ld a,e
	and %00000111
	rrca
	rrca
	rrca	
	ld e,a
	ld a,(x_pos)
	add a,e
	ld e,a
	dup 7	
	ld a,(hl) : ld (de),a : inc hl : inc d
	edup
	ld a,(hl) : ld (de),a
	pop af,de,hl
	ret
attr_out:
	push af,de
	ld a,(y_pos)
	rrca
	rrca
	rrca	
	ld e,a
	and %00000011
	add high zxscreen_attr
	ld d,a
	ld a,(x_pos)
	xor e
	and %00011111
	xor e
	ld e,a
	ld a,(color)
	ld (de),a
	pop de,af
	ret

//------------------
prepare:
	push af
	xor a
	ld (x_pos),a
	ld (y_pos),a
	ld a,%00111
	ld (color),a
	pop af
	ret
status	db 0
x_pos	db 0
y_pos	db 0
color	db 7
	endmodule
//----------------------------
	module screen
; a íîìåð ôîíà
; de àäðåñ â ýêðàíå
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
// âûâîä ôîíà 4 íà 4 áåç àòðèáóòîâ â ñåòêó 8 íà 6 ýêðàíà
// hl - ñïðàéò
// de - àäðåññ â ýêðàíå
// hl' - àòðèáóòû ñïðàéòà
// de' - àäðåñ àòðèáóòîâ
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




