
	DEVICE ZXSPECTRUM48
	LABELSLIST "c:/zx/US0376/user.l"
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
	org #8000
start:
	call init
	call main_menu
	ret
char1	db 0,0,0
init:
	call print.prepare
	call sprite.prepare
	ret
main_menu:
	call cls
	ld hl,menu1
	call print.menu
.loop1
	ld hl,(.demo_lim)
	ld (.counter),hl
.loop
	halt
	call update_menu
	call key.get
	call key.input
	or a
	jr nz,.press
	ld hl,(.counter)
	dec hl
	ld (.counter),hl
	ld a,h
	or l
	jr nz,.loop
	jp demo
.press
	push af
	ld hl,test
	call print.menu
	pop af
	push af
	call key.print
	call print.menu
	pop af
	cp 21
	jp z,game
	cp 22
	jp z,control_menu
	cp 23
	jp z,about_menu
	cp 24
	ret z
	jr .loop1
.counter	dw 0
.demo_lim	dw 1000
test	db print.attr_set,%00000101,print.at,1,1,0
menu1	db print.attr_set,%00000101,print.at,16,1,"1 start",print.at,18,1,"2 define keys",print.at,20,1,"3 about",print.at,22,1,"4 exit",0
menu2	db print.attr_set,%00000010,print.at,16,1,"up ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,17,1,"down ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,18,1,"left ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,19,1,"right ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,20,1,"fire ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,21,1,"red portal ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,22,1,"blue portal ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,23,1,"pause ",print.attr_set,%00000001,0
	db print.attr_set,%00000010,print.at,14,14,"ok y/n ",print.attr_set,%00000001,0
menu_about:
	db print.attr_set,%00000010,print.at,8,10,"it's my first work"
	db print.at,10,10,"maybe not last"
	db print.at,12,10,"if you like it"
	db print.at,14,10,"mail me",0


select1
	jp main_menu
about_menu
	call fade
	call cls
	ld hl,menu_about
	call print.menu
	ld hl,(.demo_lim)
	ld (.counter),hl
.loop
	halt
	call update_menu
	call key.get
	call key.input
	or a
	jp nz,.press
	ld hl,(.counter)
	dec hl
	ld (.counter),hl
	ld a,h
	or l
	jr nz,.loop
.press
	call fade
	jp main_menu
.counter	dw 0
.demo_lim	dw 50*30
//-------------------------------
update_menu:
	ret
//------------------------
demo:
	ld iy,char1
	ld (iy),0
.loop
	ld a,(iy)
	call sprite.get_spr
	ld d,(iy+1)
	ld e,(iy+2)
	call sprite.out_mask_pixel
	ld a,(iy+2)
	sra a
	sra a
	sra a
	and %00000111
	ld (iy),a
	dec (iy+2)
	ld a,2
	out (#fe),a
//--------------------
	ld hl,#4000
	ld de,#4001
	ld (hl),l
	ld bc,#0800-1
	ldir
	ld a,(iy)	
	cp 8
	jr c,.lab1
	ld (iy),0
.lab1
	ld a,0
	out (#fe),a
	halt
	halt
	ld a,1
	out (#fe),a
	call key.get
	call key.input
	or a
	jr z,.loop
	jp main_menu
.counter	dw 0
.demo_lim	dw 50*30
//---------------------
control_menu
	call fade
	call cls
	ld bc,#800
	ld de,keys
	ld hl,menu2
.loop2
	call print.menu
	push hl
	ld hl,(.demo_lim)
	ld (.counter),hl
.loop
	halt
	call update_menu
	call key.get
	call key.input
	or a
	jp nz,.press
	ld hl,(.counter)
	dec hl
	ld (.counter),hl
	ld a,h
	or l
	jr nz,.loop
	pop hl
	jp main_menu
.press
	cp c
	jr z,.loop
	ld (de),a
	ld c,a
	inc de
	call key.print
	call print.menu
	pop hl
	djnz .loop2
	call print.menu
.lab2
	ld hl,(.demo_lim)
	ld (.counter),hl
.loop1
	halt
	call update_menu
	call key.get
	call key.input
	or a
	jp nz,.press1
	ld hl,(.counter)
	dec hl
	ld (.counter),hl
	ld a,h
	or l
	jr nz,.loop1
.press1
	cp 15
	jr nz,.lab1
	ld de,control_keys
	ld hl,keys
	ld bc,8
	ldir
	call fade
	jp main_menu
.lab1	
	cp 4
	jr nz,.lab2	
	jp control_menu
	
	call fade
	jp main_menu
.counter	dw 0
.demo_lim	dw 50*30
keys:	ds 8
//---------------------------------
game
	call game_init
	call fade_all
	call cls	
	call info_scr
	call fade_all
	call cls	
.loop
	call scr_out
	call game_cycle
	ld a,(game_stat)
	or a
	jr z,.loop

	call fade_all
	call cls	
	call end_scr
	jp main_menu
game_init
	ld a,1
	ld (level),a
	ld a,5
	ld (lives),a
	ret
info_scr
	ld hl,pre_scr
	call print.menu
	ld a,(level)
	add "0"
	call print.char
	call print.menu
	ld a,(lives)
	add "0"
	call print.char
	ld hl,(.demo_lim)
	ld (.counter),hl
.loop
	halt
	call key.get
	call key.input
	or a
	jp nz,.press
	ld hl,(.counter)
	dec hl
	ld (.counter),hl
	ld a,h
	or l
	jr nz,.loop
.press
	ret
.counter	dw 0
.demo_lim	dw 50*3

//-----------------
scr_out
	ld a,(level)
	dec a
	ld b,a
	ld hl,game_screens
	ld c,0
	add hl,bc
	ld de,#4000
	ld ix,#5800
	ld bc,32*24
.loop
	ld a,(hl)
	push hl
	ld l,a
	ld h,high graff		
	dup 8	
	ld a,(hl) : ld (de),a : inc h : inc d
	edup
	ld a,(hl)
	ld (ix),a
	inc ix
	inc e
	jr z,.lab1
	ld a,d
	sub #08
	ld d,a	
.lab1
	pop hl
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,.loop
	ret
game_cycle

	halt
	ret

end_scr
	ret
pre_scr
	db print.attr_set,%00000010,print.at,12,10,"Level ",0
	db print.attr_set,%00000010,print.at,14,10,"Lives ",0
game_stat	db 0
level	db 1
lives	db 5

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
	push bc
	push de
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
	pop de
	down d,e
	pop bc
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
fade_all:
	ld hl,#4000
	ld bc,#1800
	ld de,0
	ld a,5
	jr fade.loop1
fade:
	ld hl,#4800
	ld bc,#1000
	ld de,0
	ld a,8
.loop1
	push af,hl,bc
.loop
	ld a,(de)
	and (hl)
	ld (hl),a
	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jr nz,.loop
	pop bc,hl,af
	dec a
	jr nz,.loop1
	ret
cls:
	ld a,(border)
	out (#fe),a
	ld a,(attr)
	ld hl,#4000+#1800+#300
	ld de,hl
	dec de
	ld bc,#301
	ld (hl),a
	lddr
	xor a
	ld (hl),a
	ld bc,#1800
	lddr
	ret
@attr	db %00000111
@border	db %00000000
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
	        DEFB 0
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
	ld de,font
	ld hl,#3D00
	ld bc,#300
.loop
	ld a,(hl)
	bit 2,l
	jr nz,.down
	rra
	or (hl)
.down
	ld (de),a
	inc hl
	inc de
	dec bc
	ld a,b
	or c
	jr nz,.loop
	xor a
	ld (x_pos),a
	ld (y_pos),a
	ld a,%00111
	ld (color),a
	ret
status	db 0
x_pos	db 0
y_pos	db 0
color	db 7
	endmodule
control_keys:
	ds 8
chrset:	dw font-#100
code_end:
	display $
//----------------------------
tables_start:
	align #100
font:
	defs #300
mask_table:
	defs #100*2*8
tables_end:
	display $
//----------------------------
data_start:
game_screens:
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,1,1,1,0,0,0,1
	db 1,0,0,0,0,0,0,1,0,1,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,1,1,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,1,0,0,0,0,1
	db 1,0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,1,0,1,0,1,0,1,1,0,0,1,1,1,0,1,1,0,0,1,1,1,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
	db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
	align #100
graff:
	db 0,%00100010
	align #100
	db 0,%00100010
	align #100
	db 0,%00100010
	align #100
	db 0,%11111111
	align #100
	db 0,%10001000
	align #100
	db 0,%10001000
	align #100
	db 0,%10001000
	align #100
	db 0,%11111111

	align #100
	db 0,%00000010
screen1:
	incbin "lukeskywalker.scr"
screen2:
	incbin "diceandchars.scr"
sprite1:
	incbin "Spr1.bin"
starwars1000:
; pixel and mask bit pairs
      DEFB #F0, #1F, #FF, #80, #FF, #00
      DEFB #F5, #1F, #7F, #C0, #FF, #00
      DEFB #FB, #0F, #FF, #80, #FF, #00
      DEFB #F5, #1F, #7F, #C0, #FF, #00
      DEFB #F4, #1F, #FF, #C0, #FF, #00
      DEFB #F9, #0F, #3F, #E0, #FF, #00
      DEFB #F6, #1F, #DF, #F0, #FF, #00
      DEFB #FA, #3F, #CF, #F8, #FF, #00
      DEFB #C9, #7F, #D7, #FC, #FF, #00
      DEFB #B1, #FF, #AB, #FE, #FF, #00
      DEFB #BB, #FF, #AB, #FE, #FF, #00
      DEFB #CF, #7F, #67, #FC, #FF, #00
      DEFB #F6, #3F, #C7, #FC, #FF, #00
      DEFB #F8, #0F, #07, #FC, #FF, #00
      DEFB #FE, #07, #07, #FC, #FF, #00
      DEFB #FE, #03, #97, #FC, #FF, #00
      DEFB #FC, #07, #8F, #F8, #FF, #00
      DEFB #FC, #07, #AF, #F8, #FF, #00
      DEFB #FC, #07, #EF, #F8, #FF, #00
      DEFB #FC, #07, #EF, #F8, #FF, #00
      DEFB #F8, #0F, #6F, #FC, #FF, #00
      DEFB #FA, #0F, #33, #FE, #FF, #00
      DEFB #F8, #0F, #29, #FF, #FF, #60
      DEFB #F8, #0F, #4A, #FF, #9F, #F0
      DEFB #F9, #0F, #72, #FF, #5F, #F0
      DEFB #F0, #1F, #FC, #8F, #9F, #F0
      DEFB #F2, #1F, #FF, #83, #4F, #F8
      DEFB #F5, #1F, #FF, #00, #8F, #F8
      DEFB #F1, #3F, #FF, #00, #AF, #F8
      DEFB #C4, #7F, #FF, #80, #DF, #70
      DEFB #AA, #FF, #FF, #80, #FF, #20
      DEFB #81, #FF, #FF, #00, #FF, #00
; Name    : starwars1
; Frame   : 1
; Palette : Monochrome
; Masked  : Yes
; RowOrder: Classic
; Size: 3 x 32 - (bytes x pixels)
starwars1001:

; pixel and mask bit pairs
      DEFB #FC, #07, #3F, #E0, #FF, #00
      DEFB #FD, #07, #5F, #F0, #FF, #00
      DEFB #FE, #03, #FF, #E0, #FF, #00
      DEFB #FD, #07, #5F, #F0, #FF, #00
      DEFB #FD, #07, #3F, #F0, #FF, #00
      DEFB #FE, #03, #4F, #F8, #FF, #00
      DEFB #FD, #07, #B7, #FC, #FF, #00
      DEFB #FE, #03, #B7, #FC, #FF, #00
      DEFB #FF, #01, #33, #FE, #FF, #00
      DEFB #FF, #07, #73, #FE, #FF, #00
      DEFB #F9, #0F, #6B, #FE, #FF, #00
      DEFB #F7, #1F, #A9, #FF, #FF, #00
      DEFB #F3, #1F, #A1, #FF, #FF, #00
      DEFB #FC, #0F, #01, #FF, #FF, #00
      DEFB #FF, #03, #81, #FF, #FF, #00
      DEFB #FF, #00, #A5, #FF, #FF, #00
      DEFB #FF, #01, #23, #FE, #FF, #00
      DEFB #FF, #01, #2B, #FE, #FF, #00
      DEFB #FF, #01, #7B, #FE, #FF, #00
      DEFB #FE, #03, #73, #FE, #FF, #00
      DEFB #FE, #03, #77, #FC, #FF, #00
      DEFB #FE, #03, #E7, #FC, #FF, #00
      DEFB #FE, #03, #4B, #FE, #FF, #00
      DEFB #FE, #03, #15, #FF, #FF, #80
      DEFB #FE, #03, #0A, #FF, #7F, #C0
      DEFB #FE, #03, #51, #FF, #3F, #E0
      DEFB #FE, #03, #BC, #EF, #BF, #E0
      DEFB #FE, #07, #3E, #E3, #3F, #E0
      DEFB #F8, #0F, #9E, #F3, #BF, #E0
      DEFB #F5, #1F, #5E, #F3, #7F, #C0
      DEFB #F0, #1F, #3F, #E1, #FF, #80
      DEFB #FF, #0F, #FF, #C0, #FF, #00

starwars1002:
      DEFB #F8, #0F, #7F, #C0, #FF, #00
      DEFB #FA, #0F, #BF, #E0, #FF, #00
      DEFB #FD, #07, #FF, #C0, #FF, #00
      DEFB #FA, #0F, #BF, #E0, #FF, #00
      DEFB #FA, #0F, #3F, #E0, #FF, #00
      DEFB #FC, #07, #DF, #F0, #FF, #00
      DEFB #FB, #0F, #8F, #F8, #FF, #00
      DEFB #FD, #07, #AF, #F8, #FF, #00
      DEFB #FD, #07, #77, #FC, #FF, #00
      DEFB #FD, #0F, #3B, #FE, #FF, #00
      DEFB #F1, #1F, #CB, #FE, #FF, #00
      DEFB #EC, #3F, #3B, #FE, #FF, #00
      DEFB #E9, #3F, #E7, #FC, #FF, #00
      DEFB #F5, #1F, #83, #FE, #FF, #00
      DEFB #FE, #0B, #03, #FE, #FF, #00
      DEFB #FF, #01, #4B, #FE, #FF, #00
      DEFB #FE, #03, #9B, #FE, #FF, #00
      DEFB #FD, #07, #B3, #FE, #FF, #00
      DEFB #FB, #0F, #E7, #FC, #FF, #00
      DEFB #F7, #1F, #83, #FE, #FF, #00
      DEFB #F6, #1F, #03, #FE, #FF, #00
      DEFB #F9, #0F, #8B, #FE, #FF, #00
      DEFB #FD, #07, #65, #FF, #FF, #00
      DEFB #FE, #03, #41, #FF, #FF, #00
      DEFB #FF, #01, #91, #FF, #FF, #00
      DEFB #FF, #00, #CA, #7F, #FF, #80
      DEFB #FF, #00, #D0, #7F, #FF, #80
      DEFB #FF, #00, #A2, #FF, #7F, #C0
      DEFB #FF, #00, #89, #FF, #BF, #E0
      DEFB #FF, #00, #F0, #7F, #BF, #E0
      DEFB #FF, #00, #F0, #1F, #3F, #E0
      DEFB #FF, #00, #FF, #0F, #FF, #C0
; Name    : starwars1
; Frame   : 3
; Palette : Monochrome
; Masked  : Yes
; RowOrder: Classic
; Size: 3 x 32 - (bytes x pixels)
starwars1003:
; pixel and mask bit pairs
      DEFB #FF, #1E, #FF, #00, #FF, #00
      DEFB #E1, #3F, #FF, #00, #FF, #00
      DEFB #EA, #3F, #FF, #80, #FF, #00
      DEFB #F7, #1F, #FF, #00, #FF, #00
      DEFB #EA, #3F, #FF, #80, #FF, #00
      DEFB #E8, #3F, #FF, #80, #FF, #00
      DEFB #F3, #1F, #7F, #C0, #FF, #00
      DEFB #EE, #3F, #3F, #E0, #FF, #00
      DEFB #F6, #1F, #9F, #F0, #FF, #00
      DEFB #F5, #3F, #DF, #F0, #FF, #00
      DEFB #C4, #7F, #EF, #F8, #FF, #00
      DEFB #B6, #FF, #6F, #F8, #FF, #00
      DEFB #BA, #FF, #CF, #F8, #FF, #00
      DEFB #C1, #7F, #9F, #F0, #FF, #00
      DEFB #F7, #3F, #0F, #F8, #FF, #00
      DEFB #F6, #1F, #0F, #F8, #FF, #00
      DEFB #F8, #0F, #2F, #F8, #FF, #00
      DEFB #FA, #0F, #6F, #F8, #FF, #00
      DEFB #F7, #1F, #4F, #F8, #FF, #00
      DEFB #F7, #1F, #87, #FC, #FF, #00
      DEFB #EE, #3F, #17, #FE, #FF, #00
      DEFB #E9, #3F, #89, #FF, #FF, #40
      DEFB #E2, #3F, #C4, #FF, #BF, #E0
      DEFB #F4, #1F, #E1, #BF, #5F, #F0
      DEFB #F9, #0F, #78, #DF, #2F, #F8
      DEFB #FA, #0F, #7E, #C7, #0F, #F8
      DEFB #FD, #07, #BF, #E1, #97, #FC
      DEFB #FC, #07, #3F, #E0, #C7, #7C
      DEFB #FB, #0F, #7F, #C0, #EF, #38
      DEFB #FA, #0F, #FF, #80, #FF, #10
      DEFB #F9, #0F, #FF, #00, #FF, #00
      DEFB #FF, #06, #FF, #00, #FF, #00
; Name    : starwars1
; Frame   : 4
; Palette : Monochrome
; Masked  : Yes
; RowOrder: Classic
; Size: 3 x 32 - (bytes x pixels)
starwars1004:
; pixel and mask bit pairs
      DEFB #FC, #07, #3F, #E0, #FF, #00
      DEFB #FD, #07, #5F, #F0, #FF, #00
      DEFB #FE, #03, #FF, #A0, #FF, #00
      DEFB #FD, #07, #5F, #F0, #FF, #00
      DEFB #FD, #07, #1F, #F0, #FF, #00
      DEFB #FE, #03, #6F, #F8, #FF, #00
      DEFB #FD, #07, #C7, #FC, #FF, #00
      DEFB #FE, #0F, #D3, #FE, #FF, #00
      DEFB #F2, #1F, #BD, #FF, #FF, #00
      DEFB #EC, #3F, #9E, #FF, #FF, #80
      DEFB #EC, #3F, #E6, #FF, #FF, #80
      DEFB #F2, #1F, #ED, #FF, #FF, #00
      DEFB #FC, #0F, #1B, #FE, #FF, #00
      DEFB #FF, #03, #73, #FE, #FF, #00
      DEFB #FF, #01, #61, #FF, #FF, #00
      DEFB #FF, #00, #81, #FF, #FF, #00
      DEFB #FF, #01, #0B, #FE, #FF, #00
      DEFB #FF, #01, #5B, #FE, #FF, #00
      DEFB #FE, #03, #73, #FE, #FF, #00
      DEFB #FE, #03, #F3, #FE, #FF, #00
      DEFB #FC, #07, #E3, #FE, #FF, #00
      DEFB #FC, #07, #C5, #FF, #FF, #60
      DEFB #FC, #07, #82, #FF, #9F, #F0
      DEFB #FD, #07, #20, #FF, #5F, #F0
      DEFB #FA, #0F, #3C, #FF, #9F, #F0
      DEFB #F9, #0F, #7F, #C3, #0F, #F8
      DEFB #F2, #1F, #FF, #80, #8F, #F8
      DEFB #F5, #3F, #FF, #00, #AF, #F8
      DEFB #C9, #7F, #FF, #00, #DF, #70
      DEFB #B5, #FF, #FF, #00, #FF, #20
      DEFB #83, #FE, #FF, #00, #FF, #00
      DEFB #FF, #7C, #FF, #00, #FF, #00
; Name    : starwars1
; Frame   : 5
; Palette : Monochrome
; Masked  : Yes
; RowOrder: Classic
; Size: 3 x 32 - (bytes x pixels)
starwars1005:
; pixel and mask bit pairs
      DEFB #F8, #0F, #7F, #C0, #FF, #00
      DEFB #FA, #0F, #BF, #E0, #FF, #00
      DEFB #FD, #07, #FF, #C0, #FF, #00
      DEFB #FA, #0F, #BF, #E0, #FF, #00
      DEFB #FA, #0F, #3F, #E0, #FF, #00
      DEFB #FC, #07, #DF, #F0, #FF, #00
      DEFB #FB, #0F, #8F, #F8, #FF, #00
      DEFB #FD, #07, #A7, #FC, #FF, #00
      DEFB #FD, #1F, #7B, #FE, #FF, #00
      DEFB #E1, #3F, #3D, #FF, #FF, #00
      DEFB #D9, #7F, #CD, #FF, #FF, #00
      DEFB #DD, #7F, #DB, #FE, #FF, #00
      DEFB #E0, #3F, #37, #FC, #FF, #00
      DEFB #FE, #1F, #E7, #FC, #FF, #00
      DEFB #FE, #03, #C3, #FE, #FF, #00
      DEFB #FF, #01, #03, #FE, #FF, #00
      DEFB #FE, #03, #17, #FC, #FF, #00
      DEFB #FE, #03, #97, #FC, #FF, #00
      DEFB #FD, #07, #67, #FC, #FF, #00
      DEFB #FD, #07, #E7, #FC, #FF, #00
      DEFB #FB, #0F, #CB, #FE, #FF, #00
      DEFB #F7, #1F, #05, #FF, #FF, #80
      DEFB #F2, #1F, #02, #FF, #7F, #C0
      DEFB #F4, #1F, #E0, #FF, #BF, #E0
      DEFB #F2, #1F, #FC, #9F, #5F, #F0
      DEFB #F4, #1F, #FE, #83, #1F, #F0
      DEFB #F2, #1F, #FF, #81, #2F, #F8
      DEFB #F8, #0F, #FF, #80, #8F, #F8
      DEFB #F2, #1F, #7F, #C0, #DF, #70
      DEFB #E4, #3F, #7F, #C0, #FF, #20
      DEFB #E8, #3F, #FF, #80, #FF, #00
      DEFB #E1, #3F, #FF, #00, #FF, #00
; Name    : starwars1
; Frame   : 6
; Palette : Monochrome
; Masked  : Yes
; RowOrder: Classic
; Size: 3 x 32 - (bytes x pixels)
starwars1006:
; pixel and mask bit pairs
      DEFB #FE, #03, #1F, #F0, #FF, #00
      DEFB #FE, #03, #AF, #F8, #FF, #00
      DEFB #FF, #01, #7F, #F0, #FF, #00
      DEFB #FE, #03, #AF, #F8, #FF, #00
      DEFB #FE, #03, #8F, #F8, #FF, #00
      DEFB #FF, #01, #37, #FC, #FF, #00
      DEFB #FE, #03, #E3, #FE, #FF, #00
      DEFB #FF, #01, #6B, #FE, #FF, #00
      DEFB #FF, #01, #5D, #FF, #FF, #00
      DEFB #FF, #03, #4D, #FF, #FF, #00
      DEFB #FC, #07, #66, #FF, #FF, #80
      DEFB #FB, #0F, #6E, #FF, #FF, #80
      DEFB #FA, #0F, #1C, #FF, #FF, #80
      DEFB #FD, #07, #70, #FF, #FF, #80
      DEFB #FF, #03, #60, #FF, #FF, #80
      DEFB #FF, #00, #92, #FF, #FF, #80
      DEFB #FF, #00, #96, #FF, #FF, #80
      DEFB #FF, #01, #54, #FF, #FF, #80
      DEFB #FE, #03, #9D, #FF, #FF, #00
      DEFB #FE, #03, #1D, #FF, #FF, #00
      DEFB #FC, #07, #3D, #FF, #FF, #00
      DEFB #FD, #07, #3B, #FE, #FF, #00
      DEFB #FE, #03, #3B, #FE, #FF, #00
      DEFB #FF, #01, #11, #FF, #FF, #00
      DEFB #FF, #01, #54, #FF, #FF, #80
      DEFB #FF, #00, #A0, #FF, #FF, #80
      DEFB #FF, #00, #B3, #FF, #FF, #00
      DEFB #FF, #00, #A7, #FC, #FF, #00
      DEFB #FF, #00, #A7, #FC, #FF, #00
      DEFB #FF, #01, #57, #FC, #FF, #00
      DEFB #FE, #03, #A7, #FC, #FF, #00
      DEFB #FF, #01, #1F, #F8, #FF, #00
; Name    : starwars1
; Frame   : 7
; Palette : Monochrome
; Masked  : Yes
; RowOrder: Classic
; Size: 3 x 32 - (bytes x pixels)
starwars1007:
; pixel and mask bit pairs
      DEFB #FE, #03, #1F, #F0, #FF, #00
      DEFB #FE, #03, #AF, #F8, #FF, #00
      DEFB #FF, #01, #7F, #F0, #FF, #00
      DEFB #FE, #03, #AF, #F8, #FF, #00
      DEFB #FE, #03, #9F, #F0, #FF, #00
      DEFB #FE, #03, #6F, #F8, #FF, #00
      DEFB #FD, #07, #CF, #F8, #FF, #00
      DEFB #FE, #03, #B7, #FC, #FF, #00
      DEFB #FE, #07, #B3, #FE, #FF, #00
      DEFB #F8, #0F, #B5, #FF, #FF, #00
      DEFB #F6, #1F, #75, #FF, #FF, #00
      DEFB #F7, #1F, #65, #FF, #FF, #00
      DEFB #F9, #0F, #E3, #FE, #FF, #00
      DEFB #FE, #07, #C3, #FE, #FF, #00
      DEFB #FF, #01, #05, #FF, #FF, #00
      DEFB #FF, #00, #85, #FF, #FF, #00
      DEFB #FF, #01, #31, #FF, #FF, #00
      DEFB #FE, #03, #33, #FE, #FF, #00
      DEFB #FE, #03, #3B, #FE, #FF, #00
      DEFB #FC, #07, #9B, #FE, #FF, #00
      DEFB #FD, #07, #1B, #FE, #FF, #00
      DEFB #FC, #07, #9B, #FE, #FF, #00
      DEFB #FE, #03, #19, #FF, #FF, #00
      DEFB #FF, #01, #85, #FF, #FF, #00
      DEFB #FF, #00, #EA, #7F, #FF, #80
      DEFB #FF, #00, #F4, #1F, #FF, #80
      DEFB #FF, #00, #E3, #3F, #7F, #C0
      DEFB #FF, #00, #E2, #3F, #7F, #C0
      DEFB #FF, #00, #FD, #1F, #3F, #E0
      DEFB #FF, #00, #FA, #0F, #BF, #E0
      DEFB #FF, #00, #F5, #1F, #3F, #E0
      DEFB #FF, #00, #F8, #0F, #FF, #C0
data_end:
	display $
//----------------------------
	savesna "main.sna",start



