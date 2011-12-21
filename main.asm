	device zxspectrum48
	org #6000
	display "start ",$
start:
	ei
	call init
	call main_menu
	jr $
	ret
char1	byte 0,0,0
init:
	call print.prepare
	call sprite.prepare
	call screen.prepare
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

;	call fade_all
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
scr_out:
	ld de,#4000
	ld ix,map_1
	ld bc,#0008
	ld hl,#1820
	call screen.out_map
	ret
game_cycle
	call key.get
	xor a
	call key.stat
	ld a,#00
	jr nc,.lab1
	ld a,#01
.lab1
	ld (game_stat),a
	halt
	ret

end_scr
	ret
pre_scr
	db print.attr_set,%00000010,print.at,12,10,"Level ",0
	db print.attr_set,%00000010,print.at,14,10,"Lives ",0
game_stat	db 0
level	db 1
map_pos	db 0,0
lives	db 5

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
	display "modules ",$
	include "module.asm"
control_keys:
	ds 8
chrset:	dw font-#100
code_end:
	display "tables ",$
//----------------------------
tables_start:
	align #100
font:
;	incbin "Fonts/news01_f.fnt"
	incbin "Fonts/SET1.fnt"
;	incbin "Fonts/IWAM_fnt.fnt"
mask_table:
	defs #100*2*8
tables_end:
	display "data ",$
//----------------------------
data_start:
map_1
	db 0,0,0,0,0,0,0,0
	db 4,5,6,7,4,8,9,4
	db 1,3,1,1,1,1,1,10
	db 12,13,13,13,14,7,2,10
	db 10,0,0,0,0,14,2,11
	db 7,0,4,8,9,4,2,10

;sprite1:
;	incbin "bin/Spr1.bin"
	include "sprites/luke.spr"
graff
	incbin "bin/riskd1.bin"
graff_attr
	defs 48*16
data_end:
	display "end ",$
	savesna "main.sna",start

	struct weapon
speed	byte 0 ;начальная скорость пули
acur	byte 0 ;точность
zalp	byte 0 ;колличество пуль в выстреле
pause	byte 0 ;время в 1/50 сек до следующего выстрела
cont	byte 0 ;длина очереди ( кооличесво выстрелов без отжатия гашетки)
	ends
;--------------------------
	struct ammo_holder
tm_out	byte 0 ;время отсоединения магазина
tm_in	byte 0 ;время присоединения нового магазина
capac	byte 0 ;вместимость магазина
next_bt	byte 0 ;колличество патронов для автоперезарядки (0) тока магазин целиком 
add_cou byte 0 ;коллво патронов при перезарядке
	ends
	struct ammo
str	byte 0 ;убойная сила 1 частицы пули
str_spd	byte 0 ;изменение убойной силы от скорости
probiv	byte 0 ;пробивная способность 1 частица
chast	byte 0 ;колличество частиц в пуле
rad	byte 0 ;радиус поражения (для разрывных - 0 точечное)
deg	byte 0 ;уменьшение в зависимости от удаления поражения
delay	byte 0 ;задержка поражения (0 при контакте)
	ends

