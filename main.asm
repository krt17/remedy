	device zxspectrum128
	org #6000
start:
	di
	halt
	savesna "main.sna",start
next_shot_timeout	byte 0
ammo_hold_current	byte 0



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

