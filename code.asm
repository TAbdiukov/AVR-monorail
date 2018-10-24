;FINAL

.include "m2560def.inc"
.def temp =r21
.def row =r17
.def col =r18
.def mask =r19
.def temp2 =r20
.def count_letter =r22          ;multiple times 
.def push_flag=r23				;whether any button has pushed
.def letter_num = r24			;maximum number
.def finish_input_flag =r25		;0 is pushed, input finish
.def temp_count_for_question =r2
.def keypad_version = r3		;char - 0 num - 1
//.def input10 = r4 unused
 

.equ PORTLDIR = 0xF0
.equ INITCOLMASK = 0xEF
.equ INITROWMASK = 0x01
.equ ROWMASK = 0x0F
.equ second_line = 0b10101000

////
	;lcd operation
////
.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4


.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

.macro do_lcd_command
	ldi r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro
.macro do_lcd_command_imme
	mov r16, @0
	rcall lcd_command
	rcall lcd_wait
.endmacro
.macro do_lcd_data
	ldi r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro
.macro do_lcd_data_imme
	mov r16, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro stop_time_head
	do_lcd_data 'T'
	do_lcd_data 'i'
	do_lcd_data 'm'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_data_imme @0
	do_lcd_data ' '
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data ' '
	do_lcd_data '1'
	do_lcd_data ':'
	do_lcd_command 0b11000000
.endmacro
.macro train_stop
	do_lcd_data 'T'
	do_lcd_data 'r'
	do_lcd_data 'a'
	do_lcd_data 'i'
	do_lcd_data 'n'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'p'
	do_lcd_data ' '
	do_lcd_data 'f'
	do_lcd_data 'o'
	do_lcd_data 'r'
	do_lcd_data ':'
	do_lcd_data ' '
	do_lcd_command 0b11000000
.endmacro
//
.macro station_name
	do_lcd_data 'T'
	do_lcd_data 'y'
	do_lcd_data 'p'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'p'
	do_lcd_data ' '
	do_lcd_data_imme @0
	do_lcd_data ' '
	do_lcd_data 'n'
	do_lcd_data 'a'
	do_lcd_data 'm'
	do_lcd_data 'e'
	do_lcd_data ':'
.endmacro
.macro stop_maximum
	do_lcd_data 'M'
	do_lcd_data 'a'
	do_lcd_data 'x'
	do_lcd_data ' '
	do_lcd_data 's'
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data 'p'
	do_lcd_data ' '
	do_lcd_data 'n'
	do_lcd_data 'u'
	do_lcd_data 'm'
	do_lcd_data ':'
	do_lcd_command 0b11000000
.endmacro
.macro stop_time
	do_lcd_data 'T'
	do_lcd_data 'i'
	do_lcd_data 'm'
	do_lcd_data 'e'
	do_lcd_data ' '
	do_lcd_data_imme @0
	inc @0
	do_lcd_data ' '
	do_lcd_data 't'
	do_lcd_data 'o'
	do_lcd_data ' '
	do_lcd_data_imme @0
	do_lcd_data ':'
	do_lcd_command 0b11000000
.endmacro
.macro finish_info
	do_lcd_data 'A'
	do_lcd_data 'l'
	do_lcd_data 'l'
	do_lcd_data ' '
	do_lcd_data 'd'
	do_lcd_data 'o'
	do_lcd_data 'n'
	do_lcd_data 'e'
	do_lcd_command 0b11000000
	do_lcd_data 'W'
	do_lcd_data 'a'
	do_lcd_data 'i'
	do_lcd_data 't'
	do_lcd_data ' '
	do_lcd_data 'f'
	do_lcd_data 'o'
	do_lcd_data 'r'
	do_lcd_data ' '
	do_lcd_data '5'
	do_lcd_data 's'
.endmacro
.macro wrong
	do_lcd_data 'E'
	do_lcd_data 'r'
	do_lcd_data 'r'
	do_lcd_data '!'
	do_lcd_data ' '
.endmacro
.macro clear
ldi YL, low(@0)
ldi YH, high(@0)
clr temp
st Y+,temp
st Y,temp
.endmacro
.macro clearonebyte
ldi YL, low(@0)
ldi YH, high(@0)
clr temp
st Y,temp
.endmacro
.macro timeten
	lsl @0
	mov r16, @0
	lsl @0
	lsl @0
	add @0, r16
.endmacro

.dseg
//////////////////////////////
;++++++   variable   +++++++++
//////////////////////////////
SecondCounter: .byte 2
TempCounter: .byte 2
Status: .byte 1
Position: .byte 1
count_question:.byte 1
Maximum: .byte 1
TempNumInfo: .byte 1
current_name_pointer: .byte 2
station_stop_time: .byte 1
current_time_pointer: .byte 2
pb_flag: .byte 1
led: .byte 1
stop_flag: .byte 1
hash_flag: .byte 1
////////////////////////////
	;station storage
////////////////////////////
	station1: .byte 10
	station2: .byte 10
	station3: .byte 10
	station4: .byte 10
	station5: .byte 10
	station6: .byte 10
	station7: .byte 10
	station8: .byte 10
	station9: .byte 10
	station10: .byte 10

time_data: .byte 10

.cseg
.org 0x0
jmp RESET

.org INT0addr    ; INT0addr is the address of EXT_INT0  
jmp EXT_INT0 
.org INT1addr    ; INT1addr is the address of EXT_INT1 
jmp EXT_INT1
jmp DEFAULT
.org OVF0addr
jmp Timer0OVF 
 
DEFAULT:reti

RESET:
ldi temp, low(RAMEND)
out SPL, temp
ldi temp, high(RAMEND)
out SPH, temp
ldi temp, PORTLDIR ; columns are outputs, rows are inputs
STS DDRL, temp     ; cannot use out
ser temp
out DDRC, temp ; Make PORTC all outputs
clr temp
out PORTC, temp
ser temp 
out DDRF, temp
out DDRA, temp
clr temp
out PORTF, temp
out PORTA, temp
;clr station
ldi yl,low(station1)
ldi yh,high(station2)
ldi temp,99
clr r16
clear_stations:
	cpi temp,0
	breq time_setup
	st y+,r16
	dec temp
	rjmp clear_stations
;clear time
time_setup:
;timer0 setup
	clear TempCounter
	clear SecondCounter
	ldi temp,0b00000000
	out TCCR0A,temp     ;set mode 000- normal mode
	ldi temp,0b00000010
	out TCCR0B,temp     ;set prescaler - 8
	ldi temp, 1<<TOIE0  ;time overflow from 3 kinds of overflow cmpA,cmpB,time overflow
	sts TIMSK0,temp     ;apply to mask

	
	 
	sei					;every timer overflow trigger an interrupt, when the times of interrupt is equal
						;to 7812(that is the times should happen in a second) add 1s
	////////////////////////////
	//set lcd start position////
	////////////////////////////
	ldi r24, second_line
	sts Position, r24
	////////////////////////////
	//set letter counter////////
	////////////////////////////
	ldi push_flag,1
	clr letter_num
	ldi count_letter, 0b00000000
	clearonebyte count_question
	

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink
	;clr line
	//

	jmp main
EXT_INT0: ;(PB0)
	 
	push temp 
	in temp, SREG
	push temp 
	ldi temp,1
	sts pb_flag,temp
	pop temp 
	out SREG, temp 
	pop temp 
	reti
	 
EXT_INT1: ;(PB1)
	 
	push temp 
	in temp, SREG
	push temp 
	ldi temp,1
	sts pb_flag,temp
	pop temp 
	out SREG, temp 
	pop temp 
	reti


/*
EXT_INT1: ; PB1
	rcall sleep_350ms
	push temp 
	in temp, SREG
	push temp
 
	lds temp,hash_flag
	cpi temp,1
	breq close_motor
	cpi temp,0
	breq open_motor
		close_motor:
		ldi temp,1
		sts stop_flag,temp
		ldi temp,0
		sts hash_flag,temp
		sei
		close_motor_loop:
			lds temp2,hash_flag
			cpi temp2,1
			breq hash_end
			rjmp close_motor_loop
		open_motor:
			ldi temp2,1
			sts hash_flag,temp2
			ldi temp, 0b00111100
			out DDRE, temp   
			ldi temp, 0xff   
			sts OCR3BL,temp
			clr temp 
			sts OCR3BH, temp
			ldi temp, (1 << CS30)               ; CS31=1: no prescaling 
			sts TCCR3B, temp    
			ldi temp, (1<< WGM30)|(1<<COM3B1)   ; WGM30=1: phase correct PWM, 8 bits  
											    ; COM3B1=1: make OC3B override the normal port functionality of the I/O pin PL3 
			sts TCCR3A, temp
	*/



	hash_end:
	cli
	clearonebyte stop_flag
	pop temp 
	out SREG, temp 
	pop temp 
	reti
	
	

Timer0OVF:
	in temp, SREG
	push temp
	push YH
	push YL
	push r25
	push r24
	lds r24,TempCounter
	lds r25,TempCounter+1
	adiw r25:r24,1
	cpi r24,low(7812)
	ldi temp,high(7812)
	cpc r25,temp
	brne NotSecond

	clear TempCounter
	lds r24,SecondCounter
	lds r25,SecondCounter+1
	adiw r25:r24,1
	sts SecondCounter, r24
	sts SecondCounter+1,r25
	lds r24, Status ;?
	cpi r24,0
	breq input_wait
	rcall partc_timer
	rjmp EndIF
NotSecond:
	sts TempCounter, r24
	sts TempCounter+1,r25
	lds r24, Status ;?
	cpi r24,0
	breq EndIF
	rcall partc_timer
EndIF:
	pop r24
	pop r25
	pop YL
	pop YH
	pop temp
	out SREG,temp
	reti
input_wait:
	lds r24,SecondCounter
	cpi r24, 1
	brsh restart
go_EndIF:
	rjmp EndIF
restart:
	cli
	clear TempCounter
	clear SecondCounter
	sbrc push_flag,0                ;if 0, then there is a char pressed 
	rjmp noAction
	lds r24,Position
	inc r24
	sts Position,r24
	ldi push_flag,1
	inc letter_num					
	lds yl,current_name_pointer
	lds yh,current_name_pointer+1
	st y+,row
	sts current_name_pointer,yl
	sts current_name_pointer+1,yh
	do_lcd_data_imme row	
noAction:
	sei
	ldi count_letter,0b00000000
	rjmp go_EndIF
///////////////////////////
//do motor and led at here
partc_timer:
	
	push temp
	push temp2
	push r24
	push r25
	//dont use r17
	lds temp,stop_flag
	cpi temp,1
	breq rail_stop
	partc_timer_end:
		pop r25
		pop r24
		pop temp2
		pop temp
		ret
	rail_stop:
	//test
		lds r24,TempCounter
		lds r25,TempCounter+1
		cpi r24,low(2604)
		ldi temp,high(2604)
		cpc r25,temp
		brsh change_led
		do_motor:
			ldi temp, 0b00111100
			out DDRE, temp  ; set PL3 (OC5A) as output. 
			ldi temp, 0x00   ; this value and the operation mode determine the PWM duty cycle sts OCR5AL, temp 
			sts OCR3BL,temp
			clr temp 
			sts OCR3BH, temp
			ldi temp, (1 << CS30) 
			sts TCCR3B, temp    
			ldi temp, (1<< WGM30)|(1<<COM3B1)  
			sts TCCR3A, temp
			rjmp partc_timer_end //havenot done this part let it stop
		change_led:
			lds temp2,led
			cpi temp2,0
			breq light
			cpi temp2,1
			breq dark
			light:
				ldi temp2,0b11111111
				out PORTC,temp2
				ldi temp2,1
				sts led,temp2
				rjmp change_end
			dark:
				ldi temp2,0b00000000
				out PORTC,temp2
				ldi temp2,0
				sts led,temp2
				rjmp change_end
			change_end:
				clear TempCounter
				rjmp do_motor
main:	
	///////////////////////////////////
		; part a
		;get and store information
	///////////////////////////////////
	stop_maximum
	ldi temp,1        ;1 is for num pad
	mov keypad_version , temp
	rcall keypad_part
	lds temp, TempNumInfo
	cpi temp, 11
	brsh wrong_info_max
	sts Maximum,temp
	rjmp ask_stop_name
wrong_info_max:
	do_lcd_command 0b00000001
	wrong						
	clearonebyte TempNumInfo
	rjmp main

ask_stop_name:
	do_lcd_command 0b00000001 ; clear display
	clr keypad_version
	lds temp, count_question ;start from 0
	lds r16, Maximum
	cp temp, r16
	brsh stop_time_start
	ldi r16, 0b00110000
	mov temp_count_for_question, temp	
	add r16, temp
	mov r1, r16
	inc r1
	rcall store_name ;store name store
	station_name r1
	rcall keypad_part
	mov r1, temp_count_for_question
	inc r1
	sts count_question, r1
	rjmp ask_stop_name

stop_time_start:
	clearonebyte count_question
	clearonebyte tempNumInfo
ask_stop_time:
	lds r1, count_question
	mov temp_count_for_question, r1
	lds r16, Maximum
	dec r16
	cp r1, r16
	brsh back_to_head                       
	inc r1
	ldi r16, 0b00110000
	add r1,r16
	stop_time r1
	ldi temp,1          ;1 is for num pad
	mov keypad_version , temp
	rcall keypad_part
	rjmp back_to_time   ;check overflow for station time
back_to_head:                                        
	lds r1, count_question
	mov r16, r1
	cpi r16, 1
	breq ask_for_waiting_time
	ldi r16, 0b00110000
	add r1,r16
	inc r1
	stop_time_head r1
	rcall keypad_part
	//
	rjmp time_head
	
ask_for_waiting_time:								;new block
	train_stop
	rcall keypad_part
	lds temp, TempNumInfo
	cpi temp, 6
	brsh wrong_stop
	cpi temp, 2
	brlo wrong_stop
	sts station_stop_time,temp
	rjmp finish
wrong_stop:
	do_lcd_command 0b00000001
	wrong						
	do_lcd_command 0b11000000
	clearonebyte TempNumInfo
	rjmp ask_for_waiting_time
	//wrong handler
finish:
	do_lcd_command 0b00000001
	finish_info
	rcall sleep_350ms
	rjmp partc
back_to_time:
	lds temp, TempNumInfo
	cpi temp, 11
	brsh wrong_station_info
	rcall store_time
	lds temp,tempNumInfo
	lds xl,current_time_pointer
	lds xh,current_time_pointer+1
	st x+,temp
	inc temp_count_for_question
	mov r1, temp_count_for_question
	sts count_question,r1
	clearonebyte tempNumInfo
	do_lcd_command 0b00000001
	rjmp ask_stop_time
time_head:
	lds temp, TempNumInfo
	cpi temp, 11
	brsh wrong_station_info
	rcall store_time
	lds temp,tempNumInfo
	lds xl,current_time_pointer
	lds xh,current_time_pointer+1
	st x+,temp
	inc temp_count_for_question
	mov r1, temp_count_for_question
	sts count_question,r1
	clearonebyte tempNumInfo
	//
	do_lcd_command 0b00000001
	rjmp ask_for_waiting_time
wrong_station_info:
	do_lcd_command 0b00000001
	wrong						
	
	clearonebyte TempNumInfo
	rjmp ask_stop_time
     //////////////////////////////////
		;part c
		;show stored information
	///////////////////////////////////// 

//
partc:
	ldi temp, (2 << ISC10) | (2 << ISC00) ;enable external interrupt
	sts EICRA, temp 
	in temp, EIMSK 
	ori temp, (1<<INT0) | (1<<INT1) 
	out EIMSK, temp
	sei
	ldi temp, 0b00111100
	out DDRE, temp   
	ldi temp, 0x00   
	sts OCR3BL,temp
	clr temp 
	sts OCR3BH, temp
	ldi temp, (1 << CS30) 
	sts TCCR3B, temp    
	ldi temp, (1<< WGM30)|(1<<COM3B1)    
	sts TCCR3A, temp

	rcall sleep_1000ms
	rcall sleep_1000ms
	rcall sleep_1000ms
	rcall sleep_1000ms
	rcall sleep_1000ms
	do_lcd_command 0b00000001
	clearonebyte count_question
	ldi temp, 1
	sts Status, temp
	ldi temp,2
	mov keypad_version, temp
	ldi temp, 1
	sts hash_flag, temp
	clr temp
	sts pb_flag,temp
	clearonebyte led
	clearonebyte stop_flag
	print_data:
		ldi temp, 0b00111100
		out DDRE, temp  
		ldi temp, 0xff    
		sts OCR3BL,temp
		clr temp 
		sts OCR3BH, temp
		ldi temp, (1 << CS30)  
		sts TCCR3B, temp    
		ldi temp, (1<< WGM30)|(1<<COM3B1)   
		sts TCCR3A, temp
		rcall store_time
		///////////////////////////
		// check if overflow
		///////////////////////////
		lds r18,Maximum
		lds r19,count_question
		sub r18,r19
		cpi r18,0
		breq time_back_to_1
		rjmp print_go_on

		time_back_to_1:
			ldi yl,low(time_data)
			ldi yh,high(time_data)
			sts current_time_pointer,yl
			sts current_time_pointer+1,yh
		print_go_on:
			lds yl,current_time_pointer
			lds yh,current_time_pointer+1
			ld r18,y
		//	
		do_lcd_command 0b00000001
		do_lcd_data 'n'
		do_lcd_data 'e'
		do_lcd_data 'x'
		do_lcd_data 't'
		do_lcd_data ':'
		do_lcd_command 0b11000000
		rcall sleep_5ms
		rcall print_all
		on_way_loop:			
			cpi r18,0
			breq check_stop
			do_lcd_data '>'
			rcall sleep_1000ms
			dec r18	
			rjmp on_way_loop
		check_stop:
			
			lds temp,pb_flag
			cpi temp,1	
			breq Station_stop
			rjmp print_data_end
		Station_stop:
			ldi r17,1
			sts stop_flag,r17
			clear TempCounter
			do_lcd_command 0b00000001
			do_lcd_data 'w'
			do_lcd_data 'a'
			do_lcd_data 'i'
			do_lcd_data 't'
			do_lcd_data ' '
			do_lcd_data 'f'
			do_lcd_data 'o'
			do_lcd_data 'r'
			do_lcd_data ':'
			do_lcd_data ' '
			do_lcd_command 0b11000000
			lds r17,count_question
			dec r17
			sts count_question,r17
			rcall print_all
			inc r17
			sts count_question,r17
			lds r17,station_stop_time
			;out PORTC,r17
			Station_stop_loop:
				do_lcd_data '>'
				rcall sleep_1000ms
				cpi r17,1
				breq print_data_end
				dec r17
				rjmp Station_stop_loop
		print_data_end:
			clearonebyte stop_flag
			clearonebyte pb_flag
			ldi temp2,0b00000000
			out PORTC,temp2
			rjmp print_data


print_all:
	//cli
	push temp
	push temp2
	push r16
	push yl
	push yh
	ldi yl,low(station1)
	ldi yh,high(station1)
	sts current_name_pointer, yl
	sts current_name_pointer+1,yh
	show_station:
		lds temp2,Maximum
		lds temp,count_question
		sub temp2,temp
		cpi temp2,0
		breq back_to_one
		///////////////////////
		//get next station
		lds r16,count_question
		lds temp,count_question
		lds temp2,Maximum
		inc temp
		sub temp2,temp
		cpi temp2,0
		breq next_first
		sts count_question,temp
		rjmp print_first
		//
		back_to_one:
			clr temp
			sts count_question,temp
			ldi yl,low(station1)
			ldi yh,high(station1)
			rjmp show_station
		next_first:
			clr temp
			sts count_question,temp
		//
		print_first:
			rcall store_name
			sts count_question,r16
			lds yl,current_name_pointer
			lds yh,current_name_pointer+1
		
			ldi temp2,9
		//print next name
		print_name:	
			ld temp,y+
			cpi temp,0
			breq show_station_end
			cpi temp2,0
			breq show_station_end
			do_lcd_data_imme temp
			dec temp2
			rjmp print_name
		show_station_end:
			do_lcd_data ' '
			lds temp,count_question
			inc temp
			sts count_question,temp
			ldi temp2,0b00000000
			out PORTC,temp2
			pop yh
			pop yl
			pop r16
			pop temp2
			pop temp
			//sei
			ret 



loop:
	rjmp loop

	////
		; keypad input
	////

keypad_part:
	ldi mask, INITCOLMASK ; initial column mask
	clr col				  ; initial column
colloop:
	STS PORTL, mask       ; set column to mask value
                          ; (sets column 0 off)
	ldi temp, 0xFF        ; implement a delay so the
                          ; hardware can stabilize
delay:
	dec temp
	brne delay
	LDS temp, PINL        ; read PORTL. Cannot use in 
	andi temp, ROWMASK    ; read only the row bits
	cpi temp, 0xF         ; check if any rows are grounded
	breq nextcol          ; if not go to the next column
	ldi mask, INITROWMASK ; initialise row check
	clr row               ; initial row
rowloop:      
	mov temp2, temp
	and temp2, mask       ; check masked bit
	brne skipconv 
	mov temp, keypad_version
	cpi temp, 1
	breq call_num
	cpi temp, 2
	breq call_working
	rcall convert_char     ; if bit is clear, convert the bitcode
back:
	cpi finish_input_flag, 1
	breq question
	jmp keypad_part        ; and start again
skipconv:
	inc row                ; else move to  the next row
	lsl mask               ; shift the mask to the next bit
	jmp rowloop          
nextcol:     
	cpi col, 3             ; check if we^re on the last column
	breq keypad_part       ; if so, no buttons were pushed,
                           ; so start again.

	sec                    ; else shift the column mask:
                           ; We must set the carry bit
	rol mask               ; and then rotate left by a bit,
	inc col                ; increment column value
	jmp colloop            ; and check the next column
call_num:
	rcall convert_num
	rjmp back
call_working:
	rjmp keypad_part
question:
	ldi r16, second_line			;restart all things for keypad
	sts Position, r16
	ldi push_flag,1
	clr letter_num
	clr finish_input_flag
	clr count_letter
	sei
	ret
////////////////////////////////
////convert result to character
///////////////////////////////
convert_char:
	;add a debouncing here, at first it's not stable,when we detect a key pushed
	rcall sleep_350ms
	
	;wait until disturbing signal disappear then convert
	cpi col, 3										
	breq letters
	cpi col, 2
	mov temp, row 
	lsl temp 
	add temp, row
	add temp, col 
	mov r16, temp
	lsl temp
	add temp, r16
	ldi r16,0b01000000
	add temp,r16
	inc temp
	jmp convert_end
letters:
	cpi row, 0
	breq delete
	cpi row, 2
	breq c_for_finish ;C for confirm
	jmp ending

c_for_finish:
	ldi finish_input_flag,1
	rjmp ending
convert_end:
	add temp, count_letter
	inc count_letter
	cpi count_letter,0b00000011
	breq remain
final:
	cpi letter_num, 11
	brsh no_num
	clr push_flag
	cpi temp,0b01011011 
	brne go_on
	ldi temp, 0b10100000
normal:
	mov row,temp             ;row is useless now ,so treat it as another temp
	lds r16, Position
	do_lcd_command_imme r16
	do_lcd_data_imme row
	clear TempCounter
	clear SecondCounter
ending:
	ret                     ; return to caller
go_on:
	rjmp normal
remain:
	ldi count_letter,0b00000000
	rjmp final
delete:
	; move cursor to the correct pos
	lds r16, Position
	cpi r16, 1
	breq cursor_decremented
		dec r16
	    do_lcd_command_imme r16
		sts Position,r16
	
	cursor_decremented:
	
	cpi count_letter, 0b00000000
	breq letters_decremented
		dec count_letter
		do_lcd_data ' '
		
	letters_decremented:
	
	rcall sleep_350ms 
	rjmp ending
	
	
no_num:	
	rjmp ending ;normal



convert_num:
	cli
	rcall sleep_350ms 
                                            ;add a debouncing here, at first it's not stable,when we detect a key pushed
	cpi col, 3 					;wait until disturbing signal disappear then convert					
	breq num_letter
	
	; If zero
	isZeroPressedChk:
		cpi col, 1
		brne isZeroPressedChkFin
		cpi row, 3
		breq zero_num
		
	isZeroPressedChkFin:
	
	mov temp, row 
	lsl temp 
	add temp, row 
	add temp, col 
	inc temp
					
	cpi push_flag,0
	breq time10
update_tempnum:
	lds temp2,TempNumInfo
	add temp2, temp                                    ;push = 0, has pressed
	sts TempNumInfo, temp2
	ldi r16,48
	add temp,r16
	clr push_flag
	jmp convert_num_end
num_letter:
	cpi row, 1
	breq zero_num
	cpi row, 2
	breq c_for_finish_num
	jmp ending
c_for_finish_num:
	ldi finish_input_flag,1
	rjmp num_end
zero_num:
	ldi temp, 0b00110000
	lds temp2,TempNumInfo
	timeten temp2
	sts TempNumInfo, temp2
convert_num_end:
	do_lcd_data_imme temp
num_end:
	ret ; return to caller
time10:
	lds temp2,TempNumInfo
	timeten temp2
	sts TempNumInfo, temp2
	rjmp update_tempnum

///
	;store operation
///

store_name:
	cli
	push temp
	push temp2
	lds temp,count_question
	ldi temp2,10
	//move y to current station
	point_station:
		load1:
			cpi temp,0
			brne load2
			ldi yl,low(station1)
			ldi yh,high(station1)
			rjmp clear_10
		load2:
			cpi temp,1
			brne load3
			ldi yl,low(station2)
			ldi yh,high(station2)
			rjmp clear_10
		load3:
			cpi temp,2
			brne load4
			ldi yl,low(station3)
			ldi yh,high(station3)
			rjmp clear_10
		load4:
			cpi temp,3
			brne load5
			ldi yl,low(station4)
			ldi yh,high(station4)
			rjmp clear_10
		load5:
			cpi temp,4
			brne load6
			ldi yl,low(station5)
			ldi yh,high(station5)
			rjmp clear_10
		load6:
			cpi temp,5
			brne load7
			ldi yl,low(station6)
			ldi yh,high(station6)
			rjmp clear_10
		load7:
			cpi temp,6
			brne load8
			ldi yl,low(station7)
			ldi yh,high(station7)
			rjmp clear_10
		load8:
			cpi temp,7
			brne load9
			ldi yl,low(station8)
			ldi yh,high(station8)
			rjmp clear_10
		load9:
			cpi temp,8
			brne load10
			ldi yl,low(station9)
			ldi yh,high(station9)
			rjmp clear_10
		load10:
			ldi yl,low(station10)
			ldi yh,high(station10)
			rjmp clear_10
		
	//clear 10 byte
	clear_10:
		sts current_name_pointer, yl
		sts current_name_pointer+1,yh
		clr r6
		clear_10_loop:
			cpi temp2,10
			breq store_name_end
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				st y+,r6
				dec temp2
				rjmp clear_10_loop
	store_name_end:	
		pop temp2
		pop temp
		sei
		ret
//storetime
store_time:
	cli
	push temp
	push yl
	push yh
	lds temp,count_question
	ldi yl,low(time_data)
	ldi yh,high(time_data)
	store_time_loop:		
		cpi temp,0
		breq store_time_end
		ld r7,y+
		dec temp	
		rjmp store_time_loop
	store_time_end:
		sts current_time_pointer,yl
		sts current_time_pointer+1,yh
		pop yh
		pop yl
		pop temp
		sei
		ret

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, r16
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	ret

lcd_data:
	out PORTF, r16
	lcd_set LCD_RS
	nop
	nop
	nop
	lcd_set LCD_E
	nop
	nop
	nop
	lcd_clr LCD_E
	nop
	nop
	nop
	lcd_clr LCD_RS
	ret

lcd_wait:
	push r16
	clr r16
	out DDRF, r16
	out PORTF, r16
	lcd_set LCD_RW
lcd_wait_loop:
	nop
	lcd_set LCD_E
	nop
	nop
    nop
	in r16, PINF
	lcd_clr LCD_E
	sbrc r16, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser r16
	out DDRF, r16
	pop r16
	ret

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret
sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret
sleep_1000ms:
	clr r16
	d_loop1000ms:
		inc r16
		rcall sleep_5ms
		cpi r16, 200
		brne d_loop1000ms
	ret
sleep_100ms:
	clr r16
	d_loop100ms:
		inc r16
		rcall sleep_5ms
		cpi r16, 20
		brne d_loop100ms
	ret
sleep_350ms:
	clr r16
	d_loop350ms:
		inc r16
		rcall sleep_5ms
		cpi r16, 70
		brne d_loop350ms
	ret