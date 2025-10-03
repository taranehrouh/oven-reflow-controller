; 76E003 ADC test program: Reads channel 7 on P1.1, pin 14
; This version uses an LED as voltage reference connected to pin 6 (P1.7/AIN0)

$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;

CLK               EQU 16600000 ; Microcontroller system frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
BAUD              EQU 115200 ; Baud rate of UART in bps
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
TIMER1_RELOAD     EQU (0x100-(CLK/(16*BAUD)))
TIMER0_RELOAD_1MS EQU (0x10000-(CLK/1000))

;FC_BUTTON     equ P1.6
UPDOWN        equ P1.5
SOUND_OUT     equ P1.0
PB6           equ P1.6
;PWM_OUT	      equ P0.4
PWM_OUT	  	  equ P1.2
LM			  equ P0.4
;PUSHbuttons in P1.3
;Voltage reading coming in on pin P0.4
;TOGGLE_BUTTON equ 
;SOAK_TIME_BUTTON equ
;SOAK_TEMP_BUTTON equ
;REFLOW_TIME_BUTTON equ
;REFLOW_TEMP_BUTTON equ

ORG 0x0000
	ljmp main
	
	
org 0x000B
	ljmp Timer0_ISR
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR


;                     1234567890123456    <- This helps determine the location of the counter
test_message:     db 'TOxxxC TR=xxxxC', 0
test_message1:     db 'TOxxxF TR=xxxxF', 0
s:    db 's', 0
r:    db 'r', 0
message1: db 'Time:', 0
message2: db 'S', 0
message3: db ' R', 0
start_message: db 'Starting Oven', 0
clabel: db 'C', 0
flabel: db 'F', 0
Sa: db 'A', 0
Sb: db 'RAMP TS', 0
Sc: db 'SOAKING', 0
Sd: db 'RAMP TR', 0 
Se: db 'REFLOW ', 0 
Sf: db 'COOLING', 0 
cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

; These register definitions needed by 'math32.inc'
DSEG at 30H
x:   ds 4
y:   ds 4
bcd: ds 5
;CorF: ds 1
VLED_ADC: ds 2
; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 2 ; The BCD counter incrememted in the ISR and displayed in the main loop
Temp:  ds 2
hours: ds 1
hundreds: ds 1
minutes: ds 1
FSM1_state: ds 1
pwm: ds 2
temperature: ds 2
soak_temp: ds 3
reflow_temp: ds 3 
soak_time: ds 3
reflow_time: ds 3
cool_temp: ds 3 
t_byte_1: ds 1
t_byte_2: ds 1
pwm_counter: ds 2
cold_j: ds 4
t_couple: ds 4
toggle_display: ds 1
not_flipping_the_code: ds 4
button_temp: ds 4
swapcf: ds 1


; These five bit variables store the value of the pushbuttons after calling'LCD_PB' below


BSEG
mf: dbit 1
CorF: dbit 1
seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
TOGGLE_BUTTON: dbit 1
SOAK_TEMP_BUTTON: dbit 1
SOAK_TIME_BUTTON: dbit 1
REFLOW_TIME_BUTTON: dbit 1
REFLOW_TEMP_BUTTON: dbit 1

$NOLIST
$include(math32.inc)
$LIST

;LAB 2 FUNCTIONS

;TIMER 0 is ONLY for the speaker
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret


;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz wave at pin SOUND_OUT   ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker the pin assigned to 'SOUND_OUT'!
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	
	;CODE FROM POWER DEMO
	inc pwm_counter
	clr c
	mov a, pwm
	subb a, pwm_counter ; If pwm_counter <= pwm then c=1
	cpl c
	mov PWM_OUT, c
	mov a, pwm_counter
	cjne a, #100, skip2
	mov pwm_counter, #0

skip2:
	 ; It is super easy to keep a seconds count here
	setb seconds_flag
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
;------------------------------------------------------;
;	CLOCK--Inc_Done is the body of the clock
;------------------------------------------------------;
Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
	add a, #0x01
	da a
	cjne a, #0x00, continue_seconds
	inc hundreds	

	continue_seconds:	
	   mov BCD_counter, a
	   sjmp Inc_Done	; if seconds reached 60, set it back to 0 and then go back to increment 
 
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01
	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti


Init_All:
	; Configure all the pins for biderectional I/O
	mov	P3M1, #0x00
	mov	P3M2, #0x00
	mov	P1M1, #0x00
	mov	P1M2, #0x00
	mov	P0M1, #0x00
	mov	P0M2, #0x00

	orl	CKCON, #0x10 ; CLK is the input for timer 1
	orl	PCON, #0x80 ; Bit SMOD=1, double baud rate
	mov	SCON, #0x52
	anl	T3CON, #0b11011111
	anl	TMOD, #0x0F ; Clear the configuration bits for timer 1
	orl	TMOD, #0x20 ; Timer 1 Mode 2
	mov	TH1, #TIMER1_RELOAD ; TH1=TIMER1_RELOAD;
	setb TR1
	
	; Using timer 0 for delay functions.  Initialize here:
	clr	TR0 ; Stop timer 0
	orl	CKCON,#0x08 ; CLK is the input for timer 0
	anl	TMOD,#0xF0 ; Clear the configuration bits for timer 0
	orl	TMOD,#0x01 ; Timer 0 in Mode 1: 16-bit timer
	
	; Initialize the pins used by the ADC (P1.1, P1.7, Pin 20 (AIN5)) as input.
	orl	P1M1, #0b10000010
	anl	P1M2, #0b01111101
	
	;GENIUS DETECTIVE LINES
	orl P0M1, #0b00010000
	anl P0M2, #0b11101111
	
	; Initialize and start the ADC:
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	; AINDIDS select if some pins are analog inputs or digital I/O:
	mov AINDIDS, #0x00 ; Disable all analog inputs
	orl AINDIDS, #0b10100001 ; Activate AIN0 and AIN7 analog inputs (AND AIN5)
	orl ADCCON1, #0x01 ; Enable ADC
	
	ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
 
Hello_World:
    DB  '\r', '\n', 0
	
wait_1ms:
	clr	TR2 ; Stop timer 0
	clr	TF2 ; Clear overflow flag
	mov	TH2, #high(TIMER0_RELOAD_1MS)
	mov	TL2,#low(TIMER0_RELOAD_1MS)
	setb TR2
	jnb	TF2, $ ; Wait for overflow
	ret

; Wait the number of miliseconds in R2
waitms:
	lcall wait_1ms
	djnz R2, waitms
	ret

; We can display a number any way we want.  In this case with
;two decimal places.
Display_formated_BCD:
	Set_Cursor(1, 11)
	Display_BCD(bcd+1)
	Display_BCD(bcd+0)
	Set_Cursor(1, 11)
	Display_char(#'=')
	Set_Cursor(1, 15)
	Display_char(#'C')
	
	Set_Cursor(1, 4)
	Display_BCD(cold_j+0)
	Set_Cursor(1, 3)
	Display_char(#'=')

	ret
	
Read_ADC:
	clr ADCF
	setb ADCS ;  ADC start trigger signal
    jnb ADCF, $ ; Wait for conversion complete
    
    ; Read the ADC result and store in [R1, R0]
    mov a, ADCRL
    anl a, #0x0f
    mov R0, a
    mov a, ADCRH   
    swap a
    push acc
    anl a, #0x0f
    mov R1, a
    pop acc
    anl a, #0xf0
    orl a, R0
    mov R0, A
	ret
	
	
Send_BCD mac
	push ar0
	mov r0, %0
	lcall ?Send_BCD
	pop ar0
endmac

?Send_BCD:
	push acc
	;write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall putchar
	;write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall putchar
	pop acc
	ret
	
LCD_PB:
	; Set variables to 1: 'no push button pressed'
	setb SOAK_TIME_BUTTON
	setb SOAK_TEMP_BUTTON
	setb REFLOW_TEMP_BUTTON
	setb REFLOW_TIME_BUTTON
	setb TOGGLE_BUTTON
	; The input pin used to check set to '1'
	setb P1.5
	
	; Check if any push button is pressed
	clr P0.0
	clr P0.1
	clr P0.2
	clr P0.3
	clr P1.3
	jb P1.5, LCD_PB_Done
	
	; Debounce
	mov R2, #50
	lcall waitms
	jb P1.5, LCD_PB_Done
	
	; Set the LCD data pins to logic 1
	setb P0.0
	setb P0.1
	setb P0.2
	setb P0.3
	setb P1.3
	
	; Check the push buttons one by one
	clr P1.3
	mov c, P1.5
	mov TOGGLE_BUTTON, c
	setb P1.3
	clr P0.0
	mov c, P1.5
	mov REFLOW_TIME_BUTTON, c
	setb P0.0
	clr P0.1
	mov c, P1.5
	mov REFLOW_TEMP_BUTTON, c
	setb P0.1
	clr P0.2
	mov c, P1.5
	mov SOAK_TEMP_BUTTON, c
	setb P0.2
	clr P0.3
	mov c, P1.5
	mov SOAK_TIME_BUTTON, c
	setb P0.3
	
LCD_PB_Done:
    ret
    
Display_PushButtons_LCD:
	Set_Cursor(2, 1)
	mov a, #'0'
	mov c, TOGGLE_BUTTON
	addc a, #0
	lcall ?WriteData
	mov a, #'0'
	mov c, REFLOW_TIME_BUTTON
	addc a, #0
	lcall ?WriteData
	mov a, #'0'
	mov c, REFLOW_TEMP_BUTTON
	addc a, #0
	lcall ?WriteData
	mov a, #'0'
	mov c, SOAK_TEMP_BUTTON
	addc a, #0
	lcall ?WriteData
	mov a, #'0'
	mov c, SOAK_TIME_BUTTON
	addc a, #0
	lcall ?WriteData
	ret
	
Average_ADC:
	Load_x(0)
	mov R5, #100
	Sum_loop0:
	lcall Read_ADC
	mov y+3, #0
	mov y+2, #0
	mov y+1, R1
	mov y+0, R0
	lcall add32
	djnz R5, Sum_loop0
	load_y(100)
	lcall div32
	ret
;-----------------------------------------------------------------------------;
;		   THIS MARKS THE START OF FUNCTIONING CODE
;     EVERYTHING ABOVE IS CALLED FUNCTIONS, and doesn't run before main
;-----------------------------------------------------------------------------;
main:
	mov hundreds, #0x00
	mov sp, #0x7f
	lcall Init_All
    lcall Timer2_Init ;initialize timer 2--IMPORTANT LINE
    
	setb EA   ; Enable Global interrupts
	lcall LCD_4BIT
	mov BCD_counter, #0x00
	mov minutes, #0x00
	mov hours, #0x01
	setb seconds_flag
    mov BCD_counter, #0x00
    mov temp, #0x00
    mov swapcf, #0x00
	
    ;clearing buttons initially since they are bits (might need to change to setb
    clr TOGGLE_BUTTON
    clr SOAK_TIME_BUTTON
    clr SOAK_TEMP_BUTTON
    clr REFLOW_TIME_BUTTON
    clr REFLOW_TEMP_BUTTON
    
    ;Setting soak_temp
    mov soak_temp+0, #0x50
    mov soak_temp+1, #0x01
    mov soak_time, #0x61
    
    ;Setting ramp_temp 
    mov reflow_temp+0, #0x20
    mov reflow_temp+1, #0x02
    mov reflow_time, #0x45

    
    
    ;Setting cool_temp 
    mov cool_temp+0, #0x60 ;60 in hex
    mov cool_temp+1, #0x00 ;60 in hex
 
    
    ;set CorF bit to 0 (starts C)
	setb CorF
	cpl CorF
    
    ; initial messages in LCD
    ;next two lines are old message from lab 3
	;Set_Cursor(1, 1)
    ;Send_Constant_String(#test_message)
    Set_Cursor(1, 1)
    Send_Constant_String(#test_message)
    Set_Cursor(2,1)
    Send_Constant_String(#message1) ; this is the line for time display
	;Set_Cursor(2, 1)
    ;Send_Constant_String(#value_message)
    ;Set_Cursor(2,16)
    ;Send_Constant_String(#clabel)

				ljmp proper_reset

mainf:
	mov hundreds, #0x00
	mov sp, #0x7f
	lcall Init_All
    lcall Timer2_Init ;initialize timer 2--IMPORTANT LINE
    
	setb EA   ; Enable Global interrupts
	lcall LCD_4BIT
	mov BCD_counter, #0x00
	mov minutes, #0x00
	mov hours, #0x01
	setb seconds_flag
    mov BCD_counter, #0x00
    mov temp, #0x00
    mov swapcf, #0x01
	
    ;clearing buttons initially since they are bits (might need to change to setb
    clr TOGGLE_BUTTON
    clr SOAK_TIME_BUTTON
    clr SOAK_TEMP_BUTTON
    clr REFLOW_TIME_BUTTON
    clr REFLOW_TEMP_BUTTON
    
    ;Setting soak_temp
    mov soak_temp+0, #0x02
    mov soak_temp+1, #0x03
    mov soak_time, #0x61
    
    ;Setting ramp_temp 
    mov reflow_temp+0, #0x28
    mov reflow_temp+1, #0x04
    mov reflow_time, #0x45

    
    
    ;Setting cool_temp 
    mov cool_temp+0, #0x60 ;60 in hex
    mov cool_temp+1, #0x00 ;60 in hex
 
    
    ;set CorF bit to 0 (starts C)
	setb CorF
	cpl CorF
    
    ; initial messages in LCD
    ;next two lines are old message from lab 3
	;Set_Cursor(1, 1)
    ;Send_Constant_String(#test_message)
    Set_Cursor(1, 1)
    mov a, swapcf
    cjne a, #0x00, message
    Send_Constant_String(#test_message)
    ljmp skiperdodal
    message:
    Send_Constant_String(#test_message1)
  skiperdodal:  
    Set_Cursor(2,1)
    Send_Constant_String(#message1) ; this is the line for time display
	;Set_Cursor(2, 1)
    ;Send_Constant_String(#value_message)
    ;Set_Cursor(2,16)
    ;Send_Constant_String(#clabel)
 proper_reset: mov FSM1_state, #0x00
    			mov pwm, #0
				mov toggle_display, #0x00
;------------------------------------------------------------------------------;
;   FOREVER LOOP--everything that we want to happen more than once goes here
;------------------------------------------------------------------------------;

Forever:

	mov R2, #250
	lcall waitms

sjmp toggle

;----------------------------------------------------;
;	Toggle used to minimize buttons. 	     ;
;	Toggle + soaktime = decrement soaktime	     ;
;	Toggle + reflowtime = decrement reflowtime   ;
;	Toggle + soaktemp = decrement soaktemp	     ;
;	Toggle + reflowtemp = decrement reflowtemp   ; 
;----------------------------------------------------;
toggle: 
;	jnb TOGGLE_BUTTON, decrement_soak_time_buffer	;change this to increment_soak_time if toggle thingy decrements first 
;	Wait_Milli_Seconds(#50)
	jnb TOGGLE_BUTTON, decrement_soak_time_buffer
	jnb TOGGLE_BUTTON, $
	ljmp increment_soak_time 

increment_soak_time:
;	jb SOAK_TIME_BUTTON, increment_soak_temp	;if SOAK_TIME_BUTTON not pressed, check to see if soak temp is being changed
;	Wait_Milli_Seconds(#50)
	jb SOAK_TIME_BUTTON, increment_soak_temp
	jb SOAK_TIME_BUTTON, $
	mov a, soak_time
	inc a
	da a
	mov soak_time, a
	ljmp forever_start ;JUMP TO WHEREVER DISPLAYS ON LCD 
	
decrement_soak_time_buffer:
	ljmp decrement_soak_time
	
increment_soak_temp:
;	jb SOAK_TEMP_BUTTON, increment_reflow_time ;if soak_temp isn't being changed, check for increment_reflow_time
;	Wait_Milli_Seconds(#50)
	jb SOAK_TEMP_BUTTON, increment_reflow_time
	jb SOAK_TEMP_BUTTON, $
	mov a, soak_temp
	inc a
	da a
	mov soak_temp, a
	ljmp forever_start

increment_reflow_time:
;	jb REFLOW_TIME_BUTTON, increment_reflow_temp
;	Wait_Milli_Seconds(#50)
	jb REFLOW_TIME_BUTTON, increment_reflow_temp
	jb REFLOW_TIME_BUTTON, $
	mov a, reflow_time
	inc a
	da a
	mov reflow_time, a
	ljmp forever_start
	

increment_reflow_temp:
;	jb REFLOW_TEMP_BUTTON, forever_start_buffer
;	Wait_Milli_Seconds(#50)
	jb REFLOW_TEMP_BUTTON, forever_start_buffer
	jb REFLOW_TEMP_BUTTON, $
	mov a, reflow_temp
	inc a
	da a
	mov reflow_temp, a
	ljmp forever_start

forever_start_buffer:
	ljmp forever_start
	
decrement_soak_time:
;	jb SOAK_TIME_BUTTON, decrement_soak_temp	;if SOAK_TIME_BUTTON not pressed, check to see if soak temp is being changed
;	Wait_Milli_Seconds(#50)
	jb SOAK_TIME_BUTTON, decrement_soak_temp
	jb SOAK_TIME_BUTTON, $
	mov a, soak_time
	subb a, #0x01
	da a
	mov button_temp, a
	add a, #0x01
	cjne a, soak_time, notchange3
	mov soak_time, button_temp
	
	ljmp forever_start
	notchange3:
	subb a, #13
	
		mov soak_time, a
	
	ljmp forever_start

decrement_soak_temp:
;	jb SOAK_TEMP_BUTTON, decrement_reflow_time ;if soak_temp isn't being changed, check for increment_reflow_time
;	Wait_Milli_Seconds(#50)
	jb SOAK_TEMP_BUTTON, decrement_reflow_time
	jb SOAK_TEMP_BUTTON, $
	mov a, soak_temp
	
	subb a, #0x01
	da a
	mov button_temp, a
	add a, #0x01
	cjne a, soak_temp, notchange
	mov soak_temp, button_temp
	
	ljmp forever_start
	notchange:
	subb a, #13
	
		mov soak_temp, a
	
	ljmp forever_start

decrement_reflow_time:
;	jb REFLOW_TIME_BUTTON, decrement_reflow_temp
;	Wait_Milli_Seconds(#50)
	jb REFLOW_TIME_BUTTON, decrement_reflow_temp
	jb REFLOW_TIME_BUTTON, $
	mov a, reflow_time
		subb a, #0x01
	da a
	mov button_temp, a
	add a, #0x01
	cjne a, reflow_time, notchange1
	mov reflow_time, button_temp
	
	ljmp forever_start
	notchange1:
	subb a, #13
	
		mov reflow_time, a
	
	ljmp forever_start

decrement_reflow_temp:
;	jb REFLOW_TEMP_BUTTON, forever_start
;	Wait_Milli_Seconds(#50)
	jb REFLOW_TEMP_BUTTON, forever_start
	jb REFLOW_TEMP_BUTTON, $
	mov a, reflow_temp
		subb a, #0x01
	da a
	mov button_temp, a
	add a, #0x01
	cjne a, reflow_temp, notchange2
	mov reflow_temp, button_temp
	
	ljmp forever_start
	notchange2:
	subb a, #13
	
		mov reflow_temp, a
	
	ljmp forever_start

forever_start:
	
	


	ljmp cont
	;jb FC_BUTTON, Cont
	;Wait_Milli_Seconds(#20)
	;jb FC_BUTTON, Cont
	;jnb FC_BUTTON, $
	
	;jnb CorF, ToF
	;cpl CorF	
	;Set_Cursor(2,16)
	;Send_Constant_String(#clabel)
	;ljmp cont
;ToF:
	;cpl CorF
	;Set_Cursor(2,16)
	;Send_Constant_String(#flabel)
	;ljmp cont
ryker:
	ljmp forever
Cont:
		; Read the voltage reference connected to AIN0 on pin 6
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x00 ; Select channel 0
	lcall LCD_PB
	lcall Read_ADC
	; Save result for later use
	mov VLED_ADC+0, R0
	mov VLED_ADC+1, R1

	; Read the signal connected to AIN7--THIS IS THE THERMOCOUPLE
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x07 ; Select channel 7
	
	lcall Average_ADC
    
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(41188) ; The MEASURED ref voltage voltage: 4.1188V, with 4 decimal places
	lcall mul32
	; Retrive the  value
	mov y+0, VLED_ADC+0
	mov y+1, VLED_ADC+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32
		
	
	
	load_y(100)
	lcall mul32 ; The voltage is in uV
	load_y(196)
	lcall div32
	load_y(41)
	lcall div32
	
	
	mov t_couple+1, x+1
	mov t_couple+0, x+0
	
	;HARDCODED 22C
	;load_y(22)
	;lcall add32
	
ljmp skip3

ryker2:
	ljmp forever	

skip3:

		; Read the voltage reference connected to AIN0 on pin 6
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x00 ; Select channel 0

	lcall Read_ADC
	; Save result for later use
	mov VLED_ADC+0, R0
	mov VLED_ADC+1, R1

	; Read the signal connected to AIN5-THIS IS THE LM
	anl ADCCON0, #0xF0
	orl ADCCON0, #0x05 ; Select channel 5 (Pin 20)
    lcall Read_ADC
    ; Convert to voltage
	mov x+0, R0
	mov x+1, R1
	; Pad other bits with zero
	mov x+2, #0
	mov x+3, #0
	Load_y(41188) ; The MEASURED voltage ref: 4.1188V, with 4 decimal places
	lcall mul32
	
	; Retrive the ADC LED value
	mov y+0, VLED_ADC+0
	mov y+1, VLED_ADC+1
	; Pad other bits with zero
	mov y+2, #0
	mov y+3, #0
	lcall div32
	
	mov x+2, #0
	mov x+3, #0
	Load_y(27300)
	lcall sub32
	
	load_y(100)
	lcall div32
	
	lcall hex2bcd
	
	mov cold_j, bcd
	
	;ADD THERMOCOUPLE TO LM READING
	mov y+0, t_couple+0
	mov y+1, t_couple+1
	mov y+2, #0
	mov y+3, #0
	lcall add32
	
	
	clr ET0
	
	mov a, swapcf
	cjne a, #0x01, loop_a
	load_Y (00002)
	lcall mul32
	load_Y (00009)
	lcall mul32
	load_Y (00010)
	lcall div32
	load_Y (03200)
	lcall add32
	
loop_a:
	jnb seconds_flag, ryker3		
	ljmp skip4

ryker3:
	ljmp forever	

skip4:

loop_b:
      clr seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    mov a, toggle_display
    cjne a, #0x01, set_temp_display
	Set_Cursor(2,1)
    Send_Constant_String(#message1)
	Set_Cursor(2, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit.inc'
	ljmp skip1
	
set_temp_display:
	Set_Cursor(2,1)
	Display_BCD(soak_temp+1)
	Display_BCD(soak_temp+0)
	Display_char(#',')
	Display_BCD(soak_time+0)
	Set_Cursor(2,1)
	Send_Constant_String(#message2)
	
	Set_Cursor(2,9)
	Display_BCD(reflow_temp+1)
	Display_BCD(reflow_temp+0)
	Display_char(#',')
	Display_BCD(reflow_time+0)
	Set_Cursor(2,8)
	Send_Constant_String(#message3)

skip1:

	lcall hex2bcd
	send_BCD(bcd+1)
	send_BCD(bcd+0)

	; Convert to BCD and display

	lcall Display_formated_BCD
	
	mov SP, #7FH ; Set the stack pointer to the begining of idata
	lcall Init_All
    mov DPTR, #Hello_World
    lcall SendString
    
  FSM1: 
	mov a, FSM1_state
	
  FSM1_state0:
	cjne a, #0, FSM1_state1
	
	jb PB6, FSM1_state0_done
checkeing:	
	jb TOGGLE_BUTTON, convert
	jnb PB6, checkeing ; Wait for key release
	
	
	Send_BCD(soak_temp+1)
	Send_BCD(soak_temp+0)
	mov a, #' '
	lcall putchar
	
	Send_BCD(soak_time+0)
	lcall putchar
	
	Send_BCD(reflow_temp+1)
	Send_BCD(reflow_temp+0)
	lcall putchar
	
	Send_BCD(reflow_time+0)
	mov a, #'\n'
	lcall putchar
	lcall Timer0_Init
	mov FSM1_state, #1
	mov BCD_counter, #0
	ljmp exit
  FSM1_state0_done: ; sends system info to terminal
  	mov A, start_message
  	lcall SendString
	ljmp exit
convert:
	mov a, swapcf
	cjne a, #0x00, ogmf
	ljmp mainf
	ogmf:
	ljmp main	
  FSM1_state1:
 	jb PB6, ryker1
	jnb PB6, $ 
	ljmp proper_reset
	ryker1:
	mov a, FSM1_state
	cjne a, #1, FSM1_state2   ;compare and jump if not equal 
	mov toggle_display, #0x01
	Set_Cursor(2,9)		;if they are equal that means we are in state 2 
    Send_Constant_String(#Sb) ;show this message if we are in state 2 
	mov pwm, #100
	
	mov a, BCD_counter

	

	clr c
	subb a, #0x90
	jc tara_bad_code
	
	mov a, bcd+0
	clr c
	subb a, #0x50
	jc caleb_good_code 
	

	ljmp tara_bad_code 
caleb_good_code:
	ljmp main
tara_bad_code:
	
	mov a, soak_temp+1
	clr c
	cjne a, bcd+1, CheckHighByte ;compare and jump if not equal 
	mov a, soak_temp+0
	clr c
	subb a, bcd+0
	jc update_state ;if carry is set, bcd+1 is greater?
	
	ljmp FSM1_state1_done

	
  update_state:
  lcall Timer0_Init   
	mov FSM1_state, #2
	mov BCD_counter, #0
	ljmp FSM1_state1_done
	
  CheckHighByte:
    jnc NotGreater  ;if no carry soak+1 is greater
    ljmp update_state
    
  NotGreater:
  	ljmp  FSM1_state1_done
  	
  FSM1_state1_done: 

	ljmp exit

  FSM1_state2:
  	jb PB6, ryker7
	jnb PB6, $ 
	ljmp proper_reset
	ryker7:
	;mov bcd, free
	cjne a, #2, FSM1_state3
	Set_Cursor(2,9)
    Send_Constant_String(#Sc)
	mov pwm, #20
	mov a, soak_time
	clr c
	subb a, BCD_counter
	jnc FSM1_state2_done
	mov FSM1_state, #3
	mov BCD_counter, #0x00 
	lcall Timer0_Init
  FSM1_state2_done:
	ljmp exit


  FSM1_state3:
 	jb PB6, ryker8
	jnb PB6, $ 
	ljmp proper_reset
	ryker8:
	cjne a, #3, FSM1_state4   ;compare and jump if not equal 
	Set_Cursor(2,9)		;if they are equal that means we are in state 2 
    Send_Constant_String(#Sd) ;show this message if we are in state 2 
	mov pwm, #100
	mov a, reflow_temp+1
	clr c
	cjne a, bcd+1, CheckHighByte3 ;compare and jump if not equal 
	mov a, reflow_temp+0
	clr c
	subb a, bcd+0
	jc update_state3 ;if carry is set, bcd+1 is greater?
	
	ljmp FSM1_state1_done3
	
  update_state3: 
  lcall Timer0_Init  
	mov FSM1_state, #4
	mov BCD_counter, #0
	ljmp FSM1_state1_done
	
  CheckHighByte3:
    jnc NotGreater3  ;if no carry soak+1 is greater
    ljmp FSM1_state4
    
  NotGreater3:
  	ljmp  FSM1_state1_done3
  	
  FSM1_state1_done3: 

	ljmp exit
		
	
  FSM1_state4:
	jb PB6, ryker4
	jnb PB6, $ 
	ljmp proper_reset
	ryker4:
  	mov a, FSM1_state
  	cjne a, #4, FSM1_state5 
  	Set_Cursor(2,9)  
	Send_Constant_String(#Se)
	mov a, reflow_time
	subb a, BCD_counter
	jnc FSM1_state_done4
	mov FSM1_state, #5
  	mov BCD_counter, #0x00 
  	lcall Timer0_Init
  FSM1_state_done4:	 
 
	ljmp exit
  
  FSM1_state5:
    jb PB6, ryker5
	jnb PB6, $ 
	ljmp proper_reset
	ryker5:  
	mov a, FSM1_state
	cjne a, #5, exit 
	Set_Cursor(2,9)  
	Send_Constant_String(#Sf)
	mov pwm, #0 
	mov BCD_counter, #0 
	
    mov a, bcd+0 
	subb a, cool_temp
	jnc FSM1_state5_done
	
	mov a, cool_temp+1
	clr c
	cjne a, bcd+1, CheckHighByte_5 ;compare and jump if not equal 
	mov a, bcd+0
	clr c
	subb a, cool_temp
	jc update_state_5 ;if carry is set, bcd+1 is greater?
	
	ljmp FSM1_state5_done
	
  update_state_5:
  lcall Timer0_Init  
  	mov toggle_display, #0x00 
	mov FSM1_state, #0
	mov BCD_counter, #0
	ljmp FSM1_state5_done
	
  CheckHighByte_5:
    jc NotGreater_5  ;if no carry bcd+1 is greater
    ljmp update_state_5
    
  NotGreater_5:
  	ljmp  FSM1_state5_done
  	
  FSM1_state5_done: 
	ljmp exit
	
	Set_Cursor(2, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter)
	
exit:
	ljmp Forever

	
END