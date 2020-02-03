  AREA  memory, DATA, READWRITE
num space 11

  AREA  AsmTemplate, CODE, READONLY
  IMPORT  main

; sample program makes the 4 LEDs P1.16, P1.17, P1.18, P1.19 go on and off in sequence
; (c) Mike Brady, 2011 -- 2019.

  EXPORT  start
start

IO1DIR  EQU  0xE0028018
IO1SET  EQU  0xE0028014
IO1CLR  EQU  0xE002801C
LEN     EQU  9

  ; initialise SP
  ldr r13, =0x40002000 ; initialise SP to top of stack

  ;
  ; main
  ;
  bl initLed
  ldr r4, =0x40000000
  ldr r0, =-1049
  mov r1, r4
  bl itoa
  ldr r7, =0x000f0000         ; led_mask = 0x000f0000
  ldr r8, =IO1SET             ; address = IO1SET
whileT                        ; while (true) {
  mov r5, r4
  ldrb r6, [r5]
forchr
  cmp r6, #'\0'               ;   for (str = itoa(num); *str != '\0'; str++)
  beq eforchr                 ;   {
  mov r0, r6
  bl displayNum               ;     displayNum(*str)
  bl delay                    ;     delay()
  str r7, [r8]                ;     turn_off_leds(P1.19-P1.16)
  ldrb r6, [r5, #1]!
  b forchr                    ;   }
eforchr
  bl delay                    ;   delay()
  b whileT                    ; }

stop b stop

;
; displayNum
; displays the least 4 significant digits on leds P1.19-P1.16
; prerequisite:
;   leds P.19-P1.16 have been setup to output and are off
; parameters:
;   r0 - num which is the ascii value of the digit to be displayed
; return:
;   none
;
displayNum
  stmfd sp!, {r4, lr}
  ldr r4, =IO1CLR             ; address = IO1CLR
  cmp r0, #'0'                ; if (num == '0')
  bne dnel0                   ; {
  ldr r2, =0x000f0000         ;   mask = 0x000f0000
  str r2, [r4]                ;   show_zero()
  b dneif0                    ; }
dnel0                         ;
  cmp r0, #'-'                ; else if (num == '-')
  bne dnel1                   ; {
  and r0, r0, #0xf            ;   num &= 0xf
  mov r0, r0, lsl #16         ;   num <<= 16
  str r0, [r4]                ;   show_neg()
  b dneif0                    ; }
dnel1                         ; else
                              ; {
  and r0, r0, #0xf            ;   num &= 0xf
  mov r1, #4                  ;   num_bits = 4
  bl reverse                  ;   reverse(num)
  mov r0, r0, lsl #16         ;   num <<= 16
  str r0, [r4]                ;   show_num()
dneif0                        ; }
  ldmfd sp!, {r4, pc}

;
; reverse
; reverses the input num bitwise
; parameters:
;   r0 - num to reverse
;   r1 - num_bits is significant bits to reverse
; return:
;   r0 - reversed bits
;
reverse
  stmfd sp!, {r4, lr}
  sub r1, r1, #1              ; num_bits--
  mov r2, #1                  ; mask = 1
  mov r4, #0                  ; result = 0
rWhileN
  cmp r0, #0                  ; while (num != 0)
  beq reWhileN                ; {
  and r3, r0, r2              ;   bit = mask & num
  cmp r3, #0                  ;   if (bit != 0)
  beq reif                    ;   {
  orr r4, r4, r2, lsl r1      ;     result |= 1 << num_bits
reif                          ;   }
  mov r0, r0, lsr #1          ;   num >>= 1
  sub r1, r1, #1              ;   num_bits--
  b rWhileN                   ; }
reWhileN
  mov r0, r4                  ; reversed = result
  ldmfd sp!, {r4, pc}

;
; itoa
; converts a signed 32 bit integer to ascii
; parameters:
;   r0 - num which is a 32 bit integer
;   r1 - address containing sufficient space to store ascii version of the
;        integer
; return:
;   r0 - the size of the string
;
itoa
  stmfd sp!, {r4-r9, lr}
  mov r3, r0                    ; r3 = num
  mov r4, r1                    ; r4 = address
  mov r5, #0                    ; i = 0
  cmp r3, #0                    ; if (num < 0)
  bge itoaeIf                   ; {
  mov r2, #'-'                  ;   neg_char = '-'
  strb r2, [r4, r5]             ;   result[i] = neg_char
  mvn r3, r3                    ;   num ~= num
  add r3, r3, #1                ;   num += 1
  mvn r0, r0                    ;   orig_num ~= orig_num
  add r0, r0, #1                ;   orig_num += 1
  add r5, r5, #1                ;   i++
itoaeIf                         ; }
  ldr r6, =table                ; table[]
  mov r7, #LEN                  ;
  sub r7, r7, #1                ;
itoaforj
  cmp r7, #0                    ; for (j = LEN - 1; j >= 0; j--)
  blt itoaeforj                 ; {
  ldr r8, [r6, r7, lsl #2]      ;   divisor = table[j]
  mov r9, #0                    ;   counter = 0
  sub r3, r3, r8                ;   num -= divisor
itoaWhileN
  cmp r3, #0                    ;   while (num > 0)
  ble itoaeWhileN               ;   {
  sub r3, r3, r8                ;     num -= divisor
  add r9, r9, #1                ;     counter++
  b itoaWhileN                  ;   }
itoaeWhileN
  add r3, r3, r8                ;   num += divisor
  cmp r3, r0                    ;   if (num != orig_num)
  beq itoaeif0                  ;   {
  add r9, r9, #'0'              ;     digit = Character.valueOf(digit)
  strb r9, [r4, r5]             ;     result[i] = digit
  add r5, r5, #1                ;     i++
itoaeif0                        ;   }
  sub r7, r7, #1
  b itoaforj                    ; }
itoaeforj
  add r3, r3, #'0'              ; digit = Character.valueOf(digit)
  strb r3, [r4, r5]             ; result[i] = digit
  add r5, r5, #1                ; i++
  mov r1, #'\0'                 ; null = '\0'
  strb r1, [r4, r5]             ; result[i] = '\0'
  mov r0, r5                    ; size = i
  ldmfd sp!, {r4-r9, pc}


;
; initLed
; initialise P1.19-P1.16 to output and turn them off
; parameters:
;   none
; return:
;   none
;
initLed
  ldr r1, =IO1DIR
  ldr r2, =0x000f0000  ;select P1.19--P1.16
  str r2, [r1]    ;make them outputs
  ldr r1, =IO1SET
  str r2, [r1]    ;set them to turn the LEDs off
  bx lr

;
; delay
; hauts the program for about half as second
; parameters:
;   none
; return:
;   none
;
delay
  ldr  r0,=40000000
dloop  subs  r0,r0,#1
  bne  dloop
  bx lr

table
  dcd 10
  dcd 100
  dcd 1000
  dcd 10000
  dcd 100000
  dcd 1000000
  dcd 10000000
  dcd 100000000
  dcd 1000000000

  END
