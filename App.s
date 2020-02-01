  AREA  AsmTemplate, CODE, READONLY
  IMPORT  main

; sample program makes the 4 LEDs P1.16, P1.17, P1.18, P1.19 go on and off in sequence
; (c) Mike Brady, 2011 -- 2019.

  EXPORT  start
start

IO1DIR  EQU  0xE0028018
IO1SET  EQU  0xE0028014
IO1CLR  EQU  0xE002801C

  ; initialise SP
  ldr r13, =0x40002000 ; initialise SP to top of stack

  ;
  ; main
  ;
  bl initLed
  ldr r4, =0x40000000
  ldr r0, =0x80000000
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
  stmfd sp!, {r4-r6, lr}
  mov r3, r0                    ; r3 = num
  mov r4, r1                    ; r4 = address
  mov r5, #0                    ; i = 0
  cmp r3, #0                    ; if (num < 0)
  bge itoaeIf                   ; {
  mov r2, #'-'                  ;   neg_char = '-'
  strb r2, [r4, r5]             ;   result[i] = neg_char
  mvn r3, r3                    ;   num ~= num
  add r3, r3, #1                ;   num += 1
  add r5, r5, #1                ;   i++
itoaeIf                         ; }
  mov r6, #0                    ; j = 0
itoaWhileN
  cmp r3, #0                    ; while (num != 0)
  beq itoaeWhile                ; {
  mov r0, r3                    ;   numerator = num
  mov r1, #10                   ;   denominator = 10
  bl udiv                       ;   div(num, 10)
  str r1, [sp, #-4]!            ;   push(num % 10)
  mov r3, r0                    ;   num /= 10
  add r6, r6, #1                ;   j++
  b itoaWhileN                  ; }
itoaeWhile
itoaWhilej
  cmp r6, #0                    ; while (j > 0)
  ble itoaeWhilej               ; {
  ldr r0, [sp], #4              ;   digit = pop()
  add r0, r0, #'0'              ;   digit = Character.valueOf(digit)
  strb r0, [r4, r5]             ;   result[i] = digit
  add r5, r5, #1                ;   i++
  sub r6, r6, #1                ;   j--
  b itoaWhilej                  ; }
itoaeWhilej
  mov r1, #'\0'                 ; null = '\0'
  strb r1, [r4, r5]             ; result[i] = '\0'
  mov r0, r5                    ; size = i
  ldmfd sp!, {r4-r6, pc}
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
  ldr  r0,=4000000
dloop  subs  r0,r0,#1
  bne  dloop
  bx lr

;
; udiv
; divides the numerator by the denominator and returns the quotient and the
; remainder
; parameters:
;   r0 - numerator
;   r1 - denominator
; return:
;   r0 - quotient
;   r1 - remainder
;
udiv  PUSH {R4-R6, LR}  ; push R2-R6 and LR
  MOV   R2, #0        ; R2 = quotient = 0
  MOV  R3, #0        ; R3 = remaider = 0
  MOV  R4, #0        ; R4 = numberOfBits = 0
  MOV  R5, R0        ; R5 = tempVariable = numerator
  MOV  R6, #0        ; R6 = i
  ; finds number of bits of the numerator
UDIV3  BCS  UDIV4          ; if (!hasCarry), while (TRUE) do (s0)
  MOVS R5, R5, LSL #1    ; logical shift left tempVariable (save carry) (s0)
  ADD  R4, R4, #1      ; numberOfBits = numberOfBits + 1 (s0)
  B   UDIV3          ; branch to UDIV11 (s0)
UDIV4  RSB  R4, R4, #33    ; numberOfBits = 33 - numberOfBits
  ; for i := n - 1 .. 0
  SUB  R4, R4, #1      ; numberOfBits = numberOfBits - 1
  MOV  R6, R4        ; i = numberOfBits - 1
UDIV0  CMP  R6, #0        ; if (i >= 0)
  BLT   UDIV2          ; while (TRUE) do (s1)
  MOV  R3, R3, LSL #1    ; R3 = R3 logical shift left by 1 bit (s1)
  ; remainder(0) = numerator(i)
  MOV  R5, #1        ; tempVariable = 1 (s1)
  MOV  R5, R5, LSL R6    ; logical shift left tempVariable by i (s1)
  AND  R5, R0, R5      ; tempVariable = numerator(i) (s1)
  MOV  R5, R5, LSR R6    ; logical shift right tempVariable bit i, this moves numerator(i) to the least significant bit (s1)
  BIC  R3, R3, #1      ; clear least significant bit of remainder (s1)
  ADD  R3, R3, R5      ; least-significant bit of R = to bit i of the numerator (R(0) = N(i)) (s1)
  ; if remainder >= denominator then
  CMP  R3, R1        ; if (remainder >= denominator) (s1)
  BLO  UDIV1          ; if (TRUE) do (s2) [(s1)]
  SUB  R3, R3, R1      ; remainder = remainder - denominator (s2) [(s1)]
  ; quotient(i) = 1
  MOV  R5, #1        ; tempVariable = 1 (s2) [(s1)]
  MOV  R5, R5, LSL R6    ; logical shift left 1 by i bits (s2) [(s1)]
  BIC  R2, R2, R5      ; bit clear quotient(i) (bit clear bit i of the quotient) (s2) [(s1)]
  ADD  R2, R2, R5      ; quotient(i) = 1 (s2) [(s1)]
UDIV1  SUB  R6, R6, #1      ; i = i - 1 (s1)
  B   UDIV0
UDIV2  MOV  R0, R2        ; R0 = quotient
  MOV  R1, R3        ; R1 = remaider
  POP {R4-R6, PC}  ; pop R2-R6 and return

  END
