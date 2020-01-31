  AREA  AsmTemplate, CODE, READONLY
  IMPORT  main

; sample program makes the 4 LEDs P1.16, P1.17, P1.18, P1.19 go on and off in sequence
; (c) Mike Brady, 2011 -- 2019.

  EXPORT  start
start

IO1DIR  EQU  0xE0028018
IO1SET  EQU  0xE0028014
IO1CLR  EQU  0xE002801C
NEGCODE EQU 0xb

  ldr  r1,=IO1DIR
  ldr  r2,=0x000f0000  ;select P1.19--P1.16
  str  r2,[r1]    ;make them outputs
  ldr  r1,=IO1SET
  str  r2,[r1]    ;set them to turn the LEDs off
  ldr  r2,=IO1CLR
; r1 points to the SET register
; r2 points to the CLEAR register

  ldr r3, =0x00000419         ; num = 1049
  ldr r9, =0x000f0000         ; reset_mask = 0x000f0000
whileT                        ; while(true)
                              ; {
  cmp r3, #0                  ;   if (num < 0)
  bge eif2                    ;   {
  mov r5, #NEGCODE            ;     neg_code = 0b1011
  mov r5, r5, lsl #4          ;     neg_code <<= 4
  ldr  r5,=4000000            ;     delay(500ms)
eif2					      ;   }
dloop0  subs  r5,r5,#1
  bne  dloop0
  ldr r9, [r1]                ;     turn_off_led()
                              ;   }
  mov r4, #3                  ;   i = 3
whilei
  cmp r4, #0                  ;   while (i >= 0)
  blt eWhilei                 ;   {
  mov r5, r4, lsl #2          ;     num_shift = i * 4
  mov r5, r3, lsr r5          ;     num_shifted = num >> num_shift
  mov r6, #0xf                ;     mask = 0xf
  and r5, r5, r6              ;     num_shifted &= mask
  cmp r5, #0                  ;     if (num_shift == 0)
  bne el0                     ;     {
  str r9, [r2]                ;        show_zero()
  b eif0                      ;      }
el0                           ;      else
                              ;     {
  ldr r6, =0x00100000         ;       led_mask = 0x00100000
  ldr r7, =0x00010000         ;       led_bit = 0x00010000 (P1.16)
  mov r8, #1                  ;       mask = 1
whileLed
  cmp r7, r6                  ;       while (led_bit != led_mask)
  beq eWhileLed               ;       {
  and r10, r5, r8             ;         bit &= mask
  cmp r10, #0                 ;         if (bit != 0)
  bne eif1                    ;         {
  str r7, [r2]                ;           turn_on_led(led_bit)
eif1                          ;         }
  mov r8, r8, lsl #1          ;         mask <<= 1
  mov r7, r7, lsl #1          ;         led_bit <<= 1
  b whileLed                  ;       }
eWhileLed
eif0                          ;      }

  ldr  r5,=4000000            ;     delay(500ms)
dloop1  subs  r5,r5,#1
  bne  dloop1  
  ldr r9, [r1]                ;     turn_off_led()
  sub r4, r4, #1              ;     i--
  b whilei                    ;   }
eWhilei
  ldr  r5,=4000000            ;     delay(500ms)
dloop2  subs  r5,r5,#1
  bne  dloop2
  b whileT                    ; }

  END
