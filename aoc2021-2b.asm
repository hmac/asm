; vim:ft=nasm

DEFAULT REL

global _main

section .text

_main:
  push rbp              ; save all the registers required by convention
  mov rbp, rsp
  push rbx
  push r12
  push r13
  push r14
  push r15
  sub rsp, 8            ; Align the stack at 16 bytes

  sub rsp, 0x10000      ; Allocate 2^16 (65536) bytes on the stack
  
  mov rax, 0x2000003    ; Call read(STDIN, &rsp, 2^16)
  mov rdi, 0
  mov rsi, rsp
  mov rdx, 0x10000
  syscall

                        ; Parse the instructions
  mov rdi, rsp
  mov rsi, rax
  call _sum_instructions; rax now holds the product of position and depth.
  mov rdi, rax
  call _print_number

_exit:
  add rsp, (8 + 0x10000); Restore the stack pointer to its original position
  pop r15               ; Restore the registers we saved at the start
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  mov rax, 0            ; Set the exit code to 0
  ret                   ; Exit.

; void parse_instructions(u8* input, u64 len)
; rax                    rdi        rsi
; This function takes a pointer to an ASCII buffer containing a series of
; submarine instructions, and the length of the buffer.
; It parses the instructions sums the magnitudes for each instruction type.
; It returns the product of the final depth and position values.
_sum_instructions:
                        ; rdi: pointer to the input buffer
  mov r9, rsi           ; r9: pointer to the end of the input buffer
  add r9, rdi
  mov rsi, 0            ; rsi: the accumulating position
  mov rdx, 0            ; rdx: the accumulating depth
  mov rcx, 0            ; rcx: the accumulating aim
_parse_instructions_loop_start:
_break:
  call _parse_instruction
  cmp rdi, r9
  jne _parse_instructions_loop_start
  mov rax, rsi
  mul rdx
  ret


; void parse_instruction(u8* input, u64 position, u64 depth, u64 aim)
; rax                    rdi        rsi           rdx        rcx
; This function takes a pointer to an ASCII buffer containing a single submarine
; instruction, and three integers, position, depth and aim. It modifies the
; integers according to the instruction.
; Instructions have the grammar (forward|down|up) [0-9].
; "down X" increases the aim by X
; "up X" decreases the aim by X
; "forward X" increases the position by X
;             and increases the depth by aim * X.
; This function advances the input pointer so that it points to the next
; instruction, if there is one.
_parse_instruction:
  mov r8, 0
  mov r8b, [rdi]
  cmp r8b, 10           ; If the first letter is a newline, skip it
  je _parse_instruction_newline
  cmp r8b, 100          ; If the first letter is 'd', handle that
  je _parse_instruction_down
  cmp r8b, 102          ; If the first letter is 'f', handle that
  je _parse_instruction_forward
                        ; Otherwise, assume the instruction is "up"
  mov r8b, [rdi+3]      ; Set the value byte to the ASCII digit
  sub r8b, 48           ; Convert the value byte from ASCII to an integer
  sub rcx, r8           ; Subtract this value from the aim
  add rdi, 5            ; Advance the input buffer to consume this instruction
  ret
_parse_instruction_down:
  mov r8b, [rdi+5]      ; Set the value byte to the ASCII digit
  sub r8b, 48           ; Convert the value byte from ASCII to an integer
  add rcx, r8           ; Add this value to the aim
  add rdi, 7            ; Advance the input buffer
  ret
_parse_instruction_forward:
  mov r8b, [rdi+8]      ; Set the value byte to the ASCII digit
  sub r8b, 48           ; Convert the value byte from ASCII to an integer
                        ; Calculate value * aim, to get the depth increase:
  mov rax, 0
  mov al, r8b           ;   rax <- value
  mov r10, rdx          ; Save rdx, since MUL overwrites it
  mul rcx               ;   rax <- rax * aim
  mov rdx, r10          ; Restore rdx
  add rdx, rax          ; Add the depth change to the depth total
  add rsi, r8           ; Add the position change to the position total
  add rdi, 10           ; Advance the input buffer
  ret
_parse_instruction_newline:
  add rdi, 1            ; Skip empty lines
  ret



; void print_number(u32 n)
;                   rdi
_print_number:
                        ; rdi holds the number
                        ; We will write the result to the stack, so reserve
                        ; 10 bytes, which can hold any 32 bit integer in ASCII
                        ; decimal format.
  sub rsp, 10
                        ; Now we need to format the answer as ASCII in order to
                        ; print it.
                        ; To do this, we divide the number by 10, and check the
                        ; remainder. e.g.
                        ; 123 / 10 = 12 rem 3 ==> 3
                        ; 12  / 10 = 1  rem 2 ==> 2
                        ; 1   / 10 = 0  rem 1 ==> 1
                        ; 0   / 10 = 0  rem 0 ==> 0
 
                        ; We start at the end and work backwards, so set our
   mov r8, rsp          ; index (r8) to rsp+9.
   add r8, 9
   mov eax, edi         ; 32-bit DIV works on edx:eax as the numerator
                        ; it stores the quotient in eax
                        ; and the remainder in edx
                        ; Our numerator is always 32 bits so we can just leave
                        ; it in eax.
_print_number_loop_start:
  mov edx, 0            ; Ensure the high 32 bits of the numerator are 0
  mov ecx, 10           ; ecx holds the denominator of our division (10)
  div ecx
  mov cl, dl            ; edx holds the remainder, which is a single digit 0-9.
  add cl, 48            ; Add 48 to convert it to ASCII.
  mov [r8], cl
                          
  sub r8, 1             ; Move to the next byte in the output
                        ; eax holds the quotient
  cmp r8, rsp
                        ; Repeat until we've written all 10 bytes
  jge _print_number_loop_start

                        ; Now we can write the 10 bytes to STDOUT
  mov rax, 0x2000004    ; write(STDOUT, &rsp, 10)
  mov rdi, 1
  mov rsi, rsp
  mov rdx, 10
  syscall
  add rsp, 10           ; Clean up the stack
  ret
