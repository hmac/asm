; vim:ft=nasm

; useful references:
; https://www.cs.uaf.edu/2017/fall/cs301/reference/x86_64.html

; macOS expects position-independent code
; see https://www.nasm.us/doc/nasmdoc7.html#section-7.2

; system call cheatsheet
; - system call number + 0x2000000 is passed in rax
; - args in rdi, rsi, rdx
; - use syscall instruction to trigger a syscall
; 
; name | number | signature
; -------------------------
; read | 3      | read(int fd, user_addr_t cbuf, user_size_t nbyte)
; write| 4      | write(int fd, user_addr_t cbuf, user_size_t nbyte)

DEFAULT REL

global _main

section .data
                        ; storage for static data
                        ; not used

section .text

_main:
  push rbp              ; save all the registers required by convention
  mov rbp, rsp
  push rbx
  push r12
  push r13
  push r14
  push r15
  sub rsp, 8            ; Not 100%, but I think this is required so that the
                        ; stack is aligned on a 16 byte boundary.
                        ; The act of entering this _main function has pushed the
                        ; previous stack pointer (rbp) on to the stack, so rsp
                        ; is out of alignment by 64 bits (8 bytes).
                        ; Subtracting a further 8 bytes puts it back in
                        ; alignment.

                        ; Now we need to read the numbers from STDIN.
                        ; Each line is either 3 or 4 digits, followed by a
                        ; newline.
                        ; To start let's just read all the input.
                        ; The input file is 2000 lines, with a max of 5 bytes (4
                        ; + newline) per line, for a total of 10 KB.
                        ; We'll make a 65 KB buffer just to be safe.

  sub rsp, 0x10000      ; Allocate 2^16 (65536) bytes on the stack
  
  mov rax, 0x2000003    ; Call read(STDIN, &rsp, 2^16)
  mov rdi, 0
  mov rsi, rsp
  mov rdx, 0x10000
  syscall

                        ; We now have (at most) 2^16 bytes from STDIN at address
                        ; rsp. rax holds the actual number of bytes read, which
                        ; we'll use to ensure we don't process any garbage.
  mov r14, rax

                        ; Now we want to reach each number in the buffer.
                        ; They're currently in ASCII format:
                        ; ASCII | 48 49 50 51 52 53 54 55 56 57 10
                        ; int   | 0  1  2  3  4  5  6  7  8  9  <newline>

                        ; So we iterate through the buffer, reading digits until
                        ; we hit a newline. Each time we read a digit, we
                        ; multiply the previous total by 10 and add the new
                        ; digit. e.g.
                        ; "123": 1, 1*10 + 2 = 12, 12 * 10 + 3 = 123
                        ; When we read a newline, we store the total in the
                        ; original buffer. Since all numbers will fit in a 16
                        ; bit int (2 ASCII bytes), we can overwrite our buffer
                        ; as we go without risk of destroying data we haven't
                        ; yet read.
                        ; For example, given the buffer "123\n456", we will get:
                        ; original | 1   2   3   \n  4   5   6
                        ;  (ascii) | 49  50  51  10  52  53  54
                        ;    after | 123 456 3   \n  4   5   6

                        ; We need to track:
  mov r8, rsp           ; 1. Our read position in the buffer (r8)
  mov r9, rsp           ; 2. Our write position in the buffer (r9)
  mov r10, 0            ; 3. Our accumulated total (r10w)
  mov r11, 0            ; 4. The current byte we're reading (r11b)
  mov r12, rsp          ; 5. The address of the end of the buffer (r12).
                        ;    This is the end of the portion of the buffer that
                        ;    contains data - it might be smaller than the actual
                        ;    buffer if the input file is < 64 butes.
  add r12, rax          ; rax holds the actual number of bytes read by the read
                        ; call.
  mov r13, 0            ; 6. A scratch register to help with multiplication (r13)

_loop_start:
  mov r11b, [r8]        ; Read the value at r8 into r11
  cmp r11b, 10          ; If it's a newline, go to the newline branch
  je _loop_newline
                        ; Otherwise, subtract 48 from r11 to convert it to
                        ; ASCII. Then multiply r10 by 10 and add r11 to it
  sub r11b, 48          
                        ; To multiply r10 by 10, we must move its value to ax
                        ; and then call mul 10, then move ax back to r10.
                        ; TODO: maybe just use ax as the scratch register instead
                        ; of r10?
  mov ax, r10w          
  mov r13, 10
  mul r13
  mov r10w, ax
  add r10w, r11w         ; Now we add r11 to r10 to accumulate the new digit
  jmp _loop_end

_loop_newline:          ; r11 holds a newline
  mov [r9], r10w        ; Write the total to *r9
  add r9, 2             ; Bump r9 by 2 bytes
  mov r10, 0            ; Reset the total

_loop_end:              ; Now we just need to bump the read pointer (r8)
  add r8, 1
  cmp r8, r12           ; If we haven't read to the end of the buffer, loop.
  jne _loop_start

                        ; Now we've read to the end of the buffer, and a prefix
                        ; of the buffer contains the input data in an array of
                        ; 16 bit integers.
                        ; r9 holds the address right after the last number.
                        ; Now we want to loop through this array again and count
                        ; how many times the number increases from the one
                        ; before.
                        ; We will need to track:
  mov r8, rsp           ; 1. Our current index in the array (r8)
  mov r10, 0            ; 2. The previous number we read (r10w)
  mov r11, 0            ; 3. The current number we have read (r11w)
  mov r12, 0            ; 4. The total we are accumulating (r12w)

                        ; When our current index reaches r9, we know we've hit
                        ; the end of the array.
_loop2_start:
  mov r11w, [r8]
  cmp r11w, r10w
  jbe _loop2_end        ; If the current number is > than the previous one,
                        ; increment r12. jbe is unsigned <=.
  add r12w, 1

_loop2_end:
  add r8, 2
  mov r10w, r11w
  cmp r8, r9
  jne _loop2_start      ; If r8 != r9 then we haven't reached the end of the
                        ; array yet, so loop again.

                        ; r12 now holds the answer + 1, since the first
                        ; iteration has the previous number as 0, so we always
                        ; count the first number as being bigger.
  sub r12w, 1           ; Subtract 1 from r12 to get the right answer.
                          
                        ; Now we just print the answer
                        ; We need to write r12w to the stack in order
                        ; to pass it to read.
  sub rsp, 4            ; Make space on the stack for the answer (4 bytes, so
                        ; max number is 9999)
                        ; Now we need to format the answer as ASCII in order to
                        ; print it.
                        ; To do this, we divide the number by 10, and check the
                        ; remainder. e.g.
                        ; 123 / 10 = 12 rem 3 ==> 3
                        ; 12  / 10 = 1  rem 2 ==> 2
                        ; 1   / 10 = 0  rem 1 ==> 1
                        ; 0   / 10 = 0  rem 0 ==> 0

                        ; We start at the end and work backwards, so set our
  mov r8, rsp           ; index (r8) to rsp+3.
  add r8, 3

  mov ax, r12w          ; DIV only works on ax
_loop3_start:
  mov cl, 10
  div cl
  mov cl, ah            ; ah holds the remainder, which is a single digit 0-9.
  add cl, 48            ; Add 48 to convert it to ASCII.
  mov [r8], cl
                          
  sub r8, 1             ; Move to the next byte in the output
                        ; al holds the quotient
                        ; We want ax to hold this number, in order to divide it
                        ; in the next iteration. We can't directly move al to ax
                        ; because they refer to the same register, but we can
                        ; zero out ah (the higher half of ax) to get the same
                        ; effect.
  mov ah, 0
  cmp r8, rsp
  jge _loop3_start      ; Repeat until we've written all 4 bytes
                        ; Now we do the same thing again.

                        ; Now we can write the four bytes to STDOUT
  mov rax, 0x2000004    ; write(STDOUT, &rsp, 4)
  mov rdi, 1
  mov rsi, rsp
  mov rdx, 4
  syscall

  add rsp, 4            ; Pop the output buffer off the stack
  add rsp, 0x10000      ; Pop the 2^16 byte buffer off the stack

_exit:
  add rsp, 8            ; Restore the stack pointer to its original position
  pop r15               ; Restore the registers we saved at the start
  pop r14
  pop r13
  pop r12
  pop rbx
  pop rbp
  mov rax, 0            ; Set the exit code to 0
  ret                   ; Exit.
