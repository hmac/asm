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

                        ; We parse the input buffer into an array of 16 bit ints using
                        ; parse_number_list.
                        ; We will provide the same buffer for input and output, so it gets
                        ; overwritten with the parsed results.
                        ; This is safe provided each line of input has at least one digit.
  mov rdi, rsp
  mov rsi, rsp
  mov rdx, r14
  call _parse_number_list
                        ; rax now holds the number of bytes written
                        ; rsp holds the same buffer as before, but with the parsed results in it.

  mov r9, rsp
  add r9, rax           ; Store the address right after the final number in r9.

                        ; Now we've read to the end of the buffer, and a prefix
                        ; of the buffer contains the input data in an array of
                        ; 16 bit integers.
                        ; r9 holds the address right after the last number.
                        ; Now we want to loop through this array again and count the number of
                        ; increases in the three-element sliding window.
                        ; e.g.

                        ; 1 | 199  A      sum(A) = 199 + 200 + 208 = 607
                        ; 2 | 200  A B    sum(B) = 200 + 208 + 210 = 618 [increase]
                        ; 3 | 208  A B C  sum(C) = 208 + 210 + 200 = 618 [no change]
                        ; 4 | 210    B C
                        ; 5 | 200      C

                        ; We will use the following registers:
  mov r8, rsp           ; r8: current array index, starting at the second element.
  add r8, 2
  mov r10w, 0           ; r10w: current sum
  mov r11w, 0xffff      ; r11w: previous sum (start at MAX_INT)
  mov r12w, 0           ; r12w: increase counter

  sub r9, 2             ; We want to stop when r8 points to the second-last element, so we
                        ; temporarily shift r9 to point to this element, to make the loop comparison
                        ; easier.

_loop2_start:
                        ; We start at index i = 1 (i.e. second element).
  mov r10w, [r8]        ; We'll compute r10 = arr[i-1] + arr[i] + arr[i+1]
  add r10w, [r8-2]
  add r10w, [r8+2]
  cmp r10w, r11w        ; If r10 > r11, r12++
  jbe _loop2_end
  inc r12w
_loop2_end:
  mov r11w, r10w        ; Then set r11 = r10, r8 += 2
  add r8, 2
  cmp r8, r9            ; Break if r8 == r9
  jne _loop2_start
  add r9, 2             ; Reset r9 to what it was


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

;
; int parse_number_list(in *char, out *int16, int len)
; rax                   rdi       rsi         rdx
;
; This procedure converts a buffer containing ASCII numbers separated by
; newlines into a buffer of 16 bit integers. It takes two buffers, the input and
; output. These can be the same buffer, provided the list contains no blank
; lines, since each line will be at least two bytes which is the same size as
; one output integer. The third argument is the length of the input buffer.
;
; The return value is the number of bytes written to the output buffer.
;
; rax                   rdi       rsi         rdx
; int parse_number_list(in *char, out *int16, int len)

; We're going to iterate through the input buffer, accumulating digits and
; converting them from ASCII to integers. For each line in the input, we read
; digits and accumulate a total. Each time we read a digit, we multiply the
; previous total by 10 and add the new digit. e.g.
; "123": 1, 1*10 + 2 = 12, 12 * 10 + 3 = 123

; When we read a newline, we write the total to the output buffer.

; For example, given the buffer "123\n456", we will get:
;    input | 1   2   3   \n  4   5   6
;  (ascii) | 49  50  51  10  52  53  54
;   output | 123 456 3   \n  4   5   6
_parse_number_list:
                        ; rdi: position in input buffer
                        ; rsi: position in output buffer
  mov r10, rdx          ; r10: address of end of input buffer
  add r10, rdi          ;  this is the sum of the input buffer pointer and the buffer length.
  mov ax, 0             ; ax: accumulated total
  mov r8, 0             ; r8b: the current byte we are reading
  mov rcx, rsi          ; rcx: the (saved) position of the output buffer
  mov r9w, 10           ; r9w: a scratch register to help with multiplication by 10

                        ; NOTE: we use ax to hold the total so that we can use a MUL
                        ; instruction without shuffling things into rax and back.

_parse_number_list_loop_start:
  mov r8b, [rdi]        ; Read the current byte
  cmp r8b, 10           ; If it's a newline, go to the newline branch
  je _parse_number_list_newline
  sub r8b, 48           ; Otherwise, subtract 48 to convert it from ASCII to a digit 0-9.
  mul r9w               ; Then multiply our total by 10 and add the digit to it.
  add ax, r8w           ; r8w is the same as r8b - the register sizes for ADD must match.
  jmp _parse_number_list_loop_end

_parse_number_list_newline:
                        ; If the current char is a newline, we want to write the total to the output
                        ; buffer.
  mov [rsi], ax         ; Write the 16 bit int to the buffer
  add rsi, 2            ; Increment the buffer pointer by 2 bytes
  mov ax, 0             ; Reset the total

_parse_number_list_loop_end:
  add rdi, 1            ; Increment the input buffer pointer
  cmp rdi, r10          ; Unless we've reached the end of the buffer, loop
  jne _parse_number_list_loop_start

  mov rax, rsi          ; The number of bytes written to the output buffer is the difference between
  sub rax, rcx          ; the original output pointer (which we saved in rcx) and the current output
                        ; pointer.
  ret
