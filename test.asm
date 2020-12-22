; vim:ft=nasm

; useful references:
; https://www.cs.uaf.edu/2017/fall/cs301/reference/x86_64.html

; macOS expects position-independent code
; see https://www.nasm.us/doc/nasmdoc7.html#section-7.2
DEFAULT REL

global _main
extern _printf
extern _getline

section .data
  uintformat:
    db "%u", 0
  newline:
    db `\n`, 0
  space:
    db ' ', 0

section .text

; system call cheatsheet
; - system call number + 0x2000000 is passed in rax
; - args in rdi, rsi, rdx
; - use syscall instruction to trigger a syscall
; 
; name | number | signature
; -------------------------
; read | 3      | read(int fd, user_addr_t cbuf, user_size_t nbyte)
; write| 4      | write(int fd, user_addr_t cbuf, user_size_t nbyte)

_main:
            push rbp                  ; push the base pointer onto the stack
            mov rbp, rsp              ; move the stack pointer into the base pointer register

                                      ; save registers that the callee may be relying on
            push rbx
            push r12
            push r13
            push r14
            push r15

                                      ; The stack pointer must be aligned on a 16 byte boundary when
                                      ; calling an external function.  entering main pushed the
                                      ; return address onto the stack (8) we then pushed 6 registers
                                      ; (48) so the stack is at 56, which is not a 16 byte boundary.
                                      ; We extend it by another 8 (64) to get a 16 byte boundary
            sub rsp, 8


                                      ; We're going to read up to 1008 bytes of STDIN to a buffer on
                                      ; the stack, then parse each line of it as a 32 bit decimal
                                      ; integer, overwriting it as we go. We'll then use this array
                                      ; of integers as input to the two calculation functions.

                                      ; Allocate 1024 bytes on the stack
            sub rsp, 1024

                                      ; Read 1008 bytes from STDIN
            mov rax, 0x2000003
            mov rdi, 0
            mov rsi, rsp
            mov rdx, 1008
            syscall

                                      ; TODO: rax contains the number of bytes read. We should use
                                      ; that to make sure we only parse what we've read.

                                      ; Now we parse each line of the input as an integer, writing
                                      ; the result into the same array. Our input consists of 3 or 4
                                      ; digit numbers so the string representation will never be
                                      ; larger than the 32 bit representation. So we can safely do
                                      ; this without tripping over ourselves.

                                      ;                rsp
                                      ;                 |
                                      ; before parsing: 1 2 3 4 , 1 2 3 , 5 6 7 8 , 4 5 6 ,
                                      ; after parsing:  000004d20000007b0000162e000001c8
                                      ;                 |                               | |
                                      ;                rsp                            rbx  rdi

            mov rdi, rsp              ; rdi will point to the next place we parse an integer
            mov rbx, rsp              ; rbx will point to the next place we write an integer
main_parse:
            call read_uint
            cmp rax, 0                ; break if rax is 0 (indicating parsing failed)
            je main_calc

            mov [rbx], eax            ; write rax back to the array

           inc rdi                    ; bump rdi to get past the newline it has stopped on
           add rbx, 4                 ; bump rbx by 4 bytes to get past the integer we wrote
           jmp main_parse             ; repeat

main_calc:
                                      ; now our parsed numbers live in a contiguous chunk of memory
                                      ; starting at rsp ending at the byte before rbx.
                                      ; We pass *start, *end to each calculation function.

            mov rdi, rsp              ; start address of number array
            mov rsi, rbx              ; 'end' address (i.e. address of byte just after last byte)

            call part1                ; calculate the answer to the first part
            mov rsi, rax              ; print it
            call putint

            mov rdi, newline
            call _printf

            mov rdi, rsp
            mov rsi, rbx
            call part2                ; calculate the answer to the second part
            mov rsi, rax              ; print it
            call putint

            add rsp, 1024             ; now restore the stack pointer
            add rsp, 8

main_exit:
                                      ; restore the registers we saved at the start
            pop r15
            pop r14
            pop r13
            pop r12

            pop rbx                   ; restore rbx
            pop rbp                   ; restore the base pointer
            mov rax, 0                ; set exit code to 0
            ret                       ; exit

; this routine expects a 32 bit integer to be in rsi
; it will print it to stdout
; we assume the stack is 16-byte aligned
putint:
            sub rsp, 8                ; re-align the stack, since entering this function will have
                                      ; bumped it by 8 bytes

            mov rdi, uintformat       ; set the format to %u
            call _printf              ; we expect the integer to be in rdi already

            add rsp, 8                ; put the stack back
            ret

; part1(int *start, int *end): returns the product of the two numbers which sum to 2020
; this function doesn't use the stack or any preserved registers so we don't need
; to do the usual saving of registers
part1:
                                      ; we will use rax, rcx and rdx
                                      ; rax and rcx point to numbers in the array
                                      ; rdx will be used as a general scratch register
                                      ; (when we need only half of rdx we refer to it as edx)
                                      ; rdi holds the start of the array
                                      ; rsi holds the end of the array
            mov rax, rdi
part1_1:
            mov rcx, rdi
part1_2:
            mov edx, [rax]
            add edx, [rcx]           ; sum the two numbers together

            cmp edx, 2020            ; if the sum is equal to 2020, jump to the end
            je part1_end

                                      ; increment the inner counter (rcx)
            add rcx, 4

                                      ; check if the inner loop has completed
            cmp rcx, rsi
            jl part1_2

                                      ; increment the outer counter (rax)
            add rax, 4

                                      ; check if the outer loop has completed
            cmp rax, rsi
            jl part1_1
part1_end:
                                      ; rax and rcx will now be pointing to the two
                                      ; numbers that sum to 2020

            mov rax, [rax]
            imul rax, [rcx]           ; multiply them together, store the result in rax

            ret                       ; exit

; part2(int *start, int *end): returns the product of the three numbers which sum to 2020
part2:
                                      ; We'll use rax, rcx and rdx for pointers into the numbers
                                      ; array. r8 will be our scratch register.
                                      ; rdi holds the start of the array
                                      ; rsi holds the end of the array
            mov rax, rdi
part2_1:
            mov rcx, rdi
part2_2:
            mov rdx, rdi
part2_3:
            mov r8d, [rax]
            add r8d, [rcx]
            add r8d, [rdx]
            cmp r8d, 2020
            je part2_end

            add rdx, 4
            cmp rdx, rsi
            jl part2_3
            add rcx, 4
            cmp rcx, rsi
            jl part2_2
            add rax, 4
            cmp rax, rsi
            jl part2_1

part2_end:
            mov rax, [rax]
            imul rax, [rcx]
            imul rax, [rdx]
            ret

; a simplified atoi
; read_uint(char *s) -> uint64
read_uint:
                                      ; things we don't handle: spaces, negative numbers
            mov rax, 0
read_uint_1:
                                      ; rdi is a pointer to the first character
            mov rcx, 0
            mov cl, [rdi]             ; copy the char to cl (last byte of rcx)
            cmp cl, 0                 ; if the char is a null byte, exit
            je read_unit_exit
            sub cl, 0x30              ; determine if the char is a digit: it should be between 0x30 and 0x39
            cmp cl, 0
            jl read_unit_exit         ; < 0: exit
            cmp cl, 9
            jg read_unit_exit         ; > 9: exit

            imul rax, 10              ; multiply the current number by 10
            add rax, rcx              ; add the parsed digit into the units column
            add rdi, 1                ; step to the next character in the string
            jmp read_uint_1           ; repeat

read_unit_exit:
            ret

