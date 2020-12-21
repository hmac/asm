; macOS expects position-independent code
; see https://www.nasm.us/doc/nasmdoc7.html#section-7.2
DEFAULT REL

global _main

section .data
  hello:
    db "hello", 10, 0
  uintformat:
    db "%lu", 0
  newline:
    db `\n`, 0
  numbers:
    db qword 1721d, 979d, 366d, 299d, 675d, 1456d

section .text

extern _printf

  _main:
    ; C calling convention
    push rbp     ; push the base pointer onto the stack
    mov rbp, rsp ; move the stack pointer into the base pointer register
    ; save registers that the callee may be relying on
    push rbx
    push rdi
    push rsi

    ; the stack pointer must be aligned on a 16 byte boundary when calling
    ; an external function.
    ; entering main pushed the return address onto the stack (8)
    ; we then pushed rbp, rbx, rdi and rsi (32)
    ; so the stack is at 40, which is not a 16 byte boundary
    ; we extend it by another 8 (48) to get a 16 byte boundary
    sub rsp, 8

    ; a loop that assembles each pair of numbers from a list
    ; how do we represent the list?
    ; put each number on the stack

    ; we have two pointers into an array of numbers in the data segment called 'numbers'.
    ; we have two loops, one for each pointer, from the start of the array to the end
    ; the numbers are 64 bit, so at the end of each loop we increment the pointer by 8
    ; on each inner loop we check if the numbers sum to 2020
    ; if they do, we multiply them together and jump

    ; we use rbx and r12 for the outer and inner loop pointers, respectively
    lea rbx, [numbers]
loop1:

    lea r12, [numbers]
loop2:
    ; TODO: check this pair of numbers

    ; here's where we do the work
    ; look up each number

    mov rsi, [rbx]
    call putint

    mov rdi, newline
    call _printf

    mov rsi, [r12]
    call putint

    mov rdi, newline
    call _printf

    ; check if these two numbers sum to 2020
    mov r15, 0
    add r15, [rbx]
    add r15, [r12]
    ;mov rsi, r15
    ;call putint
    cmp r15, 2020
    je end

    ; increment the inner counter (r12)
    add r12, 8

    ; check if the inner loop has completed
    mov r15, numbers
    add r15, (6*8)
    cmp r12, r15
    jne loop2

    ; increment the outer counter (rbx)
    add rbx, 8

    ; check if the outer loop has completed
    mov r15, numbers
    add r15, (6*8)
    cmp rbx, r15
    jne loop1

end:
    ; assume that rbx and r12 point to the indices of the answer

    ;mov rsi, [rbx]
    ;call putint
    ;mov rsi, [r12]
    ;call putint

    ; multiply them together
    mov r13, [rbx]
    imul r13, [r12]

    mov rsi, r13
    ;call putint

    mov rax, r13


    ; now restore the stack pointer
    add rsp, 8

    ; restore the saved registers by popping them from the stack
    ; I think this automatically puts the value at the stack address back into
    ; the register and then decrements the register
    pop rsi
    pop rdi
    pop rbx
    ; restore the base pointer
    pop rbp
    ; finally, return
    ret

  ; this routine expects a 64 bit integer to be in rsi
  ; it will print it to stdout
  ; we assume the stack is 16-byte aligned
  putint:
    ; C calling convention
    push rbp     ; push the base pointer onto the stack
    mov rbp, rsp ; move the stack pointer into the base pointer register

    ; we need to tell printf to print an integer with the %d format string
    ; the amd64 calling convention is to pass the first six integer or pointer
    ; arguments in registers rdi, rsi, rdx, rcx, r8 and r9

    ; we just need to use rdi and rsi
    ; we want rdi="%u", rsi=<number>
    ; our number is already in rsi, so we just leave it there

    ; our format string is stored under the label uintformat so we put that
    ; address in rdi
    lea rdi, [uintformat]

    ; add rsp, 8
    call _printf
    ; sub rsp, 8

    pop rbp
    ret

