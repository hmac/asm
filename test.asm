; macOS expects position-independent code
default rel

global _main

section .data
  hello:
    db "hello", 10, 0
  uintformat:
    db "%u", 0
  newline:
    db "\n", 0

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
    lea rdi, [rel hello]
    call _printf

    ; now restore the stack pointer
    add rsp, 8

    ; let's calculate the 10th fibonacci number
    ; we need five registers
    ; these are picked because they're preserved by any called functions
    ; rbx = n-1
    ; r12 = n
    ; r14 = scratch
    ; r15 = counter
    ; rbx = 0, r12 = 1, r15 = 0
    mov rbx, 0
    mov r12, 1
    mov r15, 0

    ; rbx | r12 | r14 | r15
    ;
    ;  0  |  1  |  -  |  1
    ;  1  |  1  |  1  |  2
    ;  1  |  2  |  1  |  3
    ;  2  |  3  |  2  |  4
    ;  3  |  5  |  3  |  5
    ;  5  |  8  |  5  |  6
    ;     |     |     |   
    ;  1  |  1  |  1  |  3
    ;  1  |  1  |  1  |  4
    ;     |     |  2  |   
    ;     |     |     |   

    ; set r14 = r12
    ; set r12 = r12 + rbx
    ; set rbx = r14
    ; increment r15
_0: mov r14, r12
    add r12, rbx
    mov rbx, r14
    add r15, 1

    ; if r15 is less than 30, jump to the start of the loop
    cmp r15, 30
    jl _0

    ; print r14
    mov rsi, r14
    call putint

    mov rax, r13

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
    lea rdi, [rel uintformat]

    add rsp, 8
    call _printf
    sub rsp, 8

    pop rbp
    ret

