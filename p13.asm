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
  ids:
    db word %(29,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,37,0,0,0,0,0,409,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,17,13,19,0,0,0,23,0,0,0,0,0,0,0,353,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41)
  id_length equ $-ids

section .text

_main:
            push rbp
            mov rbp, rsp
            push rbx
            push r12
            push r13
            push r14
            push r15
            sub rsp, 8

                                      ; we want to find the smallest number n such that
                                      ; bus 29 departs at time n, bus 37 departs at time n + 23,
                                      ; etc.
                                      ; we should first convert the data into a better format:
                                      ; an array of pairs of (bus id, offset)
                                      ; e.g. [(29, 0),(37, 23), ..]
                                      ; there are eight bus IDs in the input and all are < 1024 so
                                      ; we just need 8 * (2 + 2) = 32 bytes
            sub rsp, 32
                                      ; loop over each id
            mov rcx, ids              ; rcx is the pointer to the id
            mov r8, ids+id_length     ; r8 is the end of the array
            mov rdi, rsp              ; rdi is the pointer to the array we're writing
main_1:
            cmp rcx, r8               ; if at the end of the array, break
            je main_3
            mov ax, [rcx]             ; ax is the id we're currently on
            cmp ax, 0                 ; if 0, skip
            je main_2
            mov [rdi], ax             ; write id into our array
            add rdi, 2
            mov r9, rcx               ; write offset into our array
            mov r10, ids              ; offset = (ids - rcx) / 2
            sub r9, r10
            shr r9, 1
            mov [rdi], r9w
            add rdi, 2
main_2:
            add rcx, 2                ; increment offset
            jmp main_1                ; repeat
main_3:
                                      ; now we have an array of (id, offset) pairs
                                      ; we want to test each number from 0..
                                      ; for each number n, and pair (i, o), we test
                                      ; if i divides n + o.
                                      ; the answer is the n for which all pairs satisy this test.
                                      ; first, let's set up the two loops
                                      ; the first loop will be over all numbers
                                      ; r11 will count upwards from (arbitrarily) 2^16 + 1
                                      ; (the answer is likely to be much larger)
            mov r11, 0xffff
            lea rcx, [rsp + 32]       ; rcx points to the end of the array of pairs
main_4:     inc r11

            ;mov rsi, r11              ; print n
            ;mov rdi, uintformat
            ;call _printf
                                      ; now we loop through each pair
            mov r10, rsp              ; r10 is the pointer to the pair
main_5:     mov rax, 0                ; rax = n + o
            mov ax, [r10+2]
            add rax, r11
            mov r8, 0                 ; r8 is i
            mov r8w, [r10]
            mov rdx, 0                ; idiv R divides the number rdx:rax by R. 64 bits is plenty for us so
                                      ; we zero out the upper half (rdx)
            idiv r8
            cmp rdx, 0                ; the remainder of the division is in rdx
            jne  main_4               ; if the remainder is non-zero, this number is wrong
            add r10, 4                ; move to the next pair
            cmp r10, rcx              ; if we have more pairs left, loop
            jl main_5
                                      ; otherwise, this number (r11) is the solution!
main_6:     mov rsi, r11
            mov rdi, uintformat
            mov rax, r11
            call _printf
main_exit:
            add rsp, 8
            pop r15
            pop r14
            pop r13
            pop r12

            mov rax, 0                ; set exit code to 0

            pop rbx
            pop rbp
            ret
