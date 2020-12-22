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

_main:
            push rbp
            mov rbp, rsp
            push rbx
            push r12
            push r13
            push r14
            push r15
            sub rsp, 8

                                      ; do work here

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
