; vim:ft=nasm
; macOS expects position-independent code
; see https://www.nasm.us/doc/nasmdoc7.html#section-7.2
DEFAULT REL

global _main
extern _printf

section .data
  uintformat:
    db "%u", 0
  newline:
    db `\n`, 0
  space:
    db ' ', 0
  hello:
    db "hello", 10, 0
  numbers:
    db dword %(1975, 1600, 113, 1773, 1782, 1680, 1386, 1682, 1991, 1640, 1760, 1236, 1159, 1259, 1279, 1739, 1826, 1888, 1072, 416, 1632, 1656, 1273, 1631, 1079, 1807, 1292, 1128, 1841, 1915, 1619, 1230, 1950, 1627, 1966, 774, 1425, 1983, 1616, 1633, 1559, 1925, 960, 1407, 1708, 1211, 1666, 1910, 1960, 1125, 1242, 1884, 1829, 1881, 1585, 1731, 1753, 1784, 1095, 1267, 1756, 1226, 1107, 1664, 1710, 2000, 1181, 1997, 1607, 1889, 1613, 1859, 1479, 1763, 1692, 1967, 522, 1719, 1816, 1714, 1331, 1976, 1160, 1899, 1906, 1783, 1061, 2006, 1993, 1717, 2009, 1563, 1733, 1866, 1651, 1437, 1517, 1113, 1743, 1240, 1629, 1868, 1912, 1296, 1873, 1673, 1996, 1814, 1215, 1927, 1956, 1970, 1887, 1702, 1495, 1754, 1621, 1055, 1538, 1693, 1840, 1685, 1752, 1933, 1727, 1648, 1792, 1734, 1305, 1446, 1764, 1890, 1904, 1560, 1698, 1645, 1214, 1516, 1064, 1729, 1835, 1642, 1932, 1683, 962, 1081, 1943, 1502, 1622, 196, 1972, 1916, 1850, 1205, 1971, 1937, 1575, 1401, 1351, 2005, 1917, 1670, 1388, 1051, 1941, 1751, 1169, 510, 217, 1948, 1120, 1635, 1636, 1511, 1691, 1589, 1410, 1902, 1572, 1871, 1423, 1114, 1806, 1282, 1193, 1974, 388, 1398, 1992, 1263, 1786, 1723, 1206, 1363, 1177, 1646, 1231, 1140, 1088, 1322)


section .text

_main:
    push rbp                  ; push the base pointer onto the stack
    mov rbp, rsp              ; move the stack pointer into the base pointer register
    push rbx                  ; save registers that the callee may be relying on
    push rdi
    push rsi

    sub rsp, 8                ; the stack pointer must be aligned on a 16 byte
                              ; boundary when calling an external function.
                              ; entering main pushed the return address onto
                              ; the stack (8) we then pushed rbp, rbx, rdi and
                              ; rsi (32) so the stack is at 40, which is not a
                              ; 16 byte boundary we extend it by another 8 (48)
                              ; to get a 16 byte boundary

                              ; a loop that assembles each pair of numbers from
                              ; a list how do we represent the list?  put each
                              ; number on the stack

                              ; we have two pointers into an array of numbers
                              ; in the data segment called 'numbers'.  we have
                              ; two loops, one for each pointer, from the start
                              ; of the array to the end the numbers are 64 bit,
                              ; so at the end of each loop we increment the
                              ; pointer by 8 on each inner loop we check if the
                              ; numbers sum to 2020 if they do, we multiply
                              ; them together and jump

                              ; we use rbx and r12 for the outer and inner loop
                              ; pointers, respectively
    lea rbx, [numbers]
main_loop1:

    lea r12, [numbers]
main_loop2:


    mov r15d, 0                ; sum the two numbers together
    add r15d, [rbx]
    add r15d, [r12]

    ;mov rsi, [rbx]
    ;call putint

    ;mov rdi, space
    ;call _printf

    ;mov rsi, [r12]
    ;call putint

    ;mov rdi, space
    ;call _printf

    ;push r15
    ;mov rsi, [rsp]
    ;sub rsp, 8
    ;call putint
    ;add rsp, 8
    ;pop r15

    ;mov rdi, newline
    ;call _printf

    ;mov rsi, r15
    ;call putint
    cmp r15d, 2020            ; if the sum is equal to 2020, jump to the end
    je main_end

                              ; increment the inner counter (r12)
    add r12, 4

                              ; check if the inner loop has completed
    lea r15, [numbers]
    add r15, (200*4)
    cmp r12, r15
    jne main_loop2

                              ; increment the outer counter (rbx)
    add rbx, 4

                              ; check if the outer loop has completed
    mov r15, numbers
    add r15, (200*4)
    cmp rbx, r15
    jne main_loop1

main_end:
                              ; assume that rbx and r12 point to the indices of
                              ; the answer

    mov rsi, [rbx]            ; print the first number (rbx)
    call putint
    mov rdi, newline
    call _printf
    mov rsi, [r12]            ; print the second number (r12)
    call putint
    mov rdi, newline
    call _printf

    mov r13, [rbx]            ; multiply them together
    imul r13, [r12]

    mov rsi, r13              ; print the product
    call putint

    mov rax, r13


                              ; now restore the stack pointer
    add rsp, 8

                              ; restore the saved registers by popping them from the stack
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
    push rbp                  ; push the base pointer onto the stack
    mov rbp, rsp              ; move the stack pointer into the base pointer register

                              ; we need to tell printf to print an integer with the %d format string
                              ; the amd64 calling convention is to pass the first six integer or pointer
                              ; arguments in registers rdi, rsi, rdx, rcx, r8 and r9

                              ; we just need to use rdi and rsi
                              ; we want rdi="%u", rsi=<number>
                              ; our number is already in rsi, so we just leave it there

                              ; our format string is stored under the label uintformat so we put that
                              ; address in rdi
    lea rdi, [uintformat]

    call _printf

    pop rbp
    ret

