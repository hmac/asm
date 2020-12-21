; vim:ft=nasm

; useful references:
; https://www.cs.uaf.edu/2017/fall/cs301/reference/x86_64.html

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
  numbers:
    db dword %(1975, 1600, 113, 1773, 1782, 1680, 1386, 1682, 1991, 1640, 1760, 1236, 1159, 1259, 1279, 1739, 1826, 1888, 1072, 416, 1632, 1656, 1273, 1631, 1079, 1807, 1292, 1128, 1841, 1915, 1619, 1230, 1950, 1627, 1966, 774, 1425, 1983, 1616, 1633, 1559, 1925, 960, 1407, 1708, 1211, 1666, 1910, 1960, 1125, 1242, 1884, 1829, 1881, 1585, 1731, 1753, 1784, 1095, 1267, 1756, 1226, 1107, 1664, 1710, 2000, 1181, 1997, 1607, 1889, 1613, 1859, 1479, 1763, 1692, 1967, 522, 1719, 1816, 1714, 1331, 1976, 1160, 1899, 1906, 1783, 1061, 2006, 1993, 1717, 2009, 1563, 1733, 1866, 1651, 1437, 1517, 1113, 1743, 1240, 1629, 1868, 1912, 1296, 1873, 1673, 1996, 1814, 1215, 1927, 1956, 1970, 1887, 1702, 1495, 1754, 1621, 1055, 1538, 1693, 1840, 1685, 1752, 1933, 1727, 1648, 1792, 1734, 1305, 1446, 1764, 1890, 1904, 1560, 1698, 1645, 1214, 1516, 1064, 1729, 1835, 1642, 1932, 1683, 962, 1081, 1943, 1502, 1622, 196, 1972, 1916, 1850, 1205, 1971, 1937, 1575, 1401, 1351, 2005, 1917, 1670, 1388, 1051, 1941, 1751, 1169, 510, 217, 1948, 1120, 1635, 1636, 1511, 1691, 1589, 1410, 1902, 1572, 1871, 1423, 1114, 1806, 1282, 1193, 1974, 388, 1398, 1992, 1263, 1786, 1723, 1206, 1363, 1177, 1646, 1231, 1140, 1088, 1322)


section .text

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

            call part1                ; calculate the answer to the first part
            mov rsi, rax              ; print it
            call putint

            mov rdi, newline
            call _printf

            call part2                ; calculate the answer to the second part
            mov rsi, rax              ; print it
            call putint

            add rsp, 8                ; now restore the stack pointer

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
            push rbp                  ; push the base pointer onto the stack
            mov rbp, rsp              ; move the stack pointer into the base pointer register

                                      ; the x86-64 calling convention is to pass the first six
                                      ; arguments in the registers rdi, rsi, rdx, rcx, r8, r9.

                                      ; the first arg to printf is the format string "%u", which
                                      ; we've stored in the data section under the label uintformat.
            lea rdi, [uintformat]
                                      ; the second arg is the number to print, which is already in
                                      ; rsi.
            call _printf

            pop rbp
            ret

; part1(): returns the product of the two numbers which sum to 2020
; this function doesn't use the stack or any preserved registers so we don't need
; to do the usual saving of registers
part1:
                                      ; we will use rax, rcx and rdx
                                      ; rax and rcx point to numbers in the array
                                      ; rdx will be used as a general scratch register
                                      ; (when we need only half of rdx we refer to it as edx)
            mov rax, numbers
part1_1:
            mov rcx, numbers
part1_2:
            mov edx, [rax]
            add edx, [rcx]           ; sum the two numbers together

            cmp edx, 2020            ; if the sum is equal to 2020, jump to the end
            je part1_end

                                      ; increment the inner counter (rcx)
            add rcx, 4

                                      ; check if the inner loop has completed
            lea rdx, [numbers]
            add rdx, (200*4)
            cmp rcx, rdx
            jne part1_2

                                      ; increment the outer counter (rax)
            add rax, 4

                                      ; check if the outer loop has completed
            mov rdx, numbers
            add rdx, (200*4)
            cmp rax, rdx
            jne part1_1
part1_end:
                                      ; rax and rcx will now be pointing to the two
                                      ; numbers that sum to 2020

            mov rax, [rax]
            imul rax, [rcx]           ; multiply them together, store the result in rax

            ret                       ; exit

; part2(): returns the product of the three numbers which sum to 2020
part2:
                                      ; We'll use rax, rcx and rdx for pointers into the numbers
                                      ; array. rsi will be our scratch register.
                                      ; r8 will hold a pointer to the end of the array
            mov r8, numbers + (200*4)
            mov rax, numbers
part2_1:
            mov rcx, numbers
part2_2:
            mov rdx, numbers
part2_3:
            mov esi, [rax]
            add esi, [rcx]
            add esi, [rdx]
            cmp esi, 2020
            je part2_end

            add rdx, 4
            cmp rdx, r8
            jne part2_3
            add rcx, 4
            cmp rcx, r8
            jne part2_2
            add rax, 4
            cmp rax, r8
            jne part2_1

part2_end:
            mov rax, [rax]
            imul rax, [rcx]
            imul rax, [rdx]
            ret
