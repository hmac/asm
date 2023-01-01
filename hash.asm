; vim:ft=nasm

DEFAULT REL

global _main

section .data
  k: db dword %(0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee, 0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501, 0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be, 0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821, 0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa, 0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8, 0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed, 0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a, 0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c, 0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70, 0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05, 0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665, 0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039, 0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1, 0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1, 0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391)
  s: db %(7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21)
  hex: db %('00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '0a', '0b', '0c', '0d', '0e', '0f', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '1a', '1b', '1c', '1d', '1e', '1f', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '2a', '2b', '2c', '2d', '2e', '2f', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '3a', '3b', '3c', '3d', '3e', '3f', '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', '4a', '4b', '4c', '4d', '4e', '4f', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '5a', '5b', '5c', '5d', '5e', '5f', '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', '6a', '6b', '6c', '6d', '6e', '6f', '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', '7a', '7b', '7c', '7d', '7e', '7f', '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', '8a', '8b', '8c', '8d', '8e', '8f', '90', '91', '92', '93', '94', '95', '96', '97', '98', '99', '9a', '9b', '9c', '9d', '9e', '9f', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6', 'a7', 'a8', 'a9', 'aa', 'ab', 'ac', 'ad', 'ae', 'af', 'b0', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9', 'ba', 'bb', 'bc', 'bd', 'be', 'bf', 'c0', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6', 'c7', 'c8', 'c9', 'ca', 'cb', 'cc', 'cd', 'ce', 'cf', 'd0', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7', 'd8', 'd9', 'da', 'db', 'dc', 'dd', 'de', 'df', 'e0', 'e1', 'e2', 'e3', 'e4', 'e5', 'e6', 'e7', 'e8', 'e9', 'ea', 'eb', 'ec', 'ed', 'ee', 'ef', 'f0', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9', 'fa', 'fb', 'fc', 'fd', 'fe', 'ff')
  newline:
    db `\n`, 0

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
  jmp main

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

main:
  sub rsp, 128          ; Allocate 128 bytes for the input.
                        ; We read in 64 byte chunks, but the last chunk may be padded into two
                        ; chunks so we need space for that.

  ; Initialise the MD5 variables on the stack
  sub rsp, 32
  mov dword [rsp], 0x67452301       ; A
  mov dword [rsp+4], 0xefcdab89     ; B
  mov dword [rsp+8], 0x98badcfe     ; C
  mov dword [rsp+12], 0x10325476    ; D

main_loop_start:
  mov rax, 0            ; Zero out the input array
  mov [rsp+16], rax         
  mov [rsp+24], rax
  mov [rsp+32], rax
  mov [rsp+40], rax
  mov [rsp+48], rax
  mov [rsp+56], rax
  mov [rsp+64], rax
  mov [rsp+72], rax
  mov [rsp+80], rax
  mov [rsp+88], rax
  mov [rsp+96], rax
  mov [rsp+104], rax
  mov [rsp+112], rax
  mov [rsp+120], rax
  mov [rsp+128], rax
  mov [rsp+136], rax

  mov rax, 0x2000003    ; Call read(STDIN, &rsp, 64)
  mov rdi, 0
  lea rsi, [rsp+16]
  mov rdx, 64
  syscall

  lea rdi, [rsp+16]          ; rdi holds the pointer to the input we've read

  ; If we've read less than 64 bytes, this is the last chunk.
  ; We need to pad this chunk.
  cmp rax, 64
  jl main_last_chunk

main_call_md5:
  mov rsi, rdi
  call md5_chunk
  jmp main_loop_start

main_last_chunk:
  ; Apply padding to the last chunk.
  ; This may spill over into a second chunk.
  lea rdi, [rsp+16]
  mov rsi, rax
  call md5_pad

  ; If the new array length is > 64 bytes then the padding has spilled into a second chunk,
  ; so we need to run md5 twice.
  cmp rax, 64
  jle main_last_chunk_single

  mov rsi, rdi
  call md5_chunk
  call md5_chunk
  jmp main_end

main_last_chunk_single:

  mov rsi, rdi
  lea rdx, [rsp+128]
  call md5_chunk

main_end:
  ; Store the MD5 hash
  sub rsp, 16           ; Allocate 16 bytes for the md5 hash.
  mov [rsp], ecx
  mov [rsp+4], r15d
  mov [rsp+8], r8d
  mov [rsp+16], r9d

  mov rdi, rsp
  mov rsi, 16
  call print_hex

  ; Print a newline
  mov rax, 0x2000004    ; write(STDOUT, &newline, 1)
  mov rdi, 1
  lea rsi, newline
  mov rdx, 1
  syscall

  add rsp, 176          ; Restore the stack

  jmp _exit

; md5_pad(input: [u8], len: u64) -> u64
;         rdi          rsi          rax
; Apply padding to the input to prepare it for md5 hashing.
; The capacity of the input array should be a multiple of 64 bytes and should be at least 9 bytes
; greater than the length.
; Any unused capacity in the array should be zeroed out.
; The returned value is the new length of the array.
md5_pad:
  mov rcx, rsi          ; Save the original array length

  ; First, we add the mandatory byte 0x80 to the end of the array. This is required regardless of
  ; the length of the array.
  mov byte [rdi + rsi], 0x80

  ; sil (low byte of rsi) = length % 256
  ; so if we mask off the top two bits, we get length % 64
  mov dl, sil
  and dl, 0x3F ; 0011 1111

  ; we need enough space between the length and the next multiple of 64 to fit our 1 byte of
  ; mandatory padding, followed by the length of the message as a u64 (8 bytes).
  ; So if (length % 64) > 64 - 9, we need to extend it by another 64 bytes.
  ; Otherwise we just need to add our padding.
  cmp dl, 55
  jle md5_pad_padding
  add rsi, 64           ; add 64 bytes
  
md5_pad_padding:
  ; The distance to the next multiple is 64 - (length % 64)
  ; Extend the length by this amount
  neg dl                ; 0 - (length % 64)
  add dl, 64            ; 0 - (length % 64) + 64 = 64 - (length % 64)
  add sil, dl

  ; Now store the (original) array length as a u64 in the last 8 bytes of the array.
  mov [rdi + rsi - 8], rcx

  ; We're done.
  mov rax, rsi
  ret

; Calculate the MD5 hash of a 64 byte chunk, stored on the stack.
; We assume the chunk has been padded if necessary.
; md5_chunk(chunk: [u8; 64], a: u32, b: u32, c: u32, d: u32)
;          rsi               rsp+8   rsp+12  rsp+16  rsp+20
md5_chunk:
; a = ecx
  xor rcx, rcx
  mov ecx, [rsp+8]
; b = r15d
  xor r15, r15
  mov r15d, [rsp+12]
; c = r8d
  xor r8, r8
  mov r8d, [rsp+16]
; d = r9d
  xor r9, r9
  mov r9d, [rsp+20]

; TODO: the loop below can be structured better:
; for i in 0..=15:
;   ...
; for i in 16..=31:
;   ...
; for i in 32..=47:
;   ...
; for i in 48..=63:
;   ...

; for i in 0 to 63:
; r10 = i
  mov r10, 0
md5_chunk_loop_start:
  ; if i >= 48:
  cmp r10d, 48
  jge md5_chunk_i_63
  ; if i >= 32:
  cmp r10d, 32
  jge md5_chunk_i_47
  ; if i >= 16:
  cmp r10d, 16
  jge md5_chunk_i_31

  ; otherwise:
md5_chunk_i_15:
  ; F = (b and c) or ((not b) and d)
  ; r12 = not b
  mov r12d, r15d
  not r12d
  ; r12 = r12 and d
  and r12d, r9d
  ; r14 = b and c
  mov r14d, r15d
  and r14d, r8d
  ; r12 = r14 or r12
  or r12d, r14d
  ; g = i
  mov r13d, r10d
  jmp md5_chunk_loop_final

md5_chunk_i_31:
  ; F = (d and b) or ((not d) and c)
  ; r12 = not d
  mov r12d, r9d
  not r12d
  ; r12 = r12 and c
  and r12d, r8d
  ; r14 = d and b
  mov r14d, r9d
  and r14d, r15d
  ; r12 = r12 or r14
  or r12d, r14d
  ; g = (5*i + 1) mod 16
  ; r13 = 5 * i
  mov r13d, r10d
  mov eax, 5
  mul r13d
  ; r13 = r13 + 1
  inc r13d
  ; r13 = r13 mod 16
  and r13d, 0xf
  jmp md5_chunk_loop_final

md5_chunk_i_47:
  ; F = b xor c xor d
  ; r12 = c xor d
  mov r12d, r8d
  xor r12d, r9d
  ; r12 = r12 xor b
  xor r12d, r15d
  ; g = (3*i + 5) mod 16
  ; r13 = 3 * i
  mov r13d, r10d
  mov eax, 3
  mul r13d
  ; r13 = r13 + 5
  add r13d, 5
  ; r13 = r13 mod 16 (equiv to keeping the lowest 4 bits)
  and r13d, 0xf
  jmp md5_chunk_loop_final

; 48 <= i <= 63
md5_chunk_i_63:
  ; F = c xor (b or (not d))
  ; r12 = not d
  mov r12d, r9d
  not r12d
  ; r12 = r12 or b
  or r12d, r15d
  ; r12 = r12 xor c
  xor r12d, r8d
  ; g = i
  mov r13d, r10d
  jmp md5_chunk_loop_final

md5_chunk_loop_final:
  ; F = F + A + K[i] + M[g] (M[g] is the gth 32-bit word in the chunk)
  ; A = D
  ; D = C
  ; C = B
  ; B = B + leftrotate(F, s[i])

  ; r11 = M[g]
  mov r11d, [4*r13 + rsi] ; r11 = M[4*g]

  ; rdi = K[i]
  mov rdi, k
  add rdi, r10
  mov edi, [rdi]

  ; r11 = r11 + edi
  add r11d, edi
  ; r11 = r11 + A
  add r11d, ecx
  ; r11 = r11 + F
  add r11d, r12d

  ; A = D
  mov ecx, r9d
  ; D = C
  mov r9d, r8d
  ; C = B
  mov r8d, r15d

  ; B = B + leftrotate(F, s[i])
  ; cl = s[i]
  push rcx
  mov rdi, s
  add rdi, r10
  mov cl, [rdi]
  ; F = leftrotate(F, s[i])
  rol r12d, cl ; second arg must be in cl register (or immediate)
  pop rcx
  ; B = B + r12d
  add r15d, r12d

  inc r10d
  cmp r10d, 64
  jl md5_chunk_loop_start

md5_chunk_loop_end:
  ; a = a + A
  ; b = b + B
  ; c = c + C
  ; d = d + D
  add [rsp+8], ecx
  add [rsp+12], r15d
  add [rsp+16], r8d
  add [rsp+20], r9d
  
  ret

; void print_hex(char: [u8], len: u64)
;                rdi         rsi
print_hex:
; Save the position of the last char
  mov r8, rdi
  add r8, rsi
; Reserve stack space for the result, which is twice the size of the input
  sub rsp, rsi
  sub rsp, rsi
; Save a pointer to the output array in r9
  mov r9, rsp
; Now increment rdi until it reaches r8
; For each byte, look up the corresponding hex digit (two bytes)
; Write these two bytes to the output array and increment r9

print_hex_loop_start:
  mov r10, 0
  mov r11b, [rdi]
  add r10b, r11b
  add r10, r10
  mov r11, hex          ; Store the address of the hex digit in r10
  add r10, r11
  mov r10w, [r10]       ; Store the hex digit bytes in r10
  mov [r9], r10w        ; Move the lower 2 bytes (first 2 bytes) to the output
  add r9, 2
  inc rdi
  cmp rdi, r8
  jl print_hex_loop_start

print_hex_loop_end:
  push rsi
  mov rdx, rsi          ; Set rdx to the output length in bytes (twice the input length)
  add rdx, rdx

  ; Print the output array
  mov rax, 0x2000004    ; write(STDOUT, &rsp, rdx)
  mov rdi, 1
  lea rsi, [rsp + 8]    ; the `push rsi` on L327 means the output array is offset from rsp
  syscall

  ; Clean up and exit
  pop rsi
  lea rsp, [rsp + 2*rsi]
  ret
