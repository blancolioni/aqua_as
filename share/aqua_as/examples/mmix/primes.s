Count = 500
t = %255
n global 0   ; 254
q global 0   ; 253
r global 0   ; 252
jj global 0  ; 251
kk global 0  ; 250
pk global 0  ; 249
mm = kk

prime1: word 2
@ = @ + 4 * Count
ptop global @             ; 248
j0 global prime1 + 4 - @      ; 247
buf:  word 0, 0

main:
        set n, 3
        set jj, j0
2
        st n, ptop, jj
        incl jj, 4
3       bz jj, 2f
4       incl n, 2
5       set kk, j0
6       ld pk, ptop, kk
        div q, n, pk
        get r, rR
        bz r, 4b
7       cmp t, q, pk
        bnp t, 2b
        incl kk, 4
        jmp 6b

taddr   global @
title   byte "First Five Hundred Primes", 10, 0, 0
newLn   byte 10, 0, 0, 0

blanks  byte "   ", 0
zaddr   global @
zeroes  byte " 0000", 0, 0, 0
2       geta t, title
        trap 0, 1, 0
        neg mm, 4
3       add mm, mm, j0
        geta t, blanks
        trap 0, 1, 0
2       ld pk, ptop, mm
        geta %3, buf
        ld t, zaddr, 0
        st t, %3, 0
        ld t, zaddr, 4
        st t, %3, 4
        geta t, buf
        incl t, 4
1       div pk, pk, 10
        get r, rR
        incl r, 48
        stb r, t, 0
        sub t, t, 1
        bnz pk, 1b
        geta t, buf
        trap 0, 1, 0
        incl mm, 4 * Count / 10
        bn mm, 2b
        geta t, newLn
        trap 0, 1, 0
        cmp t, mm, 4 * (Count / 10 - 1)
        bnz t, 3b
        pop 0, 0
