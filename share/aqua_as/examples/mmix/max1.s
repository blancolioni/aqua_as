j = %0
m = %1
kk = %2
xk = %3
t = %255
x0 = %254
chrout = %253

main:       geta x0, data
            sub x0, x0, 4
            setl %2, 7
            pushj %1, Maximum
            setl chrout, 65490
            stb %1, chrout, 0
            setl %1, 10
            stb %1, chrout, 0
            trap 0, 0, 0

data:       word 65, 66, 67, 68, 67, 66, 65

Maximum:    sl kk, %0, 2    ; initialize: k <- n, j <- n
            ld m, x0, kk    ; m <- X[n]
            jmp decrK       ; to M2 with k <- n - 1
Loop:       ld xk, x0, kk   ; compare
            cmp t, xk, m    ; t <- compare X[k] m
            bnp t, decrK    ; to M5 if X[k] <= m
ChangeM:    or m, xk, 0     ; M4: change m.  m <- X[k]
            sr j, kk, 4     ; j <- k
decrK:      sub kk, kk, 4   ; M5. Decrease k.  k <- k - 1
            bp kk, Loop     ; All tested?
            pop 2, 0

