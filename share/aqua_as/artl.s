
	device.ld = 65535 * 65536 + 61696
	puts = 1

	extern main
	
start:
    put rS, 4096
    put rO, 4096
    geta %2, 1f, 0
    put rT, %2
    set %3, %0
    set %4, %1
    pushj %2, main
    trap 0, 0, 0

1   pushj %255, 2f
    put rJ, %255
    get %255, rBB
    resume 1

2   get %0, rXX
    sr %0, %0, 16
    and %0, %0, 255
    mul %0, %0, 4
	geta %1, trapVectors
	ld %1, %0, %1
    go %2, %1, 0

trapVectors
    word trapHalt, trapIO, trapLD, trapPutChar

trapHalt
    pop 0, 0

trapIO
    get %1, rBB
    seth %2, 65535
    incl %2, 61443
    setl %0, 0

3   ldb %3, %1, %0
    bz %3, 4f
    stb %3, %2, 0
    add %0, %0, 1
    jmp 3b

4   put rBB, %0
    pop 1, 0

trapPutChar
	get %0, rBB
	seth %2, 65535
	incl %2, 61443
	stb %0, %2, 0
	pop 0,0
	
trapLD
	get %0, rWW
	sub %0, %0, 4
	put rWW, %0
	seth %1, 65535
	incl %1, 61696
	st %0, %1, 4
	get %0, rXX
	st %0, %1, 0
	pop 0, 0

main
