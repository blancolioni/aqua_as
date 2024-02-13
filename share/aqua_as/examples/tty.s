ch = %0
last = %1
chrout = %2

        export _start
        extern _putchar
	
_start: setl ch, 32
        setl last, 127
        setl chrout, 61443
	inch chrout, 65535
_1:     stb ch, chrout, 0
        add  ch, ch, 1
        cmp %3, last, ch
        bnz %3, _1
        setl ch, 10
        stb ch, chrout, 0
        trap 0, 0, 0

