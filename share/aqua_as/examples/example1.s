ch = %0
last = %1

        export start
        extern system.putchar
	
start:  setl ch, 32
        setl last, 127
1:	set %2, ch
	pushj %2, system.putchar
        add  ch, ch, 1
        cmp %3, last, ch
        bnz %3, 1b
        setl ch, 10
	pushj ch, system.putchar
        trap 0, 0, 0

