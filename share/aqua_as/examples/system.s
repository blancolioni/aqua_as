	export putchar

putchar:
	setl %1, 15 * 4096
	inch %1, 65535
	st %0, %1
	pop 0, 0
	
