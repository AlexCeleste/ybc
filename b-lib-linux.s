	.text
	.align 4

	.globl	lib$div
lib$div:
## BB#0:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	cltd
	idivl	12(%ebp)
	popl	%ebp
	ret

	.globl	lib$mod
lib$mod:
## BB#0:
	pushl	%ebp
	movl	%esp, %ebp
	movl	8(%ebp), %eax
	cltd
	idivl	12(%ebp)
	movl	%edx, %eax
	popl	%ebp
	ret

	.globl	char
char:
## BB#0:
	pushl	%ebp
	movl	%esp, %ebp
	movl	12(%ebp), %eax
	movl	8(%ebp), %ecx
	movsbl	(%ecx,%eax), %eax
	popl	%ebp
	ret

