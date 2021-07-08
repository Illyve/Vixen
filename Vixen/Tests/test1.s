	.globl	a
	.data
a:
	.quad 4
	.text
	.globl	x
	.def	x;	.scl	2;	.type	32;	.endef
x:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	call	__main
	addq	$48,	$rsp
	popq	%rbp
	ret
