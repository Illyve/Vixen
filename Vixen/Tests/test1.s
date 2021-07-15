	.globl	a
	.data
a:
	.quad	0
	.text
	.globl	main
	.def	main;	.scl	2;	.type	32;	.endef
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	call	__main
	movq	$0, %r8
	movq	a, %r9
	addq	%r9, %r8
	movq	(%r8), %r8
	movq	%r8, -8(%rbp)
	addq	$48, %rsp
	popq	%rbp
	ret
