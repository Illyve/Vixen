	.text
	.globl	main
	.def	main;	.scl	2;	.type	32;	.endef
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	call	__main
	movq	$4, %r8
	movq	$8, %r9
	movq	%r8, %rcx
	movq	%r9, %rdx
	call	calloc
	movq	%rax, %r8
	movq	%r8, -8(%rbp)
	addq	$48, %rsp
	popq	%rbp
	ret
