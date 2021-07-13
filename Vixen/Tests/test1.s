.LC0:
	.ascii	"%def\n\0"
.LC1:
	.ascii	"dsfaef\0"
	.globl	a
	.data
a:
	.quad	4
	.globl	b
	.data
b:
	.quad	.LC1
	.globl	c
	.data
c:
	.quad	.LC0
	.text
	.globl	main
	.def	main;	.scl	2;	.type	32;	.endef
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	call	__main
	movq	c, %r8
	pushq	%r8
	movq	$4, %r8
	movq	%r8, %rcx
	call	e
	movq	%rax, %r8
	pushq	%r8
	popq	%r8
	movq	%r8, %rdx
	popq	%r8
	movq	%r8, %rcx
	call	printf
	addq	$48, %rsp
	popq	%rbp
	ret
	.text
	.globl	e
	.def	e;	.scl	2;	.type	32;	.endef
e:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rcx, -8(%rbp)
	movq	-8(%rbp), %r8
	movq	%r8, %rax
	addq	$48, %rsp
	popq	%rbp
	ret
