.LC0:
	.ascii	"dsfaef\0"
.LC1:
	.ascii	"jeofaief\n\0"
	.globl	a
	.data
a:
	.quad	4
	.globl	b
	.data
b:
	.quad	.LC0
	.globl	c
	.data
c:
	.quad	.LC1
	.text
	.globl	x
	.def	x;	.scl	2;	.type	32;	.endef
x:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	call	__main
	movq	$3, %r8
	movq	$4, %r9
	addq	%r9, %r8
	movq	%r8, a(%rip)
	movq	$5, %r9
	movq	%r9, a(%rip)
	addq	$48, %rsp
	popq	%rbp
	ret
