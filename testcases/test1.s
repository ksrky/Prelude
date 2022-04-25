	.text
	.file	"test1.ll"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry_0
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$putNumForm, %edi
	movl	$2, %esi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	putNumForm,@object      # @putNumForm
	.section	.rodata.str1.1,"aMS",@progbits,1
	.globl	putNumForm
putNumForm:
	.asciz	"%d\n"
	.size	putNumForm, 4


	.section	".note.GNU-stack","",@progbits
