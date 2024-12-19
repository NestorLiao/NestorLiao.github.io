        ./init312:	file format elf64-x86-64

        Disassembly of section .init:

        0000000000401000 <_init>:
401000: f3 0f 1e fa                  	endbr64
401004: 48 83 ec 08                  	subq	$0x8, %rsp
401008: 48 8b 05 e9 2f 00 00         	movq	0x2fe9(%rip), %rax      # 0x403ff8 <_GLOBAL_OFFSET_TABLE_+0x28>
40100f: 48 85 c0                     	testq	%rax, %rax
401012: 74 02                        	je	0x401016 <_init+0x16>
401014: ff d0                        	callq	*%rax
401016: 48 83 c4 08                  	addq	$0x8, %rsp
40101a: c3                           	retq

        Disassembly of section .plt:

        0000000000401020 <.plt>:
401020: ff 35 b2 2f 00 00            	pushq	0x2fb2(%rip)            # 0x403fd8 <_GLOBAL_OFFSET_TABLE_+0x8>
401026: ff 25 b4 2f 00 00            	jmpq	*0x2fb4(%rip)           # 0x403fe0 <_GLOBAL_OFFSET_TABLE_+0x10>
40102c: 0f 1f 40 00                  	nopl	(%rax)
401030: ff 25 b2 2f 00 00            	jmpq	*0x2fb2(%rip)           # 0x403fe8 <_GLOBAL_OFFSET_TABLE_+0x18>
401036: 68 00 00 00 00               	pushq	$0x0
40103b: e9 e0 ff ff ff               	jmp	0x401020 <.plt>

        Disassembly of section .text:

        0000000000401040 <main>:
401040: 53                           	pushq	%rbx
401041: 48 8d 1d bc 0f 00 00         	leaq	0xfbc(%rip), %rbx       # 0x402004 <_IO_stdin_used+0x4>
401048: 31 d2                        	xorl	%edx, %edx
40104a: bf 02 00 00 00               	movl	$0x2, %edi
40104f: 48 89 de                     	movq	%rbx, %rsi
401052: 31 c0                        	xorl	%eax, %eax
401054: e8 d7 ff ff ff               	callq	0x401030 <.plt+0x10>
401059: 48 89 de                     	movq	%rbx, %rsi
40105c: ba a2 0d 00 00               	movl	$0xda2, %edx            # imm = 0xDA2
401061: 31 c0                        	xorl	%eax, %eax
401063: bf 02 00 00 00               	movl	$0x2, %edi
401068: e8 c3 ff ff ff               	callq	0x401030 <.plt+0x10>
40106d: 48 89 de                     	movq	%rbx, %rsi
401070: 31 d2                        	xorl	%edx, %edx
401072: bf 02 00 00 00               	movl	$0x2, %edi
401077: 31 c0                        	xorl	%eax, %eax
401079: e8 b2 ff ff ff               	callq	0x401030 <.plt+0x10>
40107e: 31 c0                        	xorl	%eax, %eax
401080: 5b                           	popq	%rbx
401081: c3                           	retq
401082: 66 2e 0f 1f 84 00 00 00 00 00	nopw	%cs:(%rax,%rax)
40108c: 0f 1f 40 00                  	nopl	(%rax)

        0000000000401090 <_start>:
401090: f3 0f 1e fa                  	endbr64
401094: 31 ed                        	xorl	%ebp, %ebp
401096: 49 89 d1                     	movq	%rdx, %r9
401099: 5e                           	popq	%rsi
40109a: 48 89 e2                     	movq	%rsp, %rdx
40109d: 48 83 e4 f0                  	andq	$-0x10, %rsp
4010a1: 50                           	pushq	%rax
4010a2: 54                           	pushq	%rsp
4010a3: 45 31 c0                     	xorl	%r8d, %r8d
4010a6: 31 c9                        	xorl	%ecx, %ecx
4010a8: 48 c7 c7 40 10 40 00         	movq	$0x401040, %rdi         # imm = 0x401040
4010af: ff 15 3b 2f 00 00            	callq	*0x2f3b(%rip)           # 0x403ff0 <_GLOBAL_OFFSET_TABLE_+0x20>
4010b5: f4                           	hlt
4010b6: 66 2e 0f 1f 84 00 00 00 00 00	nopw	%cs:(%rax,%rax)

        00000000004010c0 <_dl_relocate_static_pie>:
4010c0: f3 0f 1e fa                  	endbr64
4010c4: c3                           	retq
4010c5: 66 2e 0f 1f 84 00 00 00 00 00	nopw	%cs:(%rax,%rax)
4010cf: 90                           	nop

        00000000004010d0 <deregister_tm_clones>:
4010d0: b8 10 40 40 00               	movl	$0x404010, %eax         # imm = 0x404010
4010d5: 48 3d 10 40 40 00            	cmpq	$0x404010, %rax         # imm = 0x404010
4010db: 74 13                        	je	0x4010f0 <deregister_tm_clones+0x20>
4010dd: b8 00 00 00 00               	movl	$0x0, %eax
4010e2: 48 85 c0                     	testq	%rax, %rax
4010e5: 74 09                        	je	0x4010f0 <deregister_tm_clones+0x20>
4010e7: bf 10 40 40 00               	movl	$0x404010, %edi         # imm = 0x404010
4010ec: ff e0                        	jmpq	*%rax
4010ee: 66 90                        	nop
4010f0: c3                           	retq
4010f1: 66 66 2e 0f 1f 84 00 00 00 00 00     	nopw	%cs:(%rax,%rax)
4010fc: 0f 1f 40 00                  	nopl	(%rax)

        0000000000401100 <register_tm_clones>:
401100: be 10 40 40 00               	movl	$0x404010, %esi         # imm = 0x404010
401105: 48 81 ee 10 40 40 00         	subq	$0x404010, %rsi         # imm = 0x404010
40110c: 48 89 f0                     	movq	%rsi, %rax
40110f: 48 c1 ee 3f                  	shrq	$0x3f, %rsi
401113: 48 c1 f8 03                  	sarq	$0x3, %rax
401117: 48 01 c6                     	addq	%rax, %rsi
40111a: 48 d1 fe                     	sarq	%rsi
40111d: 74 11                        	je	0x401130 <register_tm_clones+0x30>
40111f: b8 00 00 00 00               	movl	$0x0, %eax
401124: 48 85 c0                     	testq	%rax, %rax
401127: 74 07                        	je	0x401130 <register_tm_clones+0x30>
401129: bf 10 40 40 00               	movl	$0x404010, %edi         # imm = 0x404010
40112e: ff e0                        	jmpq	*%rax
401130: c3                           	retq
401131: 66 66 2e 0f 1f 84 00 00 00 00 00     	nopw	%cs:(%rax,%rax)
40113c: 0f 1f 40 00                  	nopl	(%rax)

        0000000000401140 <__do_global_dtors_aux>:
401140: f3 0f 1e fa                  	endbr64
401144: 80 3d c5 2e 00 00 00         	cmpb	$0x0, 0x2ec5(%rip)      # 0x404010 <completed.0>
40114b: 75 13                        	jne	0x401160 <__do_global_dtors_aux+0x20>
40114d: 55                           	pushq	%rbp
40114e: 48 89 e5                     	movq	%rsp, %rbp
401151: e8 7a ff ff ff               	callq	0x4010d0 <deregister_tm_clones>
401156: c6 05 b3 2e 00 00 01         	movb	$0x1, 0x2eb3(%rip)      # 0x404010 <completed.0>
40115d: 5d                           	popq	%rbp
40115e: c3                           	retq
40115f: 90                           	nop
401160: c3                           	retq
401161: 66 66 2e 0f 1f 84 00 00 00 00 00     	nopw	%cs:(%rax,%rax)
40116c: 0f 1f 40 00                  	nopl	(%rax)

        0000000000401170 <frame_dummy>:
401170: f3 0f 1e fa                  	endbr64
401174: eb 8a                        	jmp	0x401100 <register_tm_clones>

        Disassembly of section .fini:

        0000000000401178 <_fini>:
401178: f3 0f 1e fa                  	endbr64
40117c: 48 83 ec 08                  	subq	$0x8, %rsp
401180: 48 83 c4 08                  	addq	$0x8, %rsp
401184: c3                           	retq
