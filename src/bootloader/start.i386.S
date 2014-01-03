# BANNERSTART
# - Copyright 2006-2008, Galois, Inc.
# - This software is distributed under a standard, three-clause BSD license.
# - Please see the file LICENSE, distributed with this software, for specific
# - terms and conditions.
# Author: Adam Wick <awick@galois.com>
# BANNEREND
#define __ASSEMBLY__ 1
#include <arch.h>
#include <xen/arch-x86_32.h>

#if __XEN_LATEST_INTERFACE_VERSION__ >= 0x00030205
#include <xen/elfnote.h>
#include <elfnote.h>
#endif

	/* For Xen */	

.text

.globl _start, hypercall_page
.extern runtime_entry, do_hypervisor_callback
	
_start:
       cld
       fldcw newfpucw
       push %esp
       push %esi
       call runtime_entry
fail_out_of_boot:
       xor %eax, %eax
       div %eax, %eax

# Following cruft taken rom Xen mini-os (which in turn stole it from linux guests, I think)
# (There was no copyright notice.)

# These are offsets into the frame presented by a hypercallback
# They are taken from those in current mini-OS -- I don't know why
# Different values are still used in linux/entry-xen.S, but these work better.
# 	
# ES		= 0x1c
# ORIG_EAX	= 0x20
# EIP		= 0x24
# CS		= 0x28

# The offsets above do not correspond to the actual struct.
# cf: ghc-xen/rts/includes/trap.h

ES              = 0x20          
ORIG_EAX        = 0x24          
EIP             = 0x28          
CS              = 0x2c


#define SAVE_ALL \
	cld; \
	pushl %es; \
	pushl %ds; \
	pushl %eax; \
	pushl %ebp; \
	pushl %edi; \
	pushl %esi; \
	pushl %edx; \
	pushl %ecx; \
	pushl %ebx; \
	movl $(FLAT_KERNEL_DS),%edx; \
	movl %edx,%ds; \
	movl %edx,%es;

#define RESTORE_ALL	\
	popl %ebx;	\
	popl %ecx;	\
	popl %edx;	\
	popl %esi;	\
	popl %edi;	\
	popl %ebp;	\
	popl %eax;	\
	popl %ds;	\
	popl %es;	\
	addl $4,%esp;	\
	iret;		\


# A note on the "critical region" in our callback handler.
# We want to avoid stacking callback handlers due to events occurring
# during handling of the last event. To do this, we keep events disabled
# until weve done all processing. HOWEVER, we must enable events before
# popping the stack frame (cant be done atomically) and so it would still
# be possible to get enough handler activations to overflow the stack.
# Although unlikely, bugs of that kind are hard to track down, so wed
# like to avoid the possibility.
# So, on entry to the handler we detect whether we interrupted an
# existing activation in its critical region -- if so, we pop the current
# activation and restart the handler using the previous one.
.globl hypervisor_callback
hypervisor_callback:
        pushl %eax
        SAVE_ALL
        movl EIP(%esp),%eax
        cmpl $scrit,%eax
        jb   11f
        cmpl $ecrit,%eax
        jb   critical_region_fixup
11:     push %esp
        call do_hypervisor_callback
        add  $4,%esp
        movl HYPERVISOR_shared_info,%esi
        xorl %eax,%eax
        movb CS(%esp),%cl
    	test $2,%cl          # slow return to ring 2 or 3
        jne  safesti
safesti:movb $0,1(%esi)     # reenable event callbacks
scrit:  /**** START OF CRITICAL REGION ****/
        testb $0xFF,(%esi)
        jnz  14f              # process more events if necessary...
        RESTORE_ALL
14:     movb $1,1(%esi)
        jmp  11b
ecrit:  /**** END OF CRITICAL REGION ****/
# [How we do the fixup]. We want to merge the current stack frame with the
# just-interrupted frame. How we do this depends on where in the critical
# region the interrupted handler was executing, and so how many saved
# registers are in each frame. We do this quickly using the lookup table
# 'critical_fixup_table'. For each byte offset in the critical region, it
# provides the number of bytes which have already been popped from the
# interrupted stack frame. 
critical_region_fixup:
        addl $critical_fixup_table-scrit,%eax
        movzbl (%eax),%eax    # %eax contains num bytes popped
        mov  %esp,%esi
        add  %eax,%esi        # %esi points at end of src region
        mov  %esp,%edi
        add  $0x34,%edi       # %edi points at end of dst region
        mov  %eax,%ecx
        shr  $2,%ecx          # convert words to bytes
        je   16f              # skip loop if nothing to copy
15:     subl $4,%esi          # pre-decrementing copy loop
        subl $4,%edi
        movl (%esi),%eax
        movl %eax,(%edi)
        loop 15b
16:     movl %edi,%esp        # final %edi is top of merged stack
        jmp  11b
         
critical_fixup_table:        
        .byte 0x00,0x00,0x00                  # testb $0xff,(%esi)
        .byte 0x00,0x00                       # jne  14f
        .byte 0x00                            # pop  %ebx
        .byte 0x04                            # pop  %ecx
        .byte 0x08                            # pop  %edx
        .byte 0x0c                            # pop  %esi
        .byte 0x10                            # pop  %edi
        .byte 0x14                            # pop  %ebp
        .byte 0x18                            # pop  %eax
        .byte 0x1c                            # pop  %ds
        .byte 0x20                            # pop  %es
        .byte 0x24,0x24,0x24                  # add  $4,%esp
        .byte 0x28                            # iret
        .byte 0x00,0x00,0x00,0x00             # movb $1,1(%esi)
        .byte 0x00,0x00                       # jmp  11b
       
# Hypervisor uses this for application faults while it executes.
.globl failsafe_callback	
failsafe_callback:	
      pop  %ds
      pop  %es
      pop  %fs
      pop  %gs
      iret
                
# Handlers for virtual exceptions

#define ENTRY(X) .globl X ; X :
	
ENTRY(divide_error)
	pushl $0		# no error code
	pushl $do_divide_error
do_exception:
	pushl %ds
	pushl %eax
	xorl %eax, %eax
	pushl %ebp
	pushl %edi
	pushl %esi
	pushl %edx
	decl %eax			# eax = -1
	pushl %ecx
	pushl %ebx
	cld
	movl %es, %ecx
	movl ES(%esp), %edi		# get the function address
	movl ORIG_EAX(%esp), %edx	# get the error code
	movl %eax, ORIG_EAX(%esp)
	movl %ecx, ES(%esp)
	movl $(FLAT_KERNEL_DS), %ecx
	movl %ecx, %ds
	movl %ecx, %es
	movl %esp,%eax			# pt_regs pointer
    pushl %edx
    pushl %eax
	call *%edi
    addl $8,%esp
    
ret_from_exception:
        movb CS(%esp),%cl
	test $2,%cl          # slow return to ring 2 or 3
	jne  safesti
        RESTORE_ALL

ENTRY(coprocessor_error)
	pushl $0
	pushl $do_coprocessor_error
	jmp do_exception

ENTRY(simd_coprocessor_error)
	pushl $0
	pushl $do_simd_coprocessor_error
	jmp do_exception

ENTRY(device_not_available)
        iret

ENTRY(debug)
	pushl $0
	pushl $do_debug
	jmp do_exception

ENTRY(int3)
	pushl $0
	pushl $do_int3
	jmp do_exception

ENTRY(overflow)
	pushl $0
	pushl $do_overflow
	jmp do_exception

ENTRY(bounds)
	pushl $0
	pushl $do_bounds
	jmp do_exception

ENTRY(invalid_op)
	pushl $0
	pushl $do_invalid_op
	jmp do_exception

ENTRY(coprocessor_segment_overrun)
	pushl $0
	pushl $do_coprocessor_segment_overrun
	jmp do_exception

ENTRY(invalid_TSS)
	pushl $do_invalid_TSS
	jmp do_exception

ENTRY(segment_not_present)
	pushl $do_segment_not_present
	jmp do_exception

ENTRY(stack_segment)
	pushl $do_stack_segment
	jmp do_exception

ENTRY(general_protection)
	pushl $do_general_protection
	jmp do_exception

ENTRY(alignment_check)
	pushl $do_alignment_check
	jmp do_exception

# This handler is special, because it gets an extra value on its stack,
# which is the linear faulting address.
# fastcall register usage:  %eax = pt_regs, %edx = error code,
#			    %ecx = fault address
ENTRY(page_fault)
    pushl $do_page_fault
    jmp do_exception

ENTRY(machine_check)
	pushl $0
	pushl $do_machine_check
	jmp do_exception

ENTRY(spurious_interrupt_bug)
	pushl $0
	pushl $do_spurious_interrupt_bug
	jmp do_exception

# note that this must agree with HYPERCALL_PAGE attribute above
	.org 0x1000
hypercall_page:
	.org 0x2000

.data

newfpucw:
    .word 0x037f

    .align 0x1000
gdt_base:
    # Entry 0: Blank.
    .quad 0
    # Entry 1: Text segment. 0 base, limited extent
    .word 0xffff        # Segment limit 15:00
    .word 0             # base address 15:00
    .byte 0             # base address 23:16
    .byte 0xbf          # P / DPL / S / TYPE
    .byte 0xcf          # G / DB / L / AVL / seg limit 19:16
    .byte 0             # base address 31:24
    # Entry 2: Data segment. 0 base, unlimited extent
    .word 0xffff        # Segment limit 15:00                   (offset 0)
    .word 0             # base address 15:00                    (offset 2)
    .byte 0             # base address 23:16                    (offset 4)
    .byte 0xb3          # P / DPL / S / TYPE                    (offset 5)
    .byte 0xcf          # G / DB / L / AVL / seg limit 19:16    (offset 6)
    .byte 0             # base address 31:24                    (offset 7)
    # Entry 3: Stack segment. Computed base, limited extent
    .word 0xffff        # Segment limit 15:00
    .word 0             # base address 15:00
    .byte 0             # base address 23:16
    .byte 0xb3          # P / DPL / S / TYPE
    .byte 0xcf          # G / DB / L / AVL / seg limit 19:16
    .byte 0             # base address 31:24


    .align 0x1000
#if __XEN_LATEST_INTERFACE_VERSION__ >= 0x00030205
	ELFNOTE(Xen, XEN_ELFNOTE_GUEST_OS,       .asciz, "HaLVM")	
	ELFNOTE(Xen, XEN_ELFNOTE_XEN_VERSION,    .asciz, "xen-3.0")
	ELFNOTE(Xen, XEN_ELFNOTE_VIRT_BASE,      .long,  0xbac00000)
	ELFNOTE(Xen, XEN_ELFNOTE_PADDR_OFFSET,   .long,  0xbac00000)
	ELFNOTE(Xen, XEN_ELFNOTE_ENTRY,          .long,  _start)
	ELFNOTE(Xen, XEN_ELFNOTE_HYPERCALL_PAGE, .long,  hypercall_page)
    ELFNOTE(Xen, XEN_ELFNOTE_PAE_MODE,       .asciz, "yes")
	ELFNOTE(Xen, XEN_ELFNOTE_LOADER,         .asciz, "generic")
#endif

