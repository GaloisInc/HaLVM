/* from mini-os 
   no copyright notice. */   

#include <traps.h>
#include <hbmxen.h>
#include <mm.h>
#include <hypercall.h>
#include <stdio.h>
#include <arch.h>

/*
 * These are assembler stubs in start.S
 * They are the actual entry points for virtual exceptions.
 */
void divide_error(void);
void debug(void);
void int3(void);
void overflow(void);
void bounds(void);
void invalid_op(void);
void device_not_available(void);
void coprocessor_segment_overrun(void);
void invalid_TSS(void);
void segment_not_present(void);
void stack_segment(void);
void general_protection(void);
void page_fault(void);
void coprocessor_error(void);
void simd_coprocessor_error(void);
void alignment_check(void);
void spurious_interrupt_bug(void);
void machine_check(void);

struct fpstate {
  uint16_t fcw;
  uint16_t fsw;
  uint8_t  ftw;
  uint8_t  deadbyte;
  uint16_t fop;
  uint32_t fpu_ip;
  uint16_t fpu_cs;
  uint16_t reserved;
  uint32_t fpu_dp;
  uint16_t fpu_ds;
  uint16_t reserved2;
  uint32_t mxcsr;
  uint32_t mxcsr_mask;
};

unsigned char fpu_state_space[512];

void dump_fpu_information()
{
  struct fpstate *fpst = (struct fpstate *)fpu_state_space;
  asm volatile ("fxsave %0" : : "m" (*fpst));
  printf("FPU_CS: %04x FPU_IP: %08lx  FPU_DS: %04x FPU_DP: %08lx\n",
	     fpst->fpu_cs, fpst->fpu_ip, fpst->fpu_ds, fpst->fpu_dp);
  printf("FPU_ControlWord: %04x FPU_StateWord: %04x\n", fpst->fcw, fpst->fsw);
  printf("FPU_TagWord: %02x FPU_OpCode: %04x\n", fpst->ftw, fpst->fop);
  printf("FPU_CSR: %08lx FPU_CSR_MASK: %08lx\n", fpst->mxcsr, fpst->mxcsr_mask);
}

static void dump_regs(struct pt_regs *regs)
{
  // printf("Thread: %s\n", current->name);
#ifdef __i386__    
    printf("EIP: %08lx, EFLAGS %08lx.\n", regs->eip, regs->eflags);
    printf("EBX: %08lx ECX: %08lx EDX: %08lx\n",
	   regs->ebx, regs->ecx, regs->edx);
    printf("ESI: %08lx EDI: %08lx EBP: %08lx EAX: %08lx\n",
	   regs->esi, regs->edi, regs->ebp, regs->eax);
    printf("DS: %04x ES: %04x orig_eax: %08lx, eip: %08lx\n",
	   regs->xds, regs->xes, regs->orig_eax, regs->eip);
    printf("CS: %04x EFLAGS: %08lx esp: %08lx ss: %04x\n",
	   regs->xcs, regs->eflags, regs->esp, regs->xss);
    dump_fpu_information();
#else
    printf("RIP: %04lx:[<%016lx>] ", regs->cs & 0xffff, regs->rip);
    printf("\nRSP: %04lx:%016lx  EFLAGS: %08lx\n", 
           regs->ss, regs->rsp, regs->eflags);
    printf("RAX: %016lx RBX: %016lx RCX: %016lx\n",
           regs->rax, regs->rbx, regs->rcx);
    printf("RDX: %016lx RSI: %016lx RDI: %016lx\n",
           regs->rdx, regs->rsi, regs->rdi); 
    printf("RBP: %016lx R08: %016lx R09: %016lx\n",
           regs->rbp, regs->r8, regs->r9); 
    printf("R10: %016lx R11: %016lx R12: %016lx\n",
           regs->r10, regs->r11, regs->r12); 
    printf("R13: %016lx R14: %016lx R15: %016lx\n",
           regs->r13, regs->r14, regs->r15); 
#endif
}

static void do_trap(int trapnr, char *str, struct pt_regs * regs, unsigned long error_code)
{
    printf("FATAL:  Unhandled Trap %d (%s), error code=0x%lx\n", trapnr, str, error_code);
    printf("Regs address %08lx\n", (unsigned long)regs);
    dump_regs(regs);
    do_exit();
}

#define DO_ERROR(trapnr, str, name) \
void do_##name(struct pt_regs * regs, unsigned long error_code) \
{ \
	do_trap(trapnr, str, regs, error_code); \
}

#define DO_ERROR_INFO(trapnr, str, name, sicode, siaddr) \
void do_##name(struct pt_regs * regs, unsigned long error_code) \
{ \
	do_trap(trapnr, str, regs, error_code); \
}

DO_ERROR_INFO( 0, "divide error", divide_error, FPE_INTDIV, regs->eip)
DO_ERROR( 3, "int3", int3)
DO_ERROR( 4, "overflow", overflow)
DO_ERROR( 5, "bounds", bounds)
DO_ERROR_INFO( 6, "invalid operand", invalid_op, ILL_ILLOPN, regs->eip)
DO_ERROR( 7, "device not available", device_not_available)
DO_ERROR( 9, "coprocessor segment overrun", coprocessor_segment_overrun)
DO_ERROR(10, "invalid TSS", invalid_TSS)
DO_ERROR(11, "segment not present", segment_not_present)
DO_ERROR(12, "stack segment", stack_segment)
DO_ERROR_INFO(17, "alignment check", alignment_check, BUS_ADRALN, 0)
DO_ERROR(18, "machine check", machine_check)

#define read_cr2() \
        (HYPERVISOR_shared_info->vcpu_info[smp_processor_id()].arch.cr2)

void do_page_fault(struct pt_regs *regs, unsigned long error_code)
{
    unsigned long addr = read_cr2();
    printf("Page fault at linear address %08lx, regs %08lx, code %lx\n", 
	   addr, (unsigned long)regs, error_code);
    dump_regs(regs);
    // page_walk(addr);
    do_exit();
}

void do_general_protection(struct pt_regs *regs, long error_code)
{
#ifdef __i386__
    printf("GPF eip: %08lx, error_code=%lx\n", regs->eip, error_code);
#else    
    printf("GPF rip: %p, error_code=%lx\n", regs->rip, error_code);
#endif
    dump_regs(regs);
    do_exit();
}


void do_debug(struct pt_regs * regs)
{
    printf("Debug exception\n");
#define TF_MASK 0x100
    regs->eflags &= ~TF_MASK;
    dump_regs(regs);
    do_exit();
}

void do_coprocessor_error(struct pt_regs * regs)
{
    printf("Copro error\n");
    dump_regs(regs);
    do_exit();
}

void simd_math_error(void *eip)
{
    printf("SIMD error at EIP %p\n", eip);
}

void do_simd_coprocessor_error(struct pt_regs * regs)
{
    printf("SIMD copro error\n");
    dump_regs(regs);
}

void do_spurious_interrupt_bug(struct pt_regs * regs __attribute__((unused)))
{
  printf("Spurious interrupt!\n");
}

/*
 * Submit a virtual IDT to the hypervisor. This consists of tuples
 * (interrupt vector, privilege ring, CS:EIP of handler).
 * The 'privilege ring' field specifies the least-privileged ring that
 * can trap to that vector using a software-interrupt instruction (INT).
 */
static trap_info_t trap_table[] = {
    {  0, 0, __KERNEL_CS, (unsigned long)divide_error                },
    {  1, 0, __KERNEL_CS, (unsigned long)debug                       },
    {  3, 3, __KERNEL_CS, (unsigned long)int3                        },
    {  4, 3, __KERNEL_CS, (unsigned long)overflow                    },
    {  5, 3, __KERNEL_CS, (unsigned long)bounds                      },
    {  6, 0, __KERNEL_CS, (unsigned long)invalid_op                  },
    {  7, 0, __KERNEL_CS, (unsigned long)device_not_available        },
    {  9, 0, __KERNEL_CS, (unsigned long)coprocessor_segment_overrun },
    { 10, 0, __KERNEL_CS, (unsigned long)invalid_TSS                 },
    { 11, 0, __KERNEL_CS, (unsigned long)segment_not_present         },
    { 12, 0, __KERNEL_CS, (unsigned long)stack_segment               },
    { 13, 0, __KERNEL_CS, (unsigned long)general_protection          },
    { 14, 0, __KERNEL_CS, (unsigned long)page_fault                  },
    { 15, 0, __KERNEL_CS, (unsigned long)spurious_interrupt_bug      },
    { 16, 0, __KERNEL_CS, (unsigned long)coprocessor_error           },
    { 17, 0, __KERNEL_CS, (unsigned long)alignment_check             },
    { 18, 0, __KERNEL_CS, (unsigned long)machine_check               },
    { 19, 0, __KERNEL_CS, (unsigned long)simd_coprocessor_error      },
    {  0, 0,           0, 0                           }
};
    
void hypervisor_callback(void);
void failsafe_callback(void);

void init_traps(void)
{
#if defined(__x86_64__)
  HYPERVISOR_set_callbacks((unsigned long)hypervisor_callback,
                           (unsigned long)failsafe_callback,
                           0);
#else
  HYPERVISOR_set_callbacks(__KERNEL_CS, (unsigned long)hypervisor_callback,
                           __KERNEL_CS, (unsigned long)failsafe_callback);
#endif
  __sti();
  HYPERVISOR_set_trap_table(trap_table);
}

