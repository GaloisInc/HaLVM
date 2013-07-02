#include "core-hypercalls.h"

int do_domctl_op(xen_domctl_t *v)
{
  return _hypercall1(int, domctl, v);
}

int do_sysctl_op(xen_sysctl_t *v)
{
  return _hypercall1(int, sysctl, v);
}

int do_physdev_op(int cmd, void *arg)
{
  return _hypercall2(int, physdev_op, cmd, arg);
}

int do_memory_op(int cmd, void *arg)
{
  return _hypercall2(int, memory_op, cmd, arg);
}

int update_va_mapping_otherdomain(unsigned long va, unsigned long new_val,
                                  unsigned long fl, domid_t dom)
{
#ifdef CONFIG_X86_PAE
  pte_t pte = { .pte_high = 0, .pte_low = new_val };
#else
# if defined(__x86_64__)
  pte_t pte = { .pte = new_val };
# else
  pte_t pte = { .pte_low = new_val };
# endif
#endif
  return _hypercall4(int, update_va_mapping_otherdomain, va, pte.pte, fl, dom);
}
