#define __XEN__
#include <sys/types.h>
#include <stdint.h>
#include <xen/xen.h>

extern start_info_t *system_start_info;

char *get_magic_string_ptr(void)
{
  return system_start_info->magic;
}

uint32_t get_domain_flags(void)
{
  return system_start_info->flags;
}

unsigned long get_domain_mod_start(void)
{
  return system_start_info->mod_start;
}

unsigned long get_domain_mod_len(void)
{
  return system_start_info->mod_len;
}

unsigned int get_console_evtchn(void)
{
  return system_start_info->console.domU.evtchn;
}

unsigned long get_console_mfn(void)
{
  return system_start_info->console.domU.mfn;
}

unsigned int get_xenstore_evtchn(void)
{
  return system_start_info->store_evtchn;
}

unsigned long get_xenstore_mfn(void)
{
  return system_start_info->store_mfn;
}

