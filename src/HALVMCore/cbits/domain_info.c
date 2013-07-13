#include <hbmxen.h>

char *get_magic_string_ptr(void)
{
  return start_info->magic;
}

uint32_t get_domain_flags(void)
{
  return start_info->flags;
}

unsigned long get_domain_mod_start(void)
{
  return start_info->mod_start;
}

unsigned long get_domain_mod_len(void)
{
  return start_info->mod_len;
}

unsigned int get_console_evtchn(void)
{
  return start_info->console_evtchn;
}

unsigned long get_console_mfn(void)
{
  return start_info->console_mfn;
}

unsigned int get_xenstore_evtchn(void)
{
  return start_info->store_evtchn;
}

unsigned long get_xenstore_mfn(void)
{
  return start_info->store_mfn;
}

