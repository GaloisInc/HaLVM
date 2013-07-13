#ifndef HALVMCORE_DOMAIN_INFO_H
#define HALVMCORE_DOMAIN_INFO_H

#include <stdint.h>

uint32_t       get_domain_flags(void);
char          *get_magic_string_ptr(void);
unsigned long  get_domain_mod_start(void);
unsigned long  get_domain_mod_len(void);
unsigned int   get_console_evtchn(void);
unsigned long  get_console_mfn(void);
unsigned int   get_xenstore_evtchn(void);
unsigned long  get_xenstore_mfn(void);

#endif
