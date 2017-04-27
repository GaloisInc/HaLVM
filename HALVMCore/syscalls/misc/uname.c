#include <sys/utsname.h>
#include <string.h>

extern char   **__current_halvm_domain_name;
extern size_t   __current_halvm_domain_name_length;

static char halvm_name[] = "HaLVM";
static char halvm_release[] = "3.0.0"; /* FIXME: Put the real version here */
static char halvm_version[] = "HaLVM 3.0.0 (x86_64/KVM)"; /* FIXME */
static char halvm_host[] = "x86_64"; /* FIXME: Put the real host here */

int syscall_uname(struct utsname *buf)
{
  size_t copylen = (__current_halvm_domain_name_length > sizeof(buf->nodename))
                 ? sizeof(buf->nodename)
                 : __current_halvm_domain_name_length;

  memset(buf, 0, sizeof(struct utsname));
  memcpy(buf->sysname,  halvm_name, sizeof(halvm_name));
  memcpy(buf->nodename, __current_halvm_domain_name, copylen);
  memcpy(buf->release,  halvm_release, sizeof(halvm_release));
  memcpy(buf->version,  halvm_version, sizeof(halvm_version));
  memcpy(buf->machine,  halvm_host,    sizeof(halvm_host));
#ifdef _GNU_SOURCE
  memcpy(buf->domainname, __current_halvm_domain_name, copylen);
#else
  memcpy(buf->__domainname, __current_halvm_domain_name, copylen);
#endif
}
