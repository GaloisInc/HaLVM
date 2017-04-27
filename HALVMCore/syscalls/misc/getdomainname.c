#include <string.h>
#include <unistd.h>
#include <errno.h>

extern char   **__current_halvm_domain_name;
extern size_t   __current_halvm_domain_name_length;

int syscall_getdomainname(char *name, size_t namelen)
{
  size_t copylen = (namelen < __current_halvm_domain_name_length) ? namelen :
                      __current_halvm_domain_name_length;

  if(name == NULL) {
    errno = EINVAL;
    return -1;
  }

  memcpy(name, __current_halvm_domain_name, copylen);
  return 0;
}
