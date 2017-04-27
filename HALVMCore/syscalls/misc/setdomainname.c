#include <unistd.h>
#include <errno.h>

char   **__current_halvm_domain_name        = NULL;
size_t   __current_halvm_domain_name_length = 0;

int syscall_setdomainname(const char *name, size_t namelen)
{
  // FIXME: Check to see if the pointer is mapped, return EFAULT if not
  //
  if(namelen < 0) {
    errno = EINVAL;
    return -1;
  }

  if(namelen > __current_halvm_domain_name_length)
    __current_halvm_domain_name = realloc(__current_halvm_domain_name, namelen);

  if(__current_halvm_domain_name == NULL) {
    // this is non-traditional
    errno = ENOMEM;
    return -1;
  }

  __current_halvm_domain_name_length = namelen;
  memcpy(__current_halvm_domain_name, name, namelen);
  return 0;
}
