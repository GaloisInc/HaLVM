#include <sys/capability.h>
#include <errno.h>

int syscall_capset(cap_user_header_t hdrp, const cap_user_data_t datap)
{
  errno = EINVAL;
  return -1;
}
