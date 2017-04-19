#include <sys/capability.h>
#include <errno.h>

int syscall_capget(cap_user_header_t hdrp, cap_user_data_t datap)
{
  errno = EINVAL;
  return -1;
}
