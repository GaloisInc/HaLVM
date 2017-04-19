#include <errno.h>

int syscall_unshare(int flags)
{
  errno = EPERM;
  return -1;
}
