#include <errno.h>

int syscall_setns(int fd, int nstype)
{
  errno = EINVAL;
  return -1;
}
