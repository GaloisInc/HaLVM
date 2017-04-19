#include <errno.h>

int syscall_pivot_root(const char *new_root, const char *put_old)
{
  errno = EPERM;
  return -1;
}
