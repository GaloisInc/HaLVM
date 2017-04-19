#include <errno.h>

int syscall_swapoff(const char *path)
{
  errno = EINVAL;
  return -1;
}
