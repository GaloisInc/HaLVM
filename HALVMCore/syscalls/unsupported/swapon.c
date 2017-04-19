#include <errno.h>

int syscall_swapon(const char *path, int swapflags)
{
  errno = EPERM;
  return -1;
}
