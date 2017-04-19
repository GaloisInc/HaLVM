#include <errno.h>

int syscall_vhangup(void)
{
  errno = EPERM;
  return -1;
}
