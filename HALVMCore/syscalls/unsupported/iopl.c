#include <errno.h>

int syscall_iopl(int level)
{
  errno = EPERM;
  return -1;
}
