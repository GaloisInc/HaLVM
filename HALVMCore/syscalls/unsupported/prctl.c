#include <errno.h>

int syscall_prctl(int option, unsigned long arg2, unsigned long arg3,
                  unsigned long arg4, unsigned long arg5)
{
  errno = EINVAL;
  return -1;
}
