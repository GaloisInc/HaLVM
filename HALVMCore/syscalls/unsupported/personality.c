#include <errno.h>

int syscall_personality(unsigned long persona)
{
  errno = EINVAL;
  return -1;
}
