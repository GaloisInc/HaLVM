#include <errno.h>

int syscall_acct(const char *filename)
{
  errno = ENOSYS;
  return -1;
}
