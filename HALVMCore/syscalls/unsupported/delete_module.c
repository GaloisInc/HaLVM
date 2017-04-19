#include <errno.h>

int syscall_delete_module(const char *name, int flags)
{
  errno = EPERM;
  return -1;
}
