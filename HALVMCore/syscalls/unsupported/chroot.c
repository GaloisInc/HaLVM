#include <errno.h>

int syscall_chroot(const char *path)
{
  errno = EPERM;
  return -1;
}
