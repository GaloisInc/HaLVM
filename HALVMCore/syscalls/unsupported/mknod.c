#define __NEED_mode_t
#define __NEED_dev_t
#include <bits/alltypes.h>
#include <errno.h>

int syscall_mknod(const char *pathname, mode_t mode, dev_t dev)
{
  errno = ENOMEM;
  return -1;
}
