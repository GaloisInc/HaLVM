#define __NEED_gid_t
#define __NEED_size_t
#include <bits/alltypes.h>
#include <errno.h>

int syscall_setgroups(size_t size, const gid_t *list)
{
  errno = EPERM;
  return -1;
}
