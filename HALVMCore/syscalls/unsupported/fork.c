#define __NEED_pid_t
#include <bits/alltypes.h>
#include <errno.h>

pid_t syscall_fork(void)
{
  errno = ENOSYS;
  return -1;
}
