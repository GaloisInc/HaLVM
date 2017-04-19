#define __NEED_pid_t
#include <bits/alltypes.h>
#include <errno.h>

pid_t syscall_setsid(void)
{
  errno = EPERM;
  return -1;
}
