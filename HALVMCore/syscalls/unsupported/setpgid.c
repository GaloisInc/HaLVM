#define __NEED_pid_t
#include <bits/alltypes.h>
#include <errno.h>

int syscall_setpgid(pid_t pid, pid_t pgid)
{
  errno = EPERM;
  return -1;
}
