#define __NEED_pid_t
#include <sys/ptrace.h>
#include <errno.h>

long syscall_ptrace(int request, pid_t pid,
                    void *addr, void *data)
{
  errno = EINVAL;
  return -1;
}
