#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <errno.h>

pid_t syscall_wait4(pid_t pid, int *wstatus, int options, struct rusage *rusage)
{
  errno = ECHILD;
  return -1;
}
