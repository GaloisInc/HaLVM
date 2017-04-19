#include <errno.h>

int syscall_execve(const char *filename, char *const argv[],
                   char *const envp[])
{
  errno = ENOMEM;
  return -1;
}
