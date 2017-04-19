#define __NEED_idtype_t
#define __NEED_id_t
#define __NEED_siginfo_t
#include <bits/alltypes.h>
#include <sys/wait.h>
#include <errno.h>

int syscall_waitid(idtype_t idtype, id_t id, siginfo_t *infop, int options)
{
  errno = ECHILD;
  return -1;
}
