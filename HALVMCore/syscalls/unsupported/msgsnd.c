#define __NEED_size_t
#define __NEED_ssize_t
#include <bits/alltypes.h>
#include <errno.h>

int syscall_msgsnd(int msqid, const void *msgp, size_t msgsz, int msgflg)
{
  errno = EINVAL;
  return -1;
}
