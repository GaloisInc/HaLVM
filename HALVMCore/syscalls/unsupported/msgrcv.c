#define __NEED_size_t
#define __NEED_ssize_t
#include <bits/alltypes.h>
#include <errno.h>

ssize_t syscall_msgrcv(int msqid, void *msgp, size_t msgsz,
                       long msgtyp, int msgflg)
{
  errno = EINVAL;
  return -1;
}
