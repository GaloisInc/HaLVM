#include <sys/ipc.h>
#include <errno.h>

int syscall_msgget(key_t key, int msgflg)
{
  errno = (msgflg & IPC_CREAT) ? ENOMEM : ENOENT;
  return -1;
}
