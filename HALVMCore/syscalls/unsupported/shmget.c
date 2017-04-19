#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

int syscall_shmget(key_t key, size_t size, int shmflg)
{
  errno = EACCES;
  return -1;
}
