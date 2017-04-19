#include <sys/types.h>
#include <sys/shm.h>
#include <errno.h>

int syscall_shmdt(const void *shmaddr)
{
  errno = EINVAL;
  return -1;
}
