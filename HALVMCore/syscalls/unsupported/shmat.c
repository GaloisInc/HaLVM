#include <errno.h>

void *syscall_shmat(int shmid, const void *shmaddr, int shmflg)
{
  errno = EINVAL;
  return (void*)-1;
}
