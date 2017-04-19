#define __NEED_key_t
#define __NEED_gid_t
#define __NEED_pid_t
#define __NEED_uid_t
#define __NEED_mode_t
#define __NEED_size_t
#define __NEED_time_t
#include <bits/alltypes.h>
#include <bits/ipc.h>
#include <bits/shm.h>
#include <errno.h>

int syscall_shmctl(int shmid, int cmd, struct shmid_ds *buf)
{
  errno = EINVAL;
  return -1;
}
