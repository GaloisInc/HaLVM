#define __NEED_uid_t
#include <sys/fsuid.h>
#include <errno.h>

int syscall_setfsgid(uid_t fsgid)
{
  return 17; // chosen by not caring
}
