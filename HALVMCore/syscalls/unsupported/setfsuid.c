#define __NEED_uid_t
#include <sys/fsuid.h>
#include <errno.h>

int syscall_setfsuid(uid_t fsgid)
{
  return 22; // chosen by not caring, again
}
