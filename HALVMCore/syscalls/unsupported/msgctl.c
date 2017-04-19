#include <errno.h>

int syscall_msgctl(int msgid, int cmd, void *buf)
{
  errno = EINVAL;
  return -1;
}
