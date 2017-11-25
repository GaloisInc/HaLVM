#include <unistd.h>
#include <fcntl.h>
#include <stdarg.h>

int syscall_openat4(int, const char*, int, mode_t);

int syscall_openat(int fd, const char *filename, int flags, ...)
{
  mode_t mode = 0;

  if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
    va_list ap;
    va_start(ap, flags);
    mode = va_arg(ap, mode_t);
    va_end(ap);
  }

  return syscall_openat4(fd, filename, flags, mode);
}
