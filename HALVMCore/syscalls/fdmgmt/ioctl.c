#include <stdarg.h>
#include <errno.h>
#include <fdmgmt.h>

int syscall_ioctl(int fd, unsigned long request, ...)
{
  struct tty_dispatch *ttyd;
  struct socket_dispatch *sockd;
  struct file_dispatch *filed;
  va_list args;
  int res;

  va_start(args, request);
  switch(get_fd_type(fd)) {
    case closed:
      errno = EBADF;
      res = -1;
      break;

    case tty:
      ttyd = get_tty_dispatch(fd);
      assert(ttyd);
      res = ttyd.ioctl(ttyd.context, request, args);
      break;

    case network_socket:
      sockd = get_socket_dispatch(fd);
      assert(sockd);
      res = sockd.ioctl(sockd.networkStack, sockd.socketInfo, request, args);
      break;

    case file_descriptor:
      filed = get_file_dispatch(fd);
      assert(filed);
      res = filed.ioctl(filed.fileSystem, filed.fdInfo, request, args);
      break;

    case pipe:
      errno = ENOTTY;
      res = -1;
      break;
  }
  va_end(args);

  return res;
}
