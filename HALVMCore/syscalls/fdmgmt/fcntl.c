#include <fdmgmt.h>
#include <fcntl.h>
#include <errno.h>
#include <stdarg.h>

int syscall_fcntl(int fd, int cmd, ...)
{
  va_list args;
  int retval;

  if(!is_open_fd(fd)) {
    errno = EBADF;
    return -1;
  }

  va_start(args, cmd);
  switch(get_fd_type(fd)) {
    case file_descriptor: {
        struct file_dispatch *fdisp = get_file_dispatch(fd);
        fdisp->fcntl(fdisp->fileSystem, fdisp->fdInfo, cmd, args);
        break;
      }
    case pipe: {
        struct pipe_dispatch *pdisp = get_pipe_dispatch(fd);
        pdisp->fcntl(pdisp->dir, pdisp->info, cmd, args);
        break;
      }
    default:
      errno = EBADF;
      retval = -1;
  }
  va_end(args);

  return retval;
}
