#include <fdmgmt.h>
#include <errno.h>

int syscall_dup(int oldfd)
{
  enum fd_type fdtype;

  if(!is_open_fd(oldfd)) {
    errno = EBADF;
    return -1;
  }
  
  /* this is tedious but safe, as opposed to probably-safe, sketchy, and
   * much shorter */
  fdtype = get_fd_type(oldfd);
  switch(get_fd_type(oldfd)) {
    case tty:             return new_fd(fdtype, get_tty_dispatch(oldfd));
    case network_socket:  return new_fd(fdtype, get_socket_dispatch(oldfd));
    case file_descriptor: return new_fd(fdtype, get_file_dispatch(oldfd));
    case pipe:            return new_fd(fdtype, get_pipe_dispatch(oldfd));
    default:
      errno = EBADF;
      return -1;
  }
}
