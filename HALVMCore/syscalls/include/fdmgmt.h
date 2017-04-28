#ifndef HALVM_FD_MANAGEMENT_API
#define HALVM_FD_MANAGEMENT_API

#define __NEED_ssize_t
#define __NEED_size_t
#include <sys/types.h>
#include <platform_reqs.h>
#include <Rts.h>
#include <stdarg.h>

enum fd_type {
  closed,
  tty,
  network_socket,
  file_descriptor,
  pipe
};

struct tty_dispatch {
  ssize_t  (*read)(enum which_tty context, void *buf, size_t nbyte);
  ssize_t  (*write)(enum which_tty context, const void *buf, size_t nbyte);
  int      (*ioctl)(enum which_tty context, unsigned long, va_list);
  enum which_tty context;
};

struct socket_dispatch {
  StgStablePtr networkStack;
  StgStablePtr socketInfo;

  int     (*ioctl)(StgStablePtr, StgStablePtr, unsigned long, ...);
};

struct file_dispatch {
  StgStablePtr fileSystem;
  StgStablePtr fdInfo;

  int     (*ioctl)(StgStablePtr, StgStablePtr, unsigned long, va_list);
  int     (*fcntl)(StgStablePtr, StgStablePtr, int,           va_list);
};

struct pipe_info {};
enum pipe_direction { read_pipe, write_pipe };

struct pipe_dispatch {
  enum pipe_direction dir;
  struct pipe_info *info;

  int (*fcntl)(enum pipe_direction, struct pipe_info *, int, va_list);
};

// Generate new file descriptor of the given type, dispatch table, and context.
// RETURNS:
//   > 0: The new file descriptor / scoket / whatever
//   -1 : An error, with an appropriate value in errno
int new_fd(enum fd_type type, void *dispatch);
int is_open_fd(int fd);

enum   fd_type          get_fd_type(int);
struct tty_dispatch    *get_tty_dispatch(int);
struct socket_dispatch *get_socket_dispatch(int);
struct file_dispatch   *get_file_dispatch(int);
struct pipe_dispatch   *get_pipe_dispatch(int);

#endif
