#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <fdmgmt.h>
#include <platform_reqs.h>

struct fd_entry {
  enum fd_type type;
  union {
    struct { int next_free; }  closed;
    struct tty_dispatch       *open_tty;
    struct socket_dispatch    *open_socket;
    struct file_dispatch      *open_file;
    struct pipe_dispatch      *open_pipe;
  } info;
};

static struct tty_dispatch stdin_dispatch = {
    .read    = &platform_tty_read
  , .write   = &platform_tty_write
  , .ioctl   = &platform_tty_ioctl
  , .context = tty_in
};

static struct tty_dispatch stdout_dispatch = {
    .read    = &platform_tty_read
  , .write   = &platform_tty_write
  , .ioctl   = &platform_tty_ioctl
  , .context = tty_out
};

static struct tty_dispatch stderr_dispatch = {
    .read    = &platform_tty_read
  , .write   = &platform_tty_write
  , .ioctl   = &platform_tty_ioctl
  , .context = tty_err
};

static struct fd_entry default_table[] =
  { /* 0 */ { .type = tty,     .info.open_tty         = &stdin_dispatch }
  , /* 1 */ { .type = tty,     .info.open_tty         = &stdout_dispatch }
  , /* 2 */ { .type = tty,     .info.open_tty         = &stderr_dispatch }
  , /* 3 */ { .type = closed,  .info.closed.next_free = 4 }
  , /* 4 */ { .type = closed,  .info.closed.next_free = 5 }
  , /* 5 */ { .type = closed,  .info.closed.next_free = 6 }
  , /* 6 */ { .type = closed,  .info.closed.next_free = 7 }
  , /* 7 */ { .type = closed,  .info.closed.next_free = 8 }
  , /* 8 */ { .type = closed,  .info.closed.next_free = 9 }
  , /* 9 */ { .type = closed,  .info.closed.next_free = 0 }
  };

static struct fd_entry *fdtable           = default_table;
static unsigned int     fdtable_size      = 10;
static int              fdtable_next_free = 3;

int new_fd(enum fd_type new_fd_type, void *dispatch)
{
  int retval;

  if(fdtable_next_free == 0) {
    struct fd_entry *newfdt = calloc(fdtable_size+10, sizeof(struct fd_entry));
    int i;

    if(!newfdt) {
      errno = ENFILE;
      return -1;
    }

    memcpy(newfdt, fdtable, fdtable_size * sizeof(struct fd_entry));

    for(i = fdtable_next_free; i < (fdtable_size + 10); i++) {
      newfdt[i].type                  = closed;
      newfdt[i].info.closed.next_free = fdtable_next_free;
      fdtable_next_free                = i;
    }

    /* this is awkward */
    if(fdtable != default_table)
      free(fdtable);

    fdtable = newfdt;
  }

  /* get the next one, update our free list */
  retval = fdtable_next_free;
  assert(fdtable[retval].type == closed);
  fdtable_next_free = fdtable[retval].info.closed.next_free;

  /* set in the state we need */
  fdtable[retval].type = new_fd_type;
  switch(new_fd_type) {
    case closed:          assert(0);
    case tty:             fdtable[retval].info.open_tty = dispatch;
    case network_socket:  break;
    case file_descriptor: break;
    case pipe:            break;
    default:
      assert(0);
  }

  return retval;
}

int is_open_fd(int fd)
{
  if(fd < 0)
    return 0;

  if(fd >= fdtable_size)
    return 0;

  return fdtable[fd].type != closed;
}

enum fd_type get_fd_type(int fd)
{
  if(fd > fdtable_size)
    return closed;

  if(fd < 0)
    return closed;

  return fdtable[fd].type;
}

struct tty_dispatch *get_tty_dispatch(int fd)
{
  if(get_fd_type(fd) == tty)
    return fdtable[fd].info.open_tty;
  else
    return NULL;
}

struct socket_dispatch *get_socket_dispatch(int fd)
{
  if(get_fd_type(fd) == network_socket)
    return fdtable[fd].info.open_socket;
  else
    return NULL;
}

struct file_dispatch *get_file_dispatch(int fd)
{
  if(get_fd_type(fd) == file_descriptor)
    return fdtable[fd].info.open_file;
  else
    return NULL;
}

struct pipe_dispatch *get_pipe_dispatch(int fd)
{
  if(get_fd_type(fd) == pipe)
    return fdtable[fd].info.open_pipe;
  else
    return NULL;
}
