#include <unistd.h>
#include <fcntl.h>
#include <stdarg.h>
#include <errno.h>

int halvm_fcntl_dupfd(int, int, int);
int halvm_fcntl_getflags(int);
int halvm_fcntl_setflags(int, int);
int halvm_fcntl_getstatus(int);
int halvm_fcntl_setstatus(int, int);
int halvm_fcntl_setlock(int, struct flock *, int);
int halvm_fcntl_setlock(int, struct flock *, int);
int halvm_fcntl_getlock(int, struct flock *);
int halvm_fcntl_ofd_setlock(int, struct flock *, int);
int halvm_fcntl_ofd_setlock(int, struct flock *, int);
int halvm_fcntl_ofd_getlock(int, struct flock *);
int halvm_fcntl_getowner(int);
int halvm_fcntl_setowner(int, int);
int halvm_fcntl_getowner_ex(int, struct f_owner_ex *);
int halvm_fcntl_setowner_ex(int, struct f_owner_ex *);
int halvm_fcntl_getsig(int);
int halvm_fcntl_setsig(int, int);
int halvm_fcntl_setlease(int, int);
int halvm_fcntl_getlease(int);
int halvm_fcntl_notify(int, int);
int halvm_fcntl_setpipesize(int, int);
int halvm_fcntl_getpipesize(int);
int halvm_fcntl_add_seals(int, int);
int halvm_fcntl_get_seals(int);

int syscall_fcntl(int fd, int cmd, ...)
{
  struct f_owner_ex *owninfo;
  struct flock *lock;
  va_list args;
  int intarg;

  va_start(args, cmd);
  switch(cmd) {
    case F_DUPFD:
      intarg = va_arg(args, int);
      return halvm_fcntl_dupfd(fd, intarg, 0);
    case F_DUPFD_CLOEXEC:
      intarg = va_arg(args, int);
      return halvm_fcntl_dupfd(fd, intarg, O_CLOEXEC);
    case F_GETFD:
      return halvm_fcntl_getflags(fd);
    case F_SETFD:
      intarg = va_arg(args, int);
      return halvm_fcntl_setflags(fd, intarg);
    case F_GETFL:
      return halvm_fcntl_getstatus(fd);
    case F_SETFL:
      intarg = va_arg(args, int);
      return halvm_fcntl_setstatus(fd, intarg);
    case F_SETLK:
      lock = va_arg(args, struct flock *);
      return halvm_fcntl_setlock(fd, lock, 0);
    case F_SETLKW:
      lock = va_arg(args, struct flock *);
      return halvm_fcntl_setlock(fd, lock, 1);
    case F_GETLK:
      lock = va_arg(args, struct flock *);
      return halvm_fcntl_getlock(fd, lock);
    case F_OFD_SETLK:
      lock = va_arg(args, struct flock *);
      return halvm_fcntl_ofd_setlock(fd, lock, 0);
    case F_OFD_SETLKW:
      lock = va_arg(args, struct flock *);
      return halvm_fcntl_ofd_setlock(fd, lock, 1);
    case F_OFD_GETLK:
      lock = va_arg(args, struct flock *);
      return halvm_fcntl_ofd_getlock(fd, lock);
    case F_GETOWN:
      return halvm_fcntl_getowner(fd);
    case F_SETOWN:
      intarg = va_arg(args, int);
      return halvm_fcntl_setowner(fd, intarg);
    case F_GETOWN_EX:
      owninfo = va_arg(args, struct f_owner_ex *);
      return halvm_fcntl_getowner_ex(fd, owninfo);
    case F_SETOWN_EX:
      owninfo = va_arg(args, struct f_owner_ex *);
      return halvm_fcntl_setowner_ex(fd, owninfo);
    case F_GETSIG:
      return halvm_fcntl_getsig(fd);
    case F_SETSIG:
      intarg = va_arg(args, int);
      return halvm_fcntl_setsig(fd, intarg);
    case F_SETLEASE:
      intarg = va_arg(args, int);
      return halvm_fcntl_setlease(fd, intarg);
    case F_GETLEASE:
      return halvm_fcntl_getlease(fd);
    case F_NOTIFY:
      intarg = va_arg(args, int);
      return halvm_fcntl_notify(fd, intarg);
    case F_SETPIPE_SZ:
      intarg = va_arg(args, int);
      return halvm_fcntl_setpipesize(fd, intarg);
    case F_GETPIPE_SZ:
      return halvm_fcntl_getpipesize(fd);
    case F_ADD_SEALS:
      intarg = va_arg(args, int);
      return halvm_fcntl_add_seals(fd, intarg);
    case F_GET_SEALS:
      return halvm_fcntl_get_seals(fd);
    default:
      errno = EINVAL;
      return -1;
  }
}
