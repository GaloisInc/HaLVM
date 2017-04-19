#define __NEED_pid_t
#include <sys/uio.h>
#include <errno.h>

ssize_t syscall_process_vm_readv(pid_t pid, const struct iovec *local_iov,
                                 unsigned long liovcnt,
                                 const struct iovec *remote_iov,
                                 unsigned long riovcnt,
                                 unsigned long flags)
{
  errno = EPERM;
  return -1;
}

