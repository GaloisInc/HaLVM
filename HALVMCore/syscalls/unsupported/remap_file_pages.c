#define __NEED_size_t
#include <bits/alltypes.h>
#include <errno.h>

int syscall_remap_file_pages(void *addr, size_t size, int prot,
                             size_t pgoff, int flags)
{
  errno = EINVAL;
  return -1;
}
