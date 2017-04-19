#include <errno.h>

int syscall_init_module(void *module_image, unsigned long len,
                        const char *param_values)
{
  errno = EPERM;
  return -1;
}
