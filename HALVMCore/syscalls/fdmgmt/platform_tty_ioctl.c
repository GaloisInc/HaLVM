#include <stdarg.h>
#include <bits/ioctl.h>
#include <platform_reqs.h>
#include <errno.h>

int platform_tty_ioctl(enum which_tty _tty, unsigned long cmd, va_list argp)
  __attribute__((weak))
{
  struct winsize *wsize;

  switch(cmd) {
    case TIOCGWINSZ:
      wsize = va_arg(argp, struct winsize *);
      wsize->ws_row    = 50;
      wsize->ws_col    = 80;
      wsize->ws_xpixel = wsize->ws_col * 8;
      wsize->ws_ypixel = wsize->ws_row * 16;
      return 0;

    default:
      errno = EINVAL;
      return -1;
  }
}
