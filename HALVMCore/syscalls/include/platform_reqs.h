#ifndef HALVM_PLATFORM_DEPS
#define HALVM_PLATFORM_DEPS

#define __NEED_ssize_t
#include <bits/alltypes.h>
#include <stdarg.h>

/*******************************************************************************
 * USER CONSOLE
 *
 * The HaLVM likes to have a console to dump messages on. For silly Unix
 * reasons, we'd love it if this could be split into stdin/stdout/stderr,
 * but we're not going to require it. Instead, all we ask for is a single
 * set of read nad write functions that we can dispatch to, where the first
 * argument tells you what console this is. If you want to dispatch based on
 * that information, great. If that's too complicated, don't worry about it.
 *
 */
enum which_tty { tty_in, tty_out, tty_err };

ssize_t platform_tty_read(enum which_tty, void *, size_t);
ssize_t platform_tty_write(enum which_tty, const void *, size_t);
/* if you'd like, you can also write this, as well, but it's not required;
 * we have a default implementation that will be included (weakly) if you
 * don't define it.
 *
 * If you do choose to implement this, please try hard to return something
 * interesting for TIOCGWINSZ, so that isatty() and friends work right */
int     platform_tty_ioctl(enum which_tty, unsigned long, va_list);

#endif
