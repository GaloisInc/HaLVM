*** ghc-pristine/libraries/base/include/HsBase.h	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/include/HsBase.h	2010-07-15 14:47:14.151205669 -0700
***************
*** 170,187 ****
  INLINE int __hscore_get_errno(void) { return errno; }
  INLINE void __hscore_set_errno(int e) { errno = e; }
  
! #if !defined(_MSC_VER)
  INLINE int __hscore_s_isreg(mode_t m)  { return S_ISREG(m);  }
  INLINE int __hscore_s_isdir(mode_t m)  { return S_ISDIR(m);  }
  INLINE int __hscore_s_isfifo(mode_t m) { return S_ISFIFO(m); }
  INLINE int __hscore_s_isblk(mode_t m)  { return S_ISBLK(m);  }
  INLINE int __hscore_s_ischr(mode_t m)  { return S_ISCHR(m);  }
! #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  INLINE int __hscore_s_issock(mode_t m) { return S_ISSOCK(m); }
! #endif
! #endif
  
! #if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32)
  INLINE int
  __hscore_sigemptyset( sigset_t *set )
  { return sigemptyset(set); }
--- 170,189 ----
  INLINE int __hscore_get_errno(void) { return errno; }
  INLINE void __hscore_set_errno(int e) { errno = e; }
  
! #if !defined(xen_HOST_OS)
! # if !defined(_MSC_VER)
  INLINE int __hscore_s_isreg(mode_t m)  { return S_ISREG(m);  }
  INLINE int __hscore_s_isdir(mode_t m)  { return S_ISDIR(m);  }
  INLINE int __hscore_s_isfifo(mode_t m) { return S_ISFIFO(m); }
  INLINE int __hscore_s_isblk(mode_t m)  { return S_ISBLK(m);  }
  INLINE int __hscore_s_ischr(mode_t m)  { return S_ISCHR(m);  }
! #  if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  INLINE int __hscore_s_issock(mode_t m) { return S_ISSOCK(m); }
! #  endif // !(mingw32_HOST_OS || __MINGW32) 
! # endif // !_MSC_VER 
! #endif // !xen_HOST_OS
  
! #if !defined(_MSC_VER) && !defined(__MINGW32__) && !defined(_WIN32) && !defined(xen_HOST_OS)
  INLINE int
  __hscore_sigemptyset( sigset_t *set )
  { return sigemptyset(set); }
***************
*** 216,221 ****
--- 218,225 ----
  __hscore_memcpy_src_off( char *dst, char *src, int src_off, size_t sz )
  { return memcpy(dst, src+src_off, sz); }
  
+ #ifndef xen_HOST_OS
+ 
  INLINE HsInt
  __hscore_bufsiz()
  {
***************
*** 365,374 ****
--- 369,382 ----
  #endif
  }
  
+ #endif /* !xen_HOST_OS */
+ 
  #if __GLASGOW_HASKELL__
  
  #endif /* __GLASGOW_HASKELL__ */
  
+ #ifndef xen_HOST_OS
+ 
  #if defined(__MINGW32__)
  // We want the versions of stat/fstat/lseek that use 64-bit offsets,
  // and you have to ask for those explicitly.  Unfortunately there
***************
*** 387,392 ****
--- 395,401 ----
    return sizeof(struct_stat);
  }
  
+ 
  INLINE time_t __hscore_st_mtime ( struct_stat* st ) { return st->st_mtime; }
  INLINE stsize_t __hscore_st_size  ( struct_stat* st ) { return st->st_size; }
  #if !defined(_MSC_VER)
***************
*** 455,460 ****
--- 464,471 ----
  }
  #endif
  
+ #endif /* !xen_HOST_OS */
+ 
  INLINE int
  __hscore_echo( void )
  {
***************
*** 532,537 ****
--- 543,550 ----
  #endif
  }
  
+ #ifndef xen_HOST_OS
+ 
  #ifndef __MINGW32__
  INLINE size_t __hscore_sizeof_siginfo_t (void)
  {
***************
*** 539,544 ****
--- 552,559 ----
  }
  #endif
  
+ #endif /* !xen_HOST_OS */
+ 
  INLINE int
  __hscore_f_getfl( void )
  {
***************
*** 579,584 ****
--- 594,601 ----
  #endif
  }
  
+ #ifndef xen_HOST_OS
+ 
  // defined in rts/RtsStartup.c.
  extern void* __hscore_get_saved_termios(int fd);
  extern void __hscore_set_saved_termios(int fd, void* ts);
***************
*** 629,634 ****
--- 646,653 ----
  	return (select(nfds,readfds,writefds,exceptfds,timeout));
  }
  
+ #endif /* !xen_HOST_OS */
+ 
  // gettimeofday()-related
  
  #if !defined(__MINGW32__)
***************
*** 666,672 ****
  void errorBelch2(const char*s, char *t);
  void debugBelch2(const char*s, char *t);
  
! #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  
  INLINE int fcntl_read(int fd, int cmd) {
      return fcntl(fd, cmd);
--- 685,691 ----
  void errorBelch2(const char*s, char *t);
  void debugBelch2(const char*s, char *t);
  
! #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__) && !defined(xen_HOST_OS)
  
  INLINE int fcntl_read(int fd, int cmd) {
      return fcntl(fd, cmd);
