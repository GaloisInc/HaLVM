*** ghc-pristine/libraries/base/System/Posix/Internals.hs	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/System/Posix/Internals.hs	2010-07-20 17:34:05.064916524 -0700
***************
*** 222,228 ****
          throwErrnoIfMinus1Retry_ "tcSetAttr"
             (c_tcgetattr fd p_tios)
  
! #ifdef __GLASGOW_HASKELL__
          -- Save a copy of termios, if this is a standard file descriptor.
          -- These terminal settings are restored in hs_exit().
          when (fd <= 2) $ do
--- 222,228 ----
          throwErrnoIfMinus1Retry_ "tcSetAttr"
             (c_tcgetattr fd p_tios)
  
! #if defined(__GLASGOW_HASKELL__) && !defined(xen_HOST_OS)
          -- Save a copy of termios, if this is a standard file descriptor.
          -- These terminal settings are restored in hs_exit().
          when (fd <= 2) $ do
***************
*** 253,259 ****
                   c_sigprocmask const_sig_setmask p_old_sigset nullPtr
               return r
  
! #ifdef __GLASGOW_HASKELL__
  foreign import ccall unsafe "HsBase.h __hscore_get_saved_termios"
     get_saved_termios :: CInt -> IO (Ptr CTermios)
  
--- 253,259 ----
                   c_sigprocmask const_sig_setmask p_old_sigset nullPtr
               return r
  
! #if defined(__GLASGOW_HASKELL__) && !defined(xen_HOST_OS)
  foreign import ccall unsafe "HsBase.h __hscore_get_saved_termios"
     get_saved_termios :: CInt -> IO (Ptr CTermios)
  
***************
*** 355,360 ****
--- 355,362 ----
  type CFilePath = CWString
  #endif
  
+ #ifndef xen_HOST_OS
+ 
  foreign import ccall unsafe "HsBase.h access"
     c_access :: CString -> CInt -> IO CInt
  
***************
*** 379,392 ****
--- 381,428 ----
  foreign import ccall unsafe "HsBase.h isatty"
     c_isatty :: CInt -> IO CInt
  
+ #else
+ 
+ c_access :: CString -> CInt -> IO CInt
+ c_access  = error "No c_access in HaLVM"
+ 
+ c_chmod :: CString -> CMode -> IO CInt
+ c_chmod  = error "No c_chmod in HaLVM"
+ 
+ c_close :: CInt -> IO CInt
+ c_close  = error "No c_close in HaLVM"
+ 
+ c_creat :: CString -> CMode -> IO CInt
+ c_creat  = error "No c_creat in HaLVM"
+ 
+ c_dup :: CInt -> IO CInt
+ c_dup  = error "No c_dup in HaLVM"
+ 
+ c_dup2 :: CInt -> CInt -> IO CInt
+ c_dup2  = error "No c_dup2 in HaLVM"
+ 
+ c_fstat :: CInt -> Ptr CStat -> IO CInt
+ c_fstat  = error "No c_fstat in HaLVM"
+ 
+ c_isatty :: CInt -> IO CInt
+ c_isatty  = error "No c_isatty in HaLVM"
+ 
+ #endif
+ 
+ 
  #if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  foreign import ccall unsafe "HsBase.h __hscore_lseek"
     c_lseek :: CInt -> Int64 -> CInt -> IO Int64
+ #elif defined(xen_HOST_OS)
+ c_lseek :: CInt -> COff -> CInt -> IO COff
+ c_lseek _ _ _ = error "No c_lseek for the HaLVM!"
  #else
  foreign import ccall unsafe "HsBase.h __hscore_lseek"
     c_lseek :: CInt -> COff -> CInt -> IO COff
  #endif
  
+ #ifndef xen_HOST_OS
+ 
  foreign import ccall unsafe "HsBase.h __hscore_lstat"
     lstat :: CFilePath -> Ptr CStat -> IO CInt
  
***************
*** 417,426 ****
--- 453,522 ----
  foreign import ccall unsafe "HsBase.h unlink"
     c_unlink :: CString -> IO CInt
  
+ #else
+ 
+ lstat :: CFilePath -> Ptr CStat -> IO CInt
+ lstat  = error "No lstat on HaLVM"
+ 
+ c_open :: CFilePath -> CInt -> CMode -> IO CInt
+ c_open  = error "No c_open on HaLVM"
+ 
+ c_read, c_safe_read :: CInt -> Ptr Word8 -> CSize -> IO CSsize
+ c_read      = error "No c_read on HaLVM"
+ c_safe_read = error "No c_safe_read on HaLVM"
+ 
+ c_stat :: CFilePath -> Ptr CStat -> IO CInt
+ c_stat  = error "No c_stat on HaLVM"
+ 
+ c_umask :: CMode -> IO CInt
+ c_umask  = error "No c_umask on HaLVM"
+ 
+ c_write, c_safe_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
+ c_write      = error "No c_write on HaLVM"
+ c_safe_write = error "No c_safe_write on HaLVM"
+ 
+ c_ftruncate :: CInt -> COff -> IO CInt
+ c_ftruncate  = error "No c_ftruncate on HaLVM"
+ 
+ c_unlink :: CString -> IO ()
+ c_unlink  = error "No c_unlink on HaLVM"
+ 
+ #endif
+ 
  foreign import ccall unsafe "HsBase.h getpid"
     c_getpid :: IO CPid
  
  #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
+ # ifdef xen_HOST_OS
+ c_fcntl_read  :: CInt -> CInt -> IO CInt
+ c_fcntl_read _ _ = fail "c_fcntl_read doesn't exist for the HaLVM!"
+ c_fcntl_write :: CInt -> CInt -> CLong -> IO CInt
+ c_fcntl_write _ _ _ = fail "c_fcntl_write doesn't exist for the HaLVM!"
+ c_fcntl_lock  :: CInt -> CInt -> Ptr CFLock -> IO CInt
+ c_fcntl_lock _ _ _ = fail "c_fcntl_lock doesn't exist for the HaLVM!"
+ c_fork :: IO CPid 
+ c_fork = fail "c_fork doesn't exist for the HaLVM!"
+ c_link :: CString -> CString -> IO CInt
+ c_link _ _ = fail "c_link doesn't exist for the HaLVM!"
+ c_mkfifo :: CString -> CMode -> IO CInt
+ c_mkfifo _ _ = fail "c_mkfifo doesn't exist for the HaLVM!"
+ c_pipe :: Ptr CInt -> IO CInt
+ c_pipe _ = fail "c_pipe doesn't exist for the HaLVM!"
+ c_sigemptyset :: Ptr CSigset -> IO CInt
+ c_sigemptyset _ = fail "c_sigemptyset doesn't exist for the HaLVM!"
+ c_sigaddset :: Ptr CSigset -> CInt -> IO CInt
+ c_sigaddset _ _ = fail "c_sigaddset doestn't exist for the HaLVM!"
+ c_sigprocmask :: CInt -> Ptr CSigset -> Ptr CSigset -> IO CInt
+ c_sigprocmask _ _ _ = fail "c_sigprocmask doesn't exist for the HaLVM!"
+ c_tcgetattr :: CInt -> Ptr CTermios -> IO CInt
+ c_tcgetattr _ _ = fail "c_tcgetattr doesn't exist for the HaLVM!"
+ c_tcsetattr :: CInt -> CInt -> Ptr CTermios -> IO CInt
+ c_tcsetattr _ _ _ = fail "c_tcsetattr doesn't exist for the HaLVM!"
+ c_utime :: CString -> Ptr CUtimbuf -> IO CInt
+ c_utime _ _ = fail "c_utime doesn't exist for the HaLVM!"
+ c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
+ c_waitpid _ _ _ = fail "c_waitpid doesn't exist for the HaLVM!"
+ # else
  foreign import ccall unsafe "HsBase.h fcntl_read"
     c_fcntl_read  :: CInt -> CInt -> IO CInt
  
***************
*** 462,470 ****
--- 558,583 ----
  
  foreign import ccall unsafe "HsBase.h waitpid"
     c_waitpid :: CPid -> Ptr CInt -> CInt -> IO CPid
+ # endif
  #endif
  
  -- POSIX flags only:
+ #ifdef xen_HOST_OS
+ o_RDONLY :: CInt
+ o_RDONLY  = 0x00000000
+ o_WRONLY :: CInt
+ o_WRONLY  = 0x00000001
+ o_RDWR   :: CInt
+ o_RDWR    = 0x00000002
+ o_APPEND :: CInt
+ o_APPEND  = 0x00002000
+ o_CREAT  :: CInt
+ o_CREAT   = 0x00000100
+ o_EXCL   :: CInt
+ o_EXCL    = 0x00000200
+ o_TRUNC  :: CInt
+ o_TRUNC   = 0x00001000
+ #else
  foreign import ccall unsafe "HsBase.h __hscore_o_rdonly" o_RDONLY :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_o_wronly" o_WRONLY :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_o_rdwr"   o_RDWR   :: CInt
***************
*** 472,479 ****
--- 585,612 ----
  foreign import ccall unsafe "HsBase.h __hscore_o_creat"  o_CREAT  :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_o_excl"   o_EXCL   :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_o_trunc"  o_TRUNC  :: CInt
+ #endif
  
  -- non-POSIX flags.
+ #ifdef xen_HOST_OS
+ o_NOCTTY   :: CInt
+ o_NOCTTY    = 0x00000400
+ o_NONBLOCK :: CInt
+ o_NONBLOCK  = 0x00004000
+ o_BINARY   :: CInt
+ o_BINARY    = 0x00010000
+ 
+ c_s_isreg  :: CMode -> CInt
+ c_s_isreg _ = error "why are you using c_s_isreg with the HaLVM?"
+ c_s_ischr  :: CMode -> CInt
+ c_s_ischr _ = error "why are you using c_s_ischr with the HaLVM?"
+ c_s_isblk  :: CMode -> CInt
+ c_s_isblk _ = error "Why are you using c_s_isblk with the HaLVM?"
+ c_s_isdir  :: CMode -> CInt
+ c_s_isdir _ = error "Why are you using c_s_isdir with the HaLVM?"
+ c_s_isfifo :: CMode -> CInt
+ c_s_isfifo _ = error "Why are you using c_s_isfifo with the HaLVM?"
+ #else
  foreign import ccall unsafe "HsBase.h __hscore_o_noctty"   o_NOCTTY   :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_o_nonblock" o_NONBLOCK :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_o_binary"   o_BINARY   :: CInt
***************
*** 483,488 ****
--- 616,622 ----
  foreign import ccall unsafe "HsBase.h __hscore_s_isblk"  c_s_isblk  :: CMode -> CInt
  foreign import ccall unsafe "HsBase.h __hscore_s_isdir"  c_s_isdir  :: CMode -> CInt
  foreign import ccall unsafe "HsBase.h __hscore_s_isfifo" c_s_isfifo :: CMode -> CInt
+ #endif
  
  s_isreg  :: CMode -> Bool
  s_isreg cm = c_s_isreg cm /= 0
***************
*** 495,500 ****
--- 629,636 ----
  s_isfifo :: CMode -> Bool
  s_isfifo cm = c_s_isfifo cm /= 0
  
+ #ifndef xen_HOST_OS
+ 
  foreign import ccall unsafe "HsBase.h __hscore_sizeof_stat" sizeof_stat :: Int
  foreign import ccall unsafe "HsBase.h __hscore_st_mtime" st_mtime :: Ptr CStat -> IO CTime
  #ifdef mingw32_HOST_OS
***************
*** 506,511 ****
--- 642,669 ----
  foreign import ccall unsafe "HsBase.h __hscore_st_dev" st_dev :: Ptr CStat -> IO CDev
  foreign import ccall unsafe "HsBase.h __hscore_st_ino" st_ino :: Ptr CStat -> IO CIno
  
+ #else
+ 
+ sizeof_stat :: Int
+ sizeof_stat  = 0
+ 
+ st_mtime :: Ptr CStat -> IO CTime
+ st_mtime  = error "No st_mtime on HaLVM"
+ 
+ st_size :: Ptr CStat -> IO COff
+ st_size  = error "No st_size on HaLVM"
+ 
+ st_mode :: Ptr CStat -> IO CMode
+ st_mode  = error "No st_mode on HaLVM"
+ 
+ st_dev :: Ptr CStat -> IO CDev
+ st_dev  = error "No st_dev on HaLVM"
+ 
+ st_ino :: Ptr CStat -> IO CIno
+ st_ino  = error "No st_ino on HaLVM"
+ 
+ #endif
+ 
  foreign import ccall unsafe "HsBase.h __hscore_echo"         const_echo :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_tcsanow"      const_tcsanow :: CInt
  foreign import ccall unsafe "HsBase.h __hscore_icanon"       const_icanon :: CInt
***************
*** 520,542 ****
  foreign import ccall unsafe "HsBase.h __hscore_fd_cloexec"   const_fd_cloexec :: CLong
  
  #if defined(HTYPE_TCFLAG_T)
  foreign import ccall unsafe "HsBase.h __hscore_sizeof_termios"  sizeof_termios :: Int
  foreign import ccall unsafe "HsBase.h __hscore_sizeof_sigset_t" sizeof_sigset_t :: Int
  
  foreign import ccall unsafe "HsBase.h __hscore_lflag" c_lflag :: Ptr CTermios -> IO CTcflag
  foreign import ccall unsafe "HsBase.h __hscore_poke_lflag" poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
  foreign import ccall unsafe "HsBase.h __hscore_ptr_c_cc" ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
  #endif
  
  s_issock :: CMode -> Bool
! #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  s_issock cmode = c_s_issock cmode /= 0
  foreign import ccall unsafe "HsBase.h __hscore_s_issock" c_s_issock :: CMode -> CInt
  #else
  s_issock _ = False
  #endif
  
  foreign import ccall unsafe "__hscore_bufsiz"   dEFAULT_BUFFER_SIZE :: Int
  foreign import ccall unsafe "__hscore_seek_cur" sEEK_CUR :: CInt
  foreign import ccall unsafe "__hscore_seek_set" sEEK_SET :: CInt
  foreign import ccall unsafe "__hscore_seek_end" sEEK_END :: CInt
--- 678,724 ----
  foreign import ccall unsafe "HsBase.h __hscore_fd_cloexec"   const_fd_cloexec :: CLong
  
  #if defined(HTYPE_TCFLAG_T)
+ #ifndef xen_HOST_OS
  foreign import ccall unsafe "HsBase.h __hscore_sizeof_termios"  sizeof_termios :: Int
  foreign import ccall unsafe "HsBase.h __hscore_sizeof_sigset_t" sizeof_sigset_t :: Int
  
  foreign import ccall unsafe "HsBase.h __hscore_lflag" c_lflag :: Ptr CTermios -> IO CTcflag
  foreign import ccall unsafe "HsBase.h __hscore_poke_lflag" poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
  foreign import ccall unsafe "HsBase.h __hscore_ptr_c_cc" ptr_c_cc  :: Ptr CTermios -> IO (Ptr Word8)
+ #else
+ sizeof_termios, sizeof_sigset_t :: Int
+ sizeof_termios  = error "No sizeof_termios in HaLVM"
+ sizeof_sigset_t = error "No sizeof_sigset_t in HaLVM"
+ 
+ c_lflag :: Ptr CTermios -> IO CTcflag
+ c_lflag  = error "No c_lflag on HaLVM"
+ 
+ poke_c_lflag :: Ptr CTermios -> CTcflag -> IO ()
+ poke_c_lflag  = error "No poke_c_lflag on HaLVM"
+ 
+ ptr_c_cc :: Ptr CTermios -> IO (Ptr Word8)
+ ptr_c_cc  = error "No ptr_c_cc  on HaLVM"
+ #endif
  #endif
  
  s_issock :: CMode -> Bool
! #if !defined(mingw32_HOST_OS) && !defined(__MINGW32__) && !defined(xen_HOST_OS)
  s_issock cmode = c_s_issock cmode /= 0
  foreign import ccall unsafe "HsBase.h __hscore_s_issock" c_s_issock :: CMode -> CInt
  #else
  s_issock _ = False
  #endif
  
+ #ifdef xen_HOST_OS
+ dEFAULT_BUFFER_SIZE :: Int
+ dEFAULT_BUFFER_SIZE  = error "No dEFAULT_BUFFER_SIZE in HaLVM"
+ sEEK_CUR,sEEK_SET,sEEK_END :: CInt
+ sEEK_CUR = error "No sEEK_CUR in HaLVM"
+ sEEK_SET = error "No sEEK_SET in HaLVM"
+ sEEK_END = error "No sEEK_END in HaLVM"
+ #else
  foreign import ccall unsafe "__hscore_bufsiz"   dEFAULT_BUFFER_SIZE :: Int
  foreign import ccall unsafe "__hscore_seek_cur" sEEK_CUR :: CInt
  foreign import ccall unsafe "__hscore_seek_set" sEEK_SET :: CInt
  foreign import ccall unsafe "__hscore_seek_end" sEEK_END :: CInt
+ #endif
