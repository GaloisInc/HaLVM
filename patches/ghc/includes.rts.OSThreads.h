*** ghc-pristine/includes/rts/OSThreads.h	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/rts/OSThreads.h	2010-07-02 10:55:17.367268693 -0700
***************
*** 147,152 ****
--- 147,171 ----
  
  #endif // CMINUSMINUS
  
+ # elif defined(xen_HOST_OS)
+ 
+ #define OSThreadProcAttr  /* */
+ 
+ #if CMINUSMINUS
+ #define ACQUIRE_LOCK(mutex) foreign "C" halvm_acquire_lock(mutex)
+ #define RELEASE_LOCK(mutex) foreign "C" halvm_release_lock(mutex)
+ #define ASSERT_LOCK_HELD(mutex) /* nothing */
+ #else
+ typedef unsigned long Condition;
+ typedef unsigned long Mutex;
+ typedef unsigned long OSThreadId;
+ typedef unsigned long ThreadLocalKey;
+ 
+ #define ACQUIRE_LOCK(mutex) halvm_acquire_lock(mutex)
+ #define RELEASE_LOCK(mutex) halvm_release_lock(mutex)
+ #define ASSERT_LOCK_HELD(mutex) /* nothing */
+ #endif
+ 
  # else
  #  error "Threads not supported"
  # endif
