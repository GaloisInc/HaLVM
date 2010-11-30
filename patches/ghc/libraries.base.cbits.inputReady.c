*** ghc-6.10.1/libraries/base/cbits/inputReady.c	2008-11-03 10:13:59.000000000 -0800
--- xen-ghc/libraries/base/cbits/inputReady.c	2009-01-13 10:20:58.000000000 -0800
***************
*** 8,13 ****
--- 8,15 ----
  /* #include "PosixSource.h" */
  #include "HsBase.h"
  
+ #ifndef xen_HOST_OS
+ 
  /*
   * inputReady(fd) checks to see whether input is available on the file
   * descriptor 'fd'.  Input meaning 'can I safely read at least a
***************
*** 165,168 ****
          }
      }
  #endif
! }    
--- 167,172 ----
          }
      }
  #endif
! }
! 
! #endif
