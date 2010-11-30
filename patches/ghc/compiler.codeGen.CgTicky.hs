*** ghc-pristine/compiler/codeGen/CgTicky.hs	2010-06-09 11:10:08.000000000 -0700
--- xen-ghc/compiler/codeGen/CgTicky.hs	2010-07-14 17:51:28.712967645 -0700
***************
*** 36,42 ****
         staticTickyHdr,
    ) where
  
! #include "../includes/DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import ClosureInfo
--- 36,42 ----
         staticTickyHdr,
    ) where
  
! #include "DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import ClosureInfo
