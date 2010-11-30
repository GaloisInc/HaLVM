*** ghc-pristine/compiler/codeGen/StgCmmTicky.hs	2010-06-09 11:10:09.000000000 -0700
--- xen-ghc/compiler/codeGen/StgCmmTicky.hs	2010-07-14 17:53:14.696281150 -0700
***************
*** 39,45 ****
    ) where
  
  #include "HsVersions.h"
! #include "../includes/DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import StgCmmClosure
--- 39,45 ----
    ) where
  
  #include "HsVersions.h"
! #include "DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import StgCmmClosure
