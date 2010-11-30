*** ghc-pristine/compiler/codeGen/CgProf.hs	2010-06-09 11:10:08.000000000 -0700
--- xen-ghc/compiler/codeGen/CgProf.hs	2010-07-14 17:50:47.017721937 -0700
***************
*** 24,35 ****
    ) where
  
  #include "HsVersions.h"
! #include "../includes/MachDeps.h"
   -- For WORD_SIZE_IN_BITS only.
! #include "../includes/rts/Constants.h"
  	-- For LDV_CREATE_MASK, LDV_STATE_USE
  	-- which are StgWords
! #include "../includes/DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import ClosureInfo
--- 24,35 ----
    ) where
  
  #include "HsVersions.h"
! #include "MachDeps.h"
   -- For WORD_SIZE_IN_BITS only.
! #include "rts/Constants.h"
  	-- For LDV_CREATE_MASK, LDV_STATE_USE
  	-- which are StgWords
! #include "DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import ClosureInfo
