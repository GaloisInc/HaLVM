*** ghc-pristine/compiler/codeGen/StgCmmProf.hs	2010-06-09 11:10:09.000000000 -0700
--- xen-ghc/compiler/codeGen/StgCmmProf.hs	2010-07-14 17:52:18.120768252 -0700
***************
*** 25,36 ****
    ) where
  
  #include "HsVersions.h"
! #include "../includes/MachDeps.h"
   -- For WORD_SIZE_IN_BITS only.
! #include "../includes/rts/Constants.h"
  	-- For LDV_CREATE_MASK, LDV_STATE_USE
  	-- which are StgWords
! #include "../includes/DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import StgCmmClosure
--- 25,36 ----
    ) where
  
  #include "HsVersions.h"
! #include "MachDeps.h"
   -- For WORD_SIZE_IN_BITS only.
! #include "rts/Constants.h"
  	-- For LDV_CREATE_MASK, LDV_STATE_USE
  	-- which are StgWords
! #include "DerivedConstants.h"
  	-- For REP_xxx constants, which are MachReps
  
  import StgCmmClosure
