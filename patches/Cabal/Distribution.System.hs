*** pristine-Cabal/Distribution/System.hs	2012-10-03 08:45:36.000000000 -0700
--- Cabal-1.16.0.1/Distribution/System.hs	2012-10-19 14:56:18.983580093 -0700
***************
*** 82,87 ****
--- 82,88 ----
  osAliases _          OSX     = ["darwin"]
  osAliases Permissive FreeBSD = ["kfreebsdgnu"]
  osAliases Permissive Solaris = ["solaris2"]
+ osAliases _          HaLVM   = ["xen"]
  osAliases _          _       = []
  
  instance Text OS where
***************
*** 101,107 ****
              , name <- display os : osAliases strictness os ]
  
  buildOS :: OS
! buildOS = classifyOS Permissive System.Info.os
  
  -- ------------------------------------------------------------
  -- * Machine Architecture
--- 102,108 ----
              , name <- display os : osAliases strictness os ]
  
  buildOS :: OS
! buildOS = OtherOS "Xen" -- classifyOS Permissive System.Info.os
  
  -- ------------------------------------------------------------
  -- * Machine Architecture
