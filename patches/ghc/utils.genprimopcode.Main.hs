*** ghc-pristine/utils/genprimopcode/Main.hs	2010-06-09 11:10:14.000000000 -0700
--- xen-ghc/utils/genprimopcode/Main.hs	2010-07-02 14:00:43.352499029 -0700
***************
*** 634,640 ****
--- 634,642 ----
  ppType (TyApp "RealWorld"   []) = "realWorldTy"
  ppType (TyApp "ThreadId#"   []) = "threadIdPrimTy"
  ppType (TyApp "ForeignObj#" []) = "foreignObjPrimTy"
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  ppType (TyApp "BCO#"        []) = "bcoPrimTy"
+ #endif
  ppType (TyApp "()"          []) = "unitTy" 	-- unitTy is TysWiredIn's name for ()
  
  ppType (TyVar "a")               = "alphaTy"
