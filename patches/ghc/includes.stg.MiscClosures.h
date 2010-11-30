*** ghc-pristine/includes/stg/MiscClosures.h	2010-06-09 11:10:12.000000000 -0700
--- xen-ghc/includes/stg/MiscClosures.h	2010-07-02 10:52:35.177435764 -0700
***************
*** 97,103 ****
--- 97,105 ----
  RTS_INFO(__stg_EAGER_BLACKHOLE_info);
  RTS_INFO(stg_CAF_BLACKHOLE_info);
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  RTS_FUN_INFO(stg_BCO_info);
+ #endif
  RTS_INFO(stg_EVACUATED_info);
  RTS_INFO(stg_WEAK_info);
  RTS_INFO(stg_DEAD_WEAK_info);
***************
*** 146,152 ****
--- 148,156 ----
  RTS_ENTRY(stg_BLACKHOLE_entry);
  RTS_ENTRY(__stg_EAGER_BLACKHOLE_entry);
  RTS_ENTRY(stg_CAF_BLACKHOLE_entry);
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  RTS_ENTRY(stg_BCO_entry);
+ #endif
  RTS_ENTRY(stg_EVACUATED_entry);
  RTS_ENTRY(stg_WEAK_entry);
  RTS_ENTRY(stg_DEAD_WEAK_entry);
***************
*** 573,580 ****
--- 577,586 ----
  RTS_FUN(stg_finalizzeWeakzh);
  RTS_FUN(stg_deRefWeakzh);
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  RTS_FUN(stg_newBCOzh);
  RTS_FUN(stg_mkApUpd0zh);
+ #endif
  
  RTS_FUN(stg_retryzh);
  RTS_FUN(stg_catchRetryzh);
