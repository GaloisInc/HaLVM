*** ghc-6.10.1/utils/genapply/GenApply.hs	2008-11-03 10:13:13.000000000 -0800
--- xen-ghc/utils/genapply/GenApply.hs	2009-01-13 17:32:46.000000000 -0800
***************
*** 511,516 ****
--- 511,517 ----
          text "switch [INVALID_OBJECT .. N_CLOSURE_TYPES] (TO_W_(%INFO_TYPE(%STD_INFO(info)))) {",
  	nest 4 (vcat [
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  --    if fast == 1:
  --        print "    bco_lbl:"
  --    else:
***************
*** 523,528 ****
--- 524,530 ----
  		args all_args_size fun_info_label {- tag stmt -}False
  	 ]),
  	text "}",
+ #endif
  
  --    if fast == 1:
  --        print "    fun_lbl:"
