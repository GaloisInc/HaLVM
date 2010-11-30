*** ghc-6.12.3/compiler/prelude/primops.txt.pp	2010-06-09 11:10:10.000000000 -0700
--- xen-ghc/compiler/prelude/primops.txt.pp	2010-07-01 16:06:10.584696068 -0700
***************
*** 1619,1631 ****
--- 1619,1634 ----
  	{Support for the bytecode interpreter and linker.}
  ------------------------------------------------------------------------
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  primtype BCO#
     {Primitive bytecode type.}
+ #endif
  
  primop   AddrToHValueOp "addrToHValue#" GenPrimOp
     Addr# -> (# a #)
     {Convert an {\tt Addr\#} to a followable type.}
  
+ #if (defined(xen_HOST_OS) && defined(ALLOW_INTERPRETER)) || !defined(xen_HOST_OS)
  primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
     BCO# -> (# a #)
     with
***************
*** 1636,1641 ****
--- 1639,1645 ----
     with
     has_side_effects = True
     out_of_line      = True
+ #endif
  
  primop  UnpackClosureOp "unpackClosure#" GenPrimOp
     a -> (# Addr#, Array# b, ByteArray# #)
