*** ghc-pristine/libraries/base/GHC/IO/Encoding/Iconv.hs	2010-06-09 11:10:18.000000000 -0700
--- xen-ghc/libraries/base/GHC/IO/Encoding/Iconv.hs	2010-07-19 11:25:33.310916774 -0700
***************
*** 113,118 ****
--- 113,120 ----
  -- value -1, which is a possible return value from iconv_open.
  type IConv = CLong -- ToDo: (#type iconv_t)
  
+ #ifndef xen_HOST_OS
+ 
  foreign import ccall unsafe "hs_iconv_open"
      hs_iconv_open :: CString -> CString -> IO IConv
  
***************
*** 126,131 ****
--- 128,150 ----
  foreign import ccall unsafe "localeEncoding"
      c_localeEncoding :: IO CString
  
+ #else
+ 
+ hs_iconv_open :: CString -> CString -> IO IConv
+ hs_iconv_open  = error "No hs_iconv_open on HaLVM"
+ 
+ hs_iconv_close :: IConv -> IO CInt
+ hs_iconv_close  = error "No hs_iconv_close on HaLVM"
+ 
+ hs_iconv :: IConv -> Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize
+          -> IO CSize
+ hs_iconv  = error "No hs_iconv on HaLVM"
+ 
+ c_localeEncoding :: IO CString
+ c_localeEncoding  = error "No c_localeEncoding on HaLVM"
+ 
+ #endif
+ 
  haskellChar :: String
  #ifdef WORDS_BIGENDIAN
  haskellChar | charSize == 2 = "UTF-16BE"
