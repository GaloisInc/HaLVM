--- a/Distribution/System.hs	2010-06-15 07:09:39.000000000 -0700
+++ b/Distribution/System.hs	2010-07-20 11:01:06.097908508 -0700
@@ -67,7 +67,7 @@
 knownOSs :: [OS]
 knownOSs = [Linux, Windows, OSX
            ,FreeBSD, OpenBSD, NetBSD
-           ,Solaris, AIX, HPUX, IRIX]
+           ,Solaris, AIX, HPUX, IRIX, OtherOS "Xen"]
 
 osAliases :: ClassificationStrictness -> OS -> [String]
 osAliases Permissive Windows = ["mingw32", "cygwin32"]
@@ -75,6 +75,7 @@
 osAliases _          OSX     = ["darwin"]
 osAliases Permissive FreeBSD = ["kfreebsdgnu"]
 osAliases Permissive Solaris = ["solaris2"]
+osAliases _          (OtherOS "Xen") = ["xen"]
 osAliases _          _       = []
 
 instance Text OS where
@@ -94,7 +95,7 @@
             , name <- display os : osAliases strictness os ]
 
 buildOS :: OS
-buildOS = classifyOS Permissive System.Info.os
+buildOS = OtherOS "Xen" -- classifyOS Permissive System.Info.os
 
 -- ------------------------------------------------------------
 -- * Machine Architecture
