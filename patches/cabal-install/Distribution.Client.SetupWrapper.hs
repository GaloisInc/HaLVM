diff -cr a/Distribution/Client/SetupWrapper.hs b/Distribution/Client/SetupWrapper.hs
*** a/Distribution/Client/SetupWrapper.hs	2009-02-19 05:07:52.000000000 -0800
--- b/Distribution/Client/SetupWrapper.hs	2009-04-15 18:37:31.000000000 -0700
***************
*** 62,67 ****
--- 62,69 ----
           ( display )
  import Distribution.Verbosity
           ( Verbosity )
+ import Distribution.Client.Config
+          ( AuxProgram(..) )
  
  import System.Directory  ( doesFileExist, getCurrentDirectory )
  import System.FilePath   ( (</>), (<.>) )
***************
*** 70,76 ****
  import System.Process    ( runProcess, waitForProcess )
  import Control.Monad     ( when, unless )
  import Data.List         ( maximumBy )
! import Data.Maybe        ( fromMaybe, isJust )
  import Data.Monoid       ( Monoid(mempty) )
  import Data.Char         ( isSpace )
  
--- 68,74 ----
  import System.Process    ( runProcess, waitForProcess )
  import Control.Monad     ( when, unless )
  import Data.List         ( maximumBy )
! import Data.Maybe        ( fromMaybe, isJust, catMaybes )
  import Data.Monoid       ( Monoid(mempty) )
  import Data.Char         ( isSpace )
  
***************
*** 80,85 ****
--- 82,88 ----
      usePackageDB     :: PackageDBStack,
      usePackageIndex  :: Maybe (PackageIndex InstalledPackage),
      useProgramConfig :: ProgramConfiguration,
+     useAuxPrograms   :: [AuxProgram],
      useDistPref      :: FilePath,
      useLoggingHandle :: Maybe Handle,
      useWorkingDir    :: Maybe FilePath
***************
*** 92,97 ****
--- 95,101 ----
      usePackageDB     = [GlobalPackageDB, UserPackageDB],
      usePackageIndex  = Nothing,
      useProgramConfig = emptyProgramConfiguration,
+     useAuxPrograms   = [],
      useDistPref      = defaultDistPref,
      useLoggingHandle = Nothing,
      useWorkingDir    = Nothing
***************
*** 113,126 ****
--- 113,131 ----
                                            (descCabalVersion pkg)
                      }
        buildType'  = fromMaybe Custom (buildType pkg)
+       auxProgArgs = concatMap generateAuxProgramArgs (useAuxPrograms options)
        mkArgs cabalLibVersion = commandName cmd
                               : commandShowOptions cmd (flags cabalLibVersion)
+                             ++ auxProgArgs
                              ++ extraArgs
    setupMethod verbosity options' (packageId pkg) buildType' mkArgs
    where
      getPkg = findPackageDesc (fromMaybe "." (useWorkingDir options))
           >>= readPackageDescription verbosity
           >>= return . packageDescription
+     generateAuxProgramArgs (AuxProgram n path args) = catMaybes $
+       [fmap (\ p -> "--with-"++n++"="++p) path,
+        fmap (\ a -> "--"++n++"-options="++show a) args]
  
  -- | Decide if we're going to be able to do a direct internal call to the
  -- entry point in the Cabal library or if we're going to have to compile
