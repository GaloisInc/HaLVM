diff -cr a/Distribution/Client/Install.hs b/Distribution/Client/Install.hs
*** a/Distribution/Client/Install.hs	2009-02-19 05:07:52.000000000 -0800
--- b/Distribution/Client/Install.hs	2009-04-15 18:34:36.000000000 -0700
***************
*** 63,69 ****
           ( ConfigFlags(..), configureCommand, filterConfigureFlags
           , ConfigExFlags(..), InstallFlags(..) )
  import Distribution.Client.Config
!          ( defaultLogsDir, defaultCabalDir )
  import Distribution.Client.Tar (extractTarGzFile)
  import Distribution.Client.Types as Available
           ( UnresolvedDependency(..), AvailablePackage(..)
--- 50,56 ----
           ( ConfigFlags(..), configureCommand, filterConfigureFlags
           , ConfigExFlags(..), InstallFlags(..) )
  import Distribution.Client.Config
!          ( defaultLogsDir, defaultCabalDir, AuxProgram )
  import Distribution.Client.Tar (extractTarGzFile)
  import Distribution.Client.Types as Available
           ( UnresolvedDependency(..), AvailablePackage(..)
***************
*** 138,154 ****
    -> PackageDBStack
    -> [Repo]
    -> Compiler
    -> ProgramConfiguration
    -> ConfigFlags
    -> ConfigExFlags
    -> InstallFlags
    -> [UnresolvedDependency]
    -> IO ()
! install verbosity packageDB repos comp conf
    configFlags configExFlags installFlags deps =
  
    installWithPlanner planner
!         verbosity packageDB repos comp conf
          configFlags configExFlags installFlags
    where
      planner :: Planner
--- 126,143 ----
    -> PackageDBStack
    -> [Repo]
    -> Compiler
+   -> [AuxProgram]
    -> ProgramConfiguration
    -> ConfigFlags
    -> ConfigExFlags
    -> InstallFlags
    -> [UnresolvedDependency]
    -> IO ()
! install verbosity packageDB repos comp auxprogs conf
    configFlags configExFlags installFlags deps =
  
    installWithPlanner planner
!         verbosity packageDB repos comp auxprogs conf
          configFlags configExFlags installFlags
    where
      planner :: Planner
***************
*** 157,167 ****
              | otherwise = planRepoPackages PreferLatestForSelected
                              comp configFlags configExFlags installFlags deps
  
! upgrade verbosity packageDB repos comp conf
    configFlags configExFlags installFlags deps =
  
    installWithPlanner planner
!         verbosity packageDB repos comp conf
          configFlags configExFlags installFlags
    where
      planner :: Planner
--- 146,156 ----
              | otherwise = planRepoPackages PreferLatestForSelected
                              comp configFlags configExFlags installFlags deps
  
! upgrade verbosity packageDB repos comp auxprogs conf
    configFlags configExFlags installFlags deps =
  
    installWithPlanner planner
!         verbosity packageDB repos comp auxprogs conf
          configFlags configExFlags installFlags
    where
      planner :: Planner
***************
*** 181,192 ****
          -> PackageDBStack
          -> [Repo]
          -> Compiler
          -> ProgramConfiguration
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> IO ()
! installWithPlanner planner verbosity packageDBs repos comp conf
    configFlags configExFlags installFlags = do
  
    installed <- getInstalledPackages verbosity comp packageDBs conf
--- 170,182 ----
          -> PackageDBStack
          -> [Repo]
          -> Compiler
+         -> [AuxProgram]
          -> ProgramConfiguration
          -> ConfigFlags
          -> ConfigExFlags
          -> InstallFlags
          -> IO ()
! installWithPlanner planner verbosity packageDBs repos comp auxprogs conf
    configFlags configExFlags installFlags = do
  
    installed <- getInstalledPackages verbosity comp packageDBs conf
***************
*** 252,257 ****
--- 238,244 ----
                             then index
                             else Nothing,
        useProgramConfig = conf,
+       useAuxPrograms   = auxprogs,
        useDistPref      = fromFlagOrDefault
                             (useDistPref defaultSetupScriptOptions)
                             (configDistPref configFlags),
***************
*** 656,672 ****
  
    -- Configure phase
    onFailure ConfigureFailed $ do
!     setup configureCommand configureFlags
  
    -- Build phase
      onFailure BuildFailed $ do
!       setup buildCommand' buildFlags
  
    -- Doc generation phase
        docsResult <- if shouldHaddock
!         then (do setup haddockCommand haddockFlags
                   return DocsOk)
                 `catchIO`   (\_ -> return DocsFailed)
                 `catchExit` (\_ -> return DocsFailed)
          else return DocsNotTried
  
--- 656,672 ----
  
    -- Configure phase
    onFailure ConfigureFailed $ do
!     setup configureCommand configureFlags scriptOptions
  
    -- Build phase
      onFailure BuildFailed $ do
!       setup buildCommand' buildFlags scriptOptions
  
    -- Doc generation phase
        docsResult <- if shouldHaddock
!         then (do setup haddockCommand haddockFlags scriptOptions
                   return DocsOk)
                 `catchIO`   (\_ -> return DocsFailed)
                 `catchExit` (\_ -> return DocsFailed)
          else return DocsNotTried
  
***************
*** 678,684 ****
          withWin32SelfUpgrade verbosity configFlags compid pkg $ do
            case rootCmd miscOptions of
              (Just cmd) -> reexec cmd
!             Nothing    -> setup Cabal.installCommand installFlags
            return (Right (BuildOk docsResult testsResult))
  
    where
--- 597,604 ----
          withWin32SelfUpgrade verbosity configFlags compid pkg $ do
            case rootCmd miscOptions of
              (Just cmd) -> reexec cmd
!             Nothing    -> setup Cabal.installCommand installFlags 
!                                 scriptOptions{ useAuxPrograms = [] }
            return (Right (BuildOk docsResult testsResult))
  
    where
***************
*** 701,707 ****
      }
      verbosity' | isJust useLogFile = max Verbosity.verbose verbosity
                 | otherwise         = verbosity
!     setup cmd flags  = do
        logFileHandle <- case useLogFile of
          Nothing          -> return Nothing
          Just mkLogFileName -> do
--- 622,628 ----
      }
      verbosity' | isJust useLogFile = max Verbosity.verbose verbosity
                 | otherwise         = verbosity
!     setup cmd flags so = do
        logFileHandle <- case useLogFile of
          Nothing          -> return Nothing
          Just mkLogFileName -> do
***************
*** 712,719 ****
            return (Just logFile)
  
        setupWrapper verbosity
!         scriptOptions { useLoggingHandle = logFileHandle
!                       , useWorkingDir    = workingDir }
          (Just pkg)
          cmd flags []
      reexec cmd = do
--- 716,723 ----
            return (Just logFile)
  
        setupWrapper verbosity
!         so { useLoggingHandle = logFileHandle
!            , useWorkingDir    = workingDir }
          (Just pkg)
          cmd flags []
      reexec cmd = do
