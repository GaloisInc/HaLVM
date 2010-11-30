diff -cr a/Distribution/Client/Configure.hs b/Distribution/Client/Configure.hs
*** a/Distribution/Client/Configure.hs	2009-02-19 05:07:52.000000000 -0800
--- b/Distribution/Client/Configure.hs	2009-04-15 18:40:16.000000000 -0700
***************
*** 61,78 ****
           ( Platform, buildPlatform )
  import Distribution.Verbosity as Verbosity
           ( Verbosity )
  
  -- | Configure the package found in the local directory
  configure :: Verbosity
            -> PackageDBStack
            -> [Repo]
            -> Compiler
            -> ProgramConfiguration
            -> ConfigFlags
            -> ConfigExFlags
            -> [String]
            -> IO ()
! configure verbosity packageDBs repos comp conf
    configFlags configExFlags extraArgs = do
  
    installed <- getInstalledPackages verbosity comp packageDBs conf
--- 61,81 ----
           ( Platform(Platform), buildPlatform )
  import Distribution.Verbosity as Verbosity
           ( Verbosity )
+ import Distribution.Client.Config
+          ( AuxProgram )
  
  -- | Configure the package found in the local directory
  configure :: Verbosity
            -> PackageDBStack
            -> [Repo]
            -> Compiler
+           -> [AuxProgram]
            -> ProgramConfiguration
            -> ConfigFlags
            -> ConfigExFlags
            -> [String]
            -> IO ()
! configure verbosity packageDBs repos comp auxprogs conf
    configFlags configExFlags extraArgs = do
  
    installed <- getInstalledPackages verbosity comp packageDBs conf
***************
*** 116,121 ****
--- 119,125 ----
                             then index
                             else Nothing,
        useProgramConfig = conf,
+       useAuxPrograms   = auxprogs,
        useDistPref      = fromFlagOrDefault
                             (useDistPref defaultSetupScriptOptions)
                             (configDistPref configFlags),

