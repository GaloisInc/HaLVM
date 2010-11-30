diff -cr a/Main.hs b/Main.hs
*** a/Main.hs	2009-02-19 05:07:52.000000000 -0800
--- b/Main.hs	2009-04-15 18:42:48.000000000 -0700
***************
*** 181,187 ****
    (comp, conf) <- configCompilerAux configFlags'
    configure verbosity
              (configPackageDB' configFlags') (globalRepos globalFlags')
!             comp conf configFlags' configExFlags' extraArgs
  
  installAction :: (ConfigFlags, ConfigExFlags, InstallFlags)
                -> [String] -> GlobalFlags -> IO ()
--- 181,188 ----
    (comp, conf) <- configCompilerAux configFlags'
    configure verbosity
              (configPackageDB' configFlags') (globalRepos globalFlags')
!             comp (savedAuxPrograms config)
+             conf configFlags' configExFlags' extraArgs
  
  installAction :: (ConfigFlags, ConfigExFlags, InstallFlags)
                -> [String] -> GlobalFlags -> IO ()
***************
*** 205,211 ****
    (comp, conf) <- configCompilerAux configFlags'
    install verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
!           comp conf configFlags' configExFlags' installFlags'
            [ UnresolvedDependency pkg (configConfigurationsFlags configFlags')
            | pkg <- pkgs ]
  
--- 206,213 ----
    (comp, conf) <- configCompilerAux configFlags'
    install verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
!           comp (savedAuxPrograms config)
+           conf configFlags' configExFlags' installFlags'
            [ UnresolvedDependency pkg (configConfigurationsFlags configFlags')
            | pkg <- pkgs ]
  
***************
*** 265,271 ****
    (comp, conf) <- configCompilerAux configFlags'
    upgrade verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
!           comp conf configFlags' configExFlags' installFlags'
            [ UnresolvedDependency pkg (configConfigurationsFlags configFlags')
            | pkg <- pkgs ]
  
--- 267,274 ----
    (comp, conf) <- configCompilerAux configFlags'
    upgrade verbosity
            (configPackageDB' configFlags') (globalRepos globalFlags')
!           comp (savedAuxPrograms config)
+           conf configFlags' configExFlags' installFlags'
            [ UnresolvedDependency pkg (configConfigurationsFlags configFlags')
            | pkg <- pkgs ]
