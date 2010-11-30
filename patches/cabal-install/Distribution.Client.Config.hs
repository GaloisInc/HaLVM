*** b/Distribution/Client/Config.hs	2009-02-19 05:07:52.000000000 -0800
--- a/Distribution/Client/Config.hs	2009-09-08 17:58:04.000000000 -0700
***************
*** 17,23 ****
      showConfig,
      showConfigWithComments,
      parseConfig,
! 
      defaultCabalDir,
      defaultConfigFile,
      defaultCacheDir,
--- 17,24 ----
      showConfig,
      showConfigWithComments,
      parseConfig,
!     
!     AuxProgram(..),
      defaultCabalDir,
      defaultConfigFile,
      defaultCacheDir,
***************
*** 46,52 ****
           ( FieldDescr(..), liftField
           , ParseResult(..), locatedErrorMsg, showPWarning
           , readFields, warning, lineNo
!          , simpleField, listField, parseFilePathQ, showFilePath, parseTokenQ )
  import qualified Distribution.ParseUtils as ParseUtils
           ( Field(..) )
  import qualified Distribution.Text as Text
--- 47,54 ----
           ( FieldDescr(..), liftField
           , ParseResult(..), locatedErrorMsg, showPWarning
           , readFields, warning, lineNo
!          , simpleField, listField, parseFilePathQ, showFilePath, parseTokenQ 
!          , showFreeText, parseFreeText )
  import qualified Distribution.ParseUtils as ParseUtils
           ( Field(..) )
  import qualified Distribution.Text as Text
***************
*** 57,63 ****
           ( CommandUI(commandOptions), commandDefaultFlags, ShowOrParseArgs(..)
           , viewAsFieldDescr, OptionField, option, reqArg )
  import Distribution.Simple.Program
!          ( defaultProgramConfiguration )
  import Distribution.Simple.Utils
           ( notice, warn, lowercase )
  import Distribution.Compiler
--- 59,65 ----
           ( CommandUI(commandOptions), commandDefaultFlags, ShowOrParseArgs(..)
           , viewAsFieldDescr, OptionField, option, reqArg )
  import Distribution.Simple.Program
!          ( defaultProgramConfiguration, builtinPrograms, Program(..) )
  import Distribution.Simple.Utils
           ( notice, warn, lowercase )
  import Distribution.Compiler
***************
*** 102,110 ****
      savedConfigureExFlags  :: ConfigExFlags,
      savedUserInstallDirs   :: InstallDirs (Flag PathTemplate),
      savedGlobalInstallDirs :: InstallDirs (Flag PathTemplate),
!     savedUploadFlags       :: UploadFlags
    }
  
  instance Monoid SavedConfig where
    mempty = SavedConfig {
      savedGlobalFlags       = mempty,
--- 104,119 ----
      savedConfigureExFlags  :: ConfigExFlags,
      savedUserInstallDirs   :: InstallDirs (Flag PathTemplate),
      savedGlobalInstallDirs :: InstallDirs (Flag PathTemplate),
!     savedUploadFlags       :: UploadFlags,
!     savedAuxPrograms       :: [AuxProgram]
    }
  
+ data AuxProgram = AuxProgram {
+                     auxName :: String
+                   , auxPath :: Maybe FilePath
+                   , auxArgs :: Maybe String
+                   }
+ 
  instance Monoid SavedConfig where
    mempty = SavedConfig {
      savedGlobalFlags       = mempty,
***************
*** 113,119 ****
      savedConfigureExFlags  = mempty,
      savedUserInstallDirs   = mempty,
      savedGlobalInstallDirs = mempty,
!     savedUploadFlags       = mempty
    }
    mappend a b = SavedConfig {
      savedGlobalFlags       = combine savedGlobalFlags,
--- 122,129 ----
      savedConfigureExFlags  = mempty,
      savedUserInstallDirs   = mempty,
      savedGlobalInstallDirs = mempty,
!     savedUploadFlags       = mempty,
!     savedAuxPrograms       = mempty
    }
    mappend a b = SavedConfig {
      savedGlobalFlags       = combine savedGlobalFlags,
***************
*** 122,128 ****
      savedConfigureExFlags  = combine savedConfigureExFlags,
      savedUserInstallDirs   = combine savedUserInstallDirs,
      savedGlobalInstallDirs = combine savedGlobalInstallDirs,
!     savedUploadFlags       = combine savedUploadFlags
    }
      where combine field = field a `mappend` field b
  
--- 132,139 ----
      savedConfigureExFlags  = combine savedConfigureExFlags,
      savedUserInstallDirs   = combine savedUserInstallDirs,
      savedGlobalInstallDirs = combine savedGlobalInstallDirs,
!     savedUploadFlags       = combine savedUploadFlags,
!     savedAuxPrograms       = combine savedAuxPrograms
    }
      where combine field = field a `mappend` field b
  
***************
*** 309,315 ****
      },
      savedUserInstallDirs   = fmap toFlag userInstallDirs,
      savedGlobalInstallDirs = fmap toFlag globalInstallDirs,
!     savedUploadFlags       = commandDefaultFlags uploadCommand
    }
  
  -- | All config file fields.
--- 313,320 ----
      },
      savedUserInstallDirs   = fmap toFlag userInstallDirs,
      savedGlobalInstallDirs = fmap toFlag globalInstallDirs,
!     savedUploadFlags       = commandDefaultFlags uploadCommand,
!     savedAuxPrograms       = []
    }
  
  -- | All config file fields.
***************
*** 345,350 ****
--- 350,372 ----
         (commandOptions uploadCommand ParseArgs)
         ["verbose", "check"] []
  
+   ++ [ simpleField ("with-" ++ n) showFilePath' parseFilePathQ
+                    (getProgramPath n . savedAuxPrograms)
+                    (\ v sc -> sc{ savedAuxPrograms =
+                                     setProgramPath n v (savedAuxPrograms sc) })
+      | prog <- builtinPrograms
+      , let n = programName prog
+      , n `notElem` ["ghc","ghc-pkg"]
+      ]
+ 
+   ++ [ simpleField (n ++ "-opts") showFreeText parseFreeText
+                    (getProgramArgs n . savedAuxPrograms)
+                    (\ v sc -> sc{ savedAuxPrograms =
+                                    setProgramArgs n v (savedAuxPrograms sc) })
+      | prog <- builtinPrograms
+      , let n = programName prog
+      ]
+ 
    where
      toSavedConfig lift options exclusions replacements =
        [ lift (fromMaybe field replacement)
***************
*** 354,359 ****
--- 376,402 ----
              replacement = find ((== name) . fieldName) replacements
        , name `notElem` exclusions ]
      optional = Parse.option mempty . fmap toFlag
+     --
+     getProgramPath _ [] = ""
+     getProgramPath n ((AuxProgram m r _):rest)
+       | n == m    = maybe "" id r
+       | otherwise = getProgramPath n rest
+     getProgramArgs _ [] = ""
+     getProgramArgs n ((AuxProgram m _ a):rest)
+       | n == m    = maybe "" id a
+       | otherwise = getProgramArgs n rest
+     -- 
+     setProgramPath n p [] = [AuxProgram n (Just p) Nothing]
+     setProgramPath n p (f@(AuxProgram m _ a):r)
+       | n == m    = (AuxProgram n (Just p) a) : r
+       | otherwise = f : (setProgramPath n p r)
+     setProgramArgs n a [] = [AuxProgram n Nothing (Just a)]
+     setProgramArgs n a (f@(AuxProgram m p _):r)
+       | n == m    = (AuxProgram n p (Just a)) : r
+       | otherwise = f : (setProgramArgs n a r)
+     --
+     showFilePath' "" = showFreeText ""
+     showFilePath' x  = showFilePath x
  
  -- TODO: next step, make the deprecated fields elicit a warning.
  --
