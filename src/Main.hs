{-# LANGUAGE OverloadedStrings
  , RecordWildCards #-}
import PrepGhcJS.Network.Utils
import PrepGhcJS.Shell.Utils as S
import PrepGhcJS.Cabal.Utils
import Control.Monad
import qualified Data.List as DL
import qualified Text.Printf as T
import qualified Data.Text as T
import Turtle
import Prelude hiding (FilePath)
import PrepGhcJS.Types
import PrepGhcJS.Utils
import qualified Filesystem.Path.CurrentOS as FP

snapshotCache :: FilePath
snapshotCache = "pack"

ltsCfg = PrepConfig
 { master         = "ghc-8.0.tar.gz"
 , workdir        = "work-lts"
 , checkResolver  = lts
 , tag            = "lts"
 , copyIgnore     = ["cabal", "ghc", "ghc-boot", "ghc-boot-th", "ghci", "integer-gmp", "Win32"]
 , forceVersion   = [("integer-gmp", "1.0.0.1")]
 , forceFresh     = [("mtl", "2.2.1"), ("transformers-compat","0.5.1.4")]
 , ghc            = "8.0.1"
 , extraBoot      = []
 , extraBuild     = []
 , nameSuffix     = ""
 , overwriteFiles = [ ("ghcjs-base.cabal","ghcjs-boot/ghcjs/ghcjs-base/")
                    , ("boot.yaml", ghcjsVanila <> "/lib/etc/")
                    ]
 }

nightlyCfg = PrepConfig
 { master         = "ghc-8.0.tar.gz"
 , workdir        = "work-nightly"
 , checkResolver  = nightly
 , tag            = "nightly"
 , copyIgnore     = ["cabal", "ghc", "ghc-boot", "ghc-boot-th", "ghci", "integer-gmp", "Win32"]
 , forceVersion   = [("integer-gmp", "1.0.0.1")]
 , forceFresh     = [("mtl", "2.2.1"), ("transformers-compat","0.5.1.4"),("old-locale","1.0.0.7")]
 , ghc            = "8.0.1"
 , extraBoot      = ["old-locale", "base-compat", "bytestring-builder", "time-locale-compat"] -- aeson
 , extraBuild     = []
 , nameSuffix     = ""
 , overwriteFiles = [ ("ghcjs-base.cabal","ghcjs-boot/ghcjs/ghcjs-base/")
                    , ("boot.yaml", ghcjsVanila <> "/lib/etc/")
                    ]
 }


ghcjsVanila :: Text
ghcjsVanila = "ghcjs-0.2.1"

ghcjsVanilaFP :: FilePath
ghcjsVanilaFP =  FP.fromText ghcjsVanila

sync :: PrepConfig -> IO ()
sync PrepConfig{..} = do
  (d,upd) <- work80 snapshotCache master
  resolver <- checkResolver
  let sres = T.unpack resolver
  let b@(extra, longFilename) = toVer sres d
  print b
  when (upd || True) $ do
    upackTar workdir snapshotCache master
    let path = workdir </> ghcjsVanilaFP
    fixResolver path sres (map T.unpack extraBuild)
    cp (path</> "stack.yaml") (workdir </> "stack.yaml")
    keepPath $ do
      cd workdir
      shell "rm -rf ghcjs-boot" empty
      cp (ghcjsVanilaFP </> "lib/cache/boot.tar") "boot.tar"

      shell' "tar -xf boot.tar"
      shell' "rm -f boot.tar"

      pa <- keepPath $ do
          cd "ghcjs-boot/boot"
          (_,b) <- shellStrict "ls -d */" empty
          mapM (getBootDescr . T.unpack) $ filter (`notElem` copyIgnore ) $ map T.init $ T.lines b
--       print $ map showPkg p
      let p = map showPkg pa
      writeFile "boot-ng.cabal" $ fakePackage ++ DL.intercalate "," (map T.unpack $ extraBoot ++ map fst p)

      shell' "pwd"
      deps <- ghcjsDeps ghcjsVanilaFP resolver
      shell' "pwd"
      updateVersion ghcjsVanilaFP extra
--       mapM_ (uncurry getCabalPackage) deps
      print "global:"
      print deps
--       mapM_ (uncurry getCabalPackage) =<< listDependencies resolver

      echo "\n\n\nMain:"
      bootDeps <- listDependencies resolver
      let canCopy = p `DL.intersect` bootDeps

      putStrLn $ "have " ++ show (DL.sort (p DL.\\ canCopy))

      let need = DL.sort (bootDeps DL.\\ (("boot-ng","0.1.0.0"):canCopy++forceVersion))
      putStrLn $ "need " ++ show need
      keepPath $ do
          cd "ghcjs-boot/boot"
          forM (need ++ forceFresh) getPackage
      shell' "pwd"
      --- here

      mapM_ (\(f,p) -> shell' $ "cp -f ../spec-"<> tag <> "/" <> f <> " " <> p) overwriteFiles

      shell' "tar -cf boot.tar ghcjs-boot"
      shell' $ "cp -f boot.tar " <> ghcjsVanila <> "/lib/cache/"

      let newName = ghcjsVanila <> "." <> T.pack extra
      shell' ("mv " <> ghcjsVanila <> " " <> newName)
      shell' ("tar -zcf archive.tar.gz " <> newName)

      shell' ("scp archive.tar.gz ghcjs-host:/var/www/ghcjs/untested/" <> T.pack longFilename <> nameSuffix <> ".tar.gz")
      shell' ("cp archive.tar.gz ../archive/" <> T.pack longFilename <> nameSuffix <> ".tar.gz")
      shell' ("cp archive.tar.gz " <> newName <> "_ghc-"<> ghc <>".tar.gz")

    return ()

getPackage p@(name, vers) = do
  print p
  let ver = name <> "-" <> vers
  shell' "pwd"
  shell' ("rm -rf " <> name )
  shell' ("stack unpack " <> ver)
  shell' ("mv " <> ver <> " " <> name )
  return ()


shell' cmd = do
   echo cmd
   shell cmd empty
   return ()

     {-

     ./fetch-packages.sh

     cp ghcjs-boot/boot/* new-boot/
     rm -r ghcjs-boot/boot
     mv new-boot ghcjs-boot/boot
     cp -f ghcjs-base.cabal1 ghcjs-boot/ghcjs/ghcjs-base/ghcjs-base.cabal

     echo tar
     tar -cf boot.tar ghcjs-boot
     cp -f boot.yaml ghcjs-0.2.0/lib/etc/
     cp -f boot.tar ghcjs-0.2.0/lib/cache/
     [ -d ghcjs-0.2.0.$EXTRA ] && rm -r ghcjs-0.2.0.$EXTRA
     mv ghcjs-0.2.0 ghcjs-0.2.0.$EXTRA
     tar -zcf archive.tar.gz ghcjs-0.2.0.$EXTRA
     # upload somewhere
ghc-8.0-2016-07-02-nightly-2016-07-02-820160702
     -}
--   ghcjsDeps resolver
--   getDepsAt path resolver
--   prepareLTS
--   return ()

syncLts = sync ltsCfg
syncNightly = sync nightlyCfg
syncLtsMem = sync ltsCfg{nameSuffix="-mem", overwriteFiles = [ ("ghcjs-base.cabal","ghcjs-boot/ghcjs/ghcjs-base/")
                                                             , ("boot.yaml", ghcjsVanila <> "/lib/etc/")
                                                             , ("thrunner.js", ghcjsVanila <> "/lib/etc/")
                                                             ]}

main = do
  putStr "works"
--   print =<< syncWork
--   print (g,s,l)
--   mapM_ (uncurry getCabalPackage) =<< listDependencies s
--   print =<< getDescr "upstream-git/ghcjs/ghcjs"

--   syncLts
--   sync (ltsCfg {checkResolver = lts1 "6.8"})
--   sync (ltsCfg {checkResolver = lts1 "6.9"})
--   sync (ltsCfg {checkResolver = lts1 "6.10"})
--   sync (ltsCfg {checkResolver = lts1 "6.11"})
--   sync (ltsCfg {checkResolver = lts1 "6.12"})
  syncLts
  syncLtsMem
  syncNightly

{-

use cabal lib
use git lib
use shake

-}


{-

download master
download dev-shiv
download dev-boot
rebase boot against stack

-}


