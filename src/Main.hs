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


snapshotCache :: FilePath
snapshotCache = "pack"
-- workDir = "work"

ltsCfg = PrepConfig
 { master  = "master.tar.gz"
 , workdir = "work-lts"
 , checkResolver = lts
 , tag = "lts"
 , copyIgnore = ["cabal", "Win32"]
 , forceVersion = [("integer-gmp", "0.5.1.0")]
 , forceFresh = [("mtl", "2.2.1")]
 , ghc = "7.10.3"
 }

nightlyCfg = PrepConfig
 { master  = "ghc-8.0.tar.gz"
 , workdir = "work-nightly"
 , checkResolver = nightly
 , tag     = "nightly"
 , copyIgnore = ["cabal", "ghc", "ghc-boot", "ghc-boot-th", "ghci", "integer-gmp", "Win32"]
 , forceVersion = [("integer-gmp", "1.0.0.1")]
 , forceFresh = [("mtl", "2.2.1"), ("transformers-compat","0.5.1.4")]
 , ghc = "8.0.1"
 }

sync :: PrepConfig -> IO ()
sync PrepConfig{..} = do
  (d,upd) <- work80 snapshotCache master
  resolver <- checkResolver
  let sres = T.unpack resolver
  let b@(extra, longFilename) = toVer sres d
  print b
  when (upd || True) $ do
    upackTar workdir snapshotCache master
    let path = workdir </> "ghcjs-0.2.0"
    fixResolver path sres
    cp (path</> "stack.yaml") (workdir </> "stack.yaml")
    deps <- ghcjsDeps path resolver
    updateVersion path extra
    mapM_ (uncurry getCabalPackage) deps
    print deps
    mapM_ (uncurry getCabalPackage) =<< listDependencies resolver
    keepPath $ do
      cd workdir
      shell "rm -rf ghcjs-boot" empty
      cp "ghcjs-0.2.0/lib/cache/boot.tar" "boot.tar"
--       echo "tar"
     {-
     cp ghcjs-0.2.0/lib/cache/boot.tar .
     tar -xf boot.tar
     cp patches/* ghcjs-boot/patches
     -}

      shell "tar -xf boot.tar" empty
      shell "rm -f boot.tar" empty
      pa <- keepPath $ do
          cd "ghcjs-boot/boot"
          (_,b) <- shellStrict "ls -d */" empty
          mapM (getBootDescr . T.unpack) $ filter (`notElem` copyIgnore ) $ map T.init $ T.lines b
--       print $ map showPkg p
      let p = map showPkg pa
      writeFile "boot-ng.cabal" $ fakePackage ++ DL.intercalate "," (map (T.unpack . fst) p)
      echo "\n\n\nMain:"
      bootDeps <- listDependencies resolver
      let canCopy = p `DL.intersect` bootDeps

      putStrLn $ "have " ++ show (DL.sort (p DL.\\ canCopy))

      let need = DL.sort (bootDeps DL.\\ (("boot-ng","0.1.0.0"):canCopy++forceVersion))
      putStrLn $ "needa " ++ show need
      keepPath $ do
          cd "ghcjs-boot/boot"
          forM (need ++ forceFresh) getPackage
      shell "pwd" empty
      shell "mkdir -p ghcjs-0.2.0/src-bin/haddock" empty
      keepPath $ do
         cd "ghcjs-0.2.0/src-bin/haddock"
         shell' "wget https://raw.githubusercontent.com/ghcjs/ghcjs/ghc-8.0/src-bin/haddock/ResponseFile.hs"
      shell' $ "cp -f ../spec-"<> ghc <> "/ghcjs-base.cabal ghcjs-boot/ghcjs/ghcjs-base/"
      shell' "tar -cf boot.tar ghcjs-boot"
      shell' "cp -f boot.tar ghcjs-0.2.0/lib/cache/"
      shell' $ "cp -f ../spec-"<> ghc <> "/boot.yaml ghcjs-0.2.0/lib/etc/"

      let newName = "ghcjs-0.2.0." <> T.pack extra
      shell' ("mv ghcjs-0.2.0 " <> newName)
      shell' ("tar -zcf archive.tar.gz " <> newName)

--       shell' ("scp archive.tar.gz ghcjs-host:/var/www/ghcjs/" <> (T.pack longFilename) <> ".tar.gz")
      shell' ("scp archive.tar.gz ghcjs-host:/var/www/ghcjs/untested/" <> T.pack longFilename <> ".tar.gz")
      shell' ("cp archive.tar.gz ../archive/" <> T.pack longFilename <> ".tar.gz")
      shell' ("cp archive.tar.gz " <> newName <> "_ghc-"<> ghc <>".tar.gz")
--       shell' ("cp archive.tar.gz " <> newName <> "_ghc-8.0.1.tar.gz")
--       forM canCopy print
    return ()

getPackage p@(name, vers) = do
  print p
  let ver = name <> "-" <> vers
  shell' "pwd"
  shell' ("rm -rf " <> name )
  shell' ("tar -zxf ../../../cabalCache/" <> ver <> ".tar.gz")
  shell' ("mv " <> ver <> " " <> name )
  shell' ("wget https://hackage.haskell.org/package/"<> ver <>"/"<> name <>".cabal -O " <> name <> "/" <> name <>".cabal")
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

-- syncLts = sync ltsCfg
syncNightly = sync nightlyCfg

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


