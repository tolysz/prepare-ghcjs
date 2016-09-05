{-# LANGUAGE OverloadedStrings
  , LambdaCase
  #-}
module PrepGhcJS.Shell.Utils where

import qualified Data.Text as T
import Turtle
import Prelude hiding (FilePath)
import PrepGhcJS.Types

gitBase = "upstream-git"
cabalCacheBase = "../cabalCache"

gitCR = "git clone --recursive  "

justClone (repo, d, br) = do
  testdir d >>= \case
    False -> void $ shell (gitCR <> repo) empty
    True -> return ()
  cd d
  shell ("git checkout " <> br) empty
  shell "git fetch " empty
  shell "git submodule update --recursive " empty
  cd ".."

gitClone :: [(Text,FilePath,Text)] -> IO ()
gitClone repos = do
   mktree gitBase
   cd gitBase
   mapM_ justClone repos
   cd ".."

syncWork = gitClone [ ("https://github.com/ghcjs/ghcjs-boot.git", "ghcjs-boot" , "ghc-8.0")
                    , ("https://github.com/ghcjs/ghcjs.git"     , "ghcjs", "ghc-8.0")
                    , ("https://github.com/ghcjs/shims.git"     , "shims", "ghc-8.0")
                    ]
-- https://github.com/ghcjs/ghcjs-boot.git @ ghc-8.0
-- https://github.com/ghcjs/ghcjs.git @ ghc-8.0
-- https://github.com/ghcjs/shims.git @ ghc-8.0

{-
git clone --recursive -j8 git://github.com/foo/bar.git
git pull --recurse-submodules
git submodule update --recursive
-}

fixResolver :: FilePath -> String  -> IO ()
fixResolver fp res = writeFile (T.unpack (fromPath fp) ++ "/stack.yaml")
     $ "resolver: " ++ res ++ "\nallow-newer: true\n"

updateVersion :: FilePath -> String -> IO ()
updateVersion fp extra =
   void $ shell ("sed \"s/^Version:.*/Version:        0.2.0."<> T.pack extra <>"/\" -i " <> fromPath fp <> "/ghcjs.cabal" ) empty

getCabalPackage pkg pvers = do
  mktree cabalCacheBase
  let ver=pkg <> "-" <> pvers
      verz= ver <> ".tar.gz"

  let dst = cabalCacheBase </> fromText verz
  let dst1 = fromPath dst
  testfile dst >>= \case
--     True -> echo $ "cached: " <> ver
    True -> return ()
    False -> do
       echo $ "miss: " <> ver
       void $ shell ("wget https://hackage.haskell.org/package/"<> ver <>"/"<> verz <>" -O " <> dst1) empty

-- ls upstream-git/ghcjs-boot/boot

withIgnorePackage c = do
   mv "ghcjs.cabal" "ghcjs.cabal~"
   shell "grep -v bin-package-db ghcjs.cabal~ > ghcjs.cabal" empty
   ret <- c
   mv "ghcjs.cabal~" "ghcjs.cabal"
   return ret

listDependencies res = do
  (_,a) <- shellStrict ("stack list-dependencies --resolver " <> res) empty
  return $ map ((\[a,b] -> (a,b)) . T.words) $ T.lines a

keepPath m = do
  old <- pwd
--   echo $ fromPath old
  r <- m
  cd old
  return r

getDepsAt path res = keepPath $ do
  cd path
  listDependencies res

ghcjsDeps path r = keepPath $ do
   cd path
   withIgnorePackage $ listDependencies r


prepareLTS :: IO ()
prepareLTS = echo "Done"

prepareNightly :: IO ()
prepareNightly = echo "Done"


upackTar :: FilePath -> FilePath -> FilePath -> IO ()
upackTar wd ca f = keepPath $ do
 shell ("rm -rf " <> fromPath wd) empty
 mktree wd
 cd wd
 void $ shell ("tar -zxf " <> "../" <> fromPath ca <> "/" <> fromPath f) empty
--  "work-lts" snapshotCache ltsMaster
