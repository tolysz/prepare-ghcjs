module PrepGhcJS.Utils where

import qualified Text.Printf as T
import qualified Data.Text as T

toVer sres@('l':'t':'s':'-':p) d =
   let tv = T.printf "900%03d%03d" (read ma::Int) (read (tail mi)::Int)
   in
     (tv, "master-" ++ d ++ "-"  ++ sres ++ "-" ++ tv)
  where
      aa@(ma,mi) = break (=='.') p
toVer sres@('n':'i':'g':'h':'t':'l':'y':'-':s) d =
    let tv = '8':filter (/= '-') s
    in
     (tv, "ghc-8.0-" ++ d ++ "-"  ++ sres ++ "-" ++ tv)
