{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , LambdaCase
           , BangPatterns
           , ViewPatterns
           #-}
module PrepGhcJS.Network.Utils where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.Conduit.List      as CL
import           Network.HTTP.Simple as Si
import           Data.Time.Format
import           Data.Time.Clock
import           Data.Monoid
import qualified Data.Text as T
import           Network.HTTP.Client
import           System.Directory
import           System.IO              (stdout, hClose, openFile, IOMode(..))
import Turtle
import Prelude hiding (FilePath)

import PrepGhcJS.Types

work80 :: FilePath -> FilePath -> IO (String, Bool)
work80 (T.unpack . fromPath->cacheDir) (T.unpack . fromPath->f1) = do
    let f = cacheDir <> "/" <> f1
    createDirectoryIfMissing True cacheDir
    let etagFile = f <> ".etag"
    !oldEtag <- doesFileExist etagFile >>= \case
       True -> S.readFile etagFile
       False -> return ""
    initReq <- parseRequest $ "http://ghcjs.luite.com/"<> f1
--     let req = "HEAD" `setRequestMethod` initReq
    let req = "GET" `setRequestMethod` initReq

    (ret, mat, etag) <- httpSink req $ \response -> do
      let [tim] :: [UTCTime] = map (parseTimeOrError True defaultTimeLocale "%a, %d %b %Y %T %Z" . S8.unpack) (getResponseHeader "Last-Modified" response)
          [etag] = getResponseHeader "ETag" response
          ret = formatTime defaultTimeLocale "%F" tim
          mat = etag == oldEtag
      liftIO $ putStrLn
           $ "The status code was: "
          ++ show (getResponseStatusCode response)
          ++ show (getResponseHeader "Content-Type" response)
          ++ show (getResponseHeader "Content-Length" response)
          ++ show tim
          ++ show mat
      if mat
        then do
            liftIO $ print "cached"
            return (ret, False, etag)
        else do
            h <- liftIO $ openFile ( f <> "~") WriteMode
            liftIO $ print $ "kope" ++ (show etag) ++ (show oldEtag)
            CL.mapM_ (S.hPut h)
            liftIO $ hClose h
            return  (ret, True, etag)

    fin <- if mat
      then
         shell ("tar tf " <> T.pack f <> "~") empty >>= \case
              ExitFailure _ -> echo "Update failing" >> return False
              ExitSuccess -> do
--                  liftIO $ print a
                 liftIO $! S.writeFile etagFile etag
                 mv ( fromString (f <> "~")) (fromString f)
                 return True
      else
         return False


    return (ret, fin)

nightly :: IO T.Text
nightly = redirLocation "https://www.stackage.org/nightly"

lts :: IO T.Text
lts = redirLocation "https://www.stackage.org/lts"

redirLocation url = do
  initReq <- parseRequest url
  let req = Si.setRequestIgnoreStatus $ ("GET" `setRequestMethod` initReq){redirectCount=0}
  httpSink req $ \response ->
    return $ head $ T.pack . S8.unpack . S.drop 1 <$> getResponseHeader "Location" response

