{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (parseOnly, many', string, try, Parser, manyTill, anyChar)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Conduit.Binary (sinkFile)
import Data.List.Split (splitOn)
import Network.HTTP.Conduit
import qualified Data.Conduit as C
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base (urlEncode, urlDecode)
import Network.URI (unEscapeString)
import System.Environment (getArgs)

type URL = String
type FILETYPE = String
type QUERY = String
type PAGE = Int
type FILENAME = String

mkQuery :: FILETYPE -> QUERY -> PAGE -> URL
mkQuery t q p = "http://www.google.com/search?q=" ++
                urlEncode ("filetype:" ++ t ++ " " ++ q) ++
                "&ie=UTF-8&oe=UTF-8&num=" ++ show num ++
                "&start=" ++ show (start p)
  where (num, start) = (100, (*num))

fromLazy :: TL.Text -> Text
fromLazy = T.pack . TL.unpack

between :: Text -> Text -> Parser URL
between s e = manyTill anyChar (try $ string s) >> manyTill anyChar (try $ string e)
urls :: Parser [URL]
urls = many' url
url :: Parser URL
url = between "<a href=\"/url?q=" "&amp;"

search :: FILETYPE -> QUERY -> PAGE -> IO (Either String [URL])
search t q p = do
  rsp <- simpleHTTP $ getRequest $ mkQuery t q p
  str <- getResponseBody rsp
  let (utf8str, utf8text) = (BL.pack str, fromLazy $ decodeUtf8 utf8str)
  return $ parseOnly urls utf8text

getFileName :: URL -> FILENAME
getFileName = urlDecode . unEscapeString . last . splitOn "/"

download :: URL -> IO ()
download url = do
  req <- parseUrl url
  withManager $ \manager -> do
    res <- http req manager
    responseBody res C.$$+- sinkFile (getFileName url)

debug :: URL -> IO ()
debug url = do
  putStr url
  putStr "  ==>  "
  putStrLn $ getFileName url

main :: IO ()
main = do
  (t:q:ps) <- getArgs
  let p = if length ps == 0 then 0 else read (head ps) - 1
  either putStr (mapM_ debug) =<< search t q p
