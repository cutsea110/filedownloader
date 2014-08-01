{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (parseOnly, many', string, try, Parser, manyTill, anyChar)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TL
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Network.HTTP.Base
import System.Environment (getArgs)

mkQuery :: String -> String -> Int -> String
mkQuery t q p = "http://www.google.com/search?q=" ++
                urlEncode ("filetype:" ++ t ++ " " ++ q) ++
                "&ie=UTF-8&oe=UTF-8&num=" ++ show num ++
                "&start=" ++ show (start p)
  where (num, start) = (100, (*num))

fromLazy :: TL.Text -> Text
fromLazy = T.pack . TL.unpack

between :: Text -> Text -> Parser String
between s e = manyTill anyChar (try $ string s) >> manyTill anyChar (try $ string e)
urls :: Parser [String]
urls = many' url
url :: Parser String
url = between "<a href=\"/url?q=" "&amp;"

main :: IO ()
main = do
  (t:q:ps) <- getArgs
  rsp <- simpleHTTP $ getRequest $ mkQuery t q (if length ps == 0 then 0 else read (head ps) - 1)
  str <- getResponseBody rsp
  let (utf8str, utf8text) = (BL.pack str, fromLazy $ decodeUtf8 utf8str)
  either putStr (mapM_ putStrLn) $ parseOnly urls utf8text
