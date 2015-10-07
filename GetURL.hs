module GetURL where

import Network.HTTP.Conduit
import Data.List
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.IO
import Data.Char
import Control.Applicative
import Text.XML.Light
import System.Environment
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text

getURL :: String  -> IO String
getURL url = do
  request <- parseUrl url
  manager <- newManager tlsManagerSettings
  lbs <- responseBody <$> httpLbs request manager
  return $ Text.unpack (Text.decodeUtf8 (B.concat (L.toChunks lbs)))
