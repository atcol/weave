module Weave.Network.HTTP (
  http
  )
  where
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Aeson                (encode, object, (.=))
import           Data.CaseInsensitive      (mk)
import           Data.HashMap.Strict       (HashMap, toList)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Network.HTTP.Client       (RequestBody (..), httpLbs, method,
                                            newManager, parseRequest,
                                            requestBody, requestHeaders,
                                            responseBody, responseStatus)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import           Protolude                 hiding (toList)
import           Weave.Types               hiding (method)

http ::  T.Text -> T.Text -> HashMap T.Text T.Text -> Maybe T.Text -> IO T.Text
http u m h b = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest $ T.unpack u
  let request =
        case b of
          Just b' -> initialRequest { requestHeaders = hr, method = T.encodeUtf8 m, requestBody = RequestBodyLBS $ encode b' }
          Nothing -> initialRequest { requestHeaders = hr, method = T.encodeUtf8 m }
  response <- httpLbs request manager
  let r = responseBody response

  return $ cs $ r
    where hr = map toHdr $ toList h
          toHdr (k,v) = (mk $ cs k, cs v)
