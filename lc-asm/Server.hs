{-# LANGUAGE OverloadedStrings #-}
module Server
  ( run
  )
where

import qualified Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.HTTP.Types.Status      ( status400
                                                , status200
                                                )
import           Data.ByteString.Lazy.Char8     ( unpack )
import           Data.Aeson                     ( encode
                                                , ToJSON
                                                )
import qualified Data.Map.Strict               as Map

run
  :: (ToJSON err, ToJSON response)
  => String
  -> (String -> Either err response)
  -> IO ()
run portStr handle = do
  putStrLn $ "Launching HTTP server on port " <> portStr
  Warp.run (read portStr) app
 where
  app req respond = do
    input <- unpack <$> Wai.strictRequestBody req
    case handle input of
      Left err -> respond $ Wai.responseLBS
        status400
        corsHeader
        (encode (Map.singleton "error" err))
      Right response ->
        respond $ Wai.responseLBS status200 corsHeader (encode response)
  corsHeader = [("Access-Control-Allow-Origin", "*")]
