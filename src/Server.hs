
module Server (runServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Data.Text (Text)
import Data.Aeson (Value, object, (.=))
import Servant
import Network.Wai.Handler.Warp (run)

import Config
import Job (startJob, TemplateId)


type API
  = "document"
    :> QueryParam "tplId" TemplateId
    :> ReqBody '[JSON] Value
    :> Post '[JSON] Value
  :<|> "document" :> Raw



runServer :: Config -> IO ()
runServer cfg@(Config {httpPort, docDir})
  = run httpPort
  $ serve (Proxy :: Proxy API)
  $ createDocument cfg
    :<|> serveDirectory docDir



createDocument :: Config -> Maybe TemplateId -> Value -> Handler Value
createDocument cfg mTplId obj
  = case mTplId of
    Nothing -> throwError err404
    Just tplId -> do
      docId <- liftIO $ startJob cfg tplId obj
      return $ object
        [ "docId" .= docId
        , "docUrl" .= ("/document/" ++ docId ++ ".pdf")
        ]
