{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text as T
import           Data.Time.Calendar
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Migrations
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Environment
import           System.Exit
import           System.IO
import           Config
import           Text.Printf

data User = User { userName :: T.Text
                 , userPoints :: Int
                 } deriving (Eq, Show, Generic)

data Charge = Charge { chargeUser :: T.Text
                     , chargeAmount :: Int
                     } deriving (Eq, Show, Generic)

instance ToJSON User where
    toJSON user =
        object [ "name" .= userName user
               , "points" .= userPoints user
               ]

instance FromJSON Charge where
    parseJSON (Object v) = Charge
        <$> v .: "user"
        <*> v .: "amount"
    parseJSON invalid = typeMismatch "Charge" invalid

type TsugarAPI = "user" :> Capture "name" T.Text :> Get '[JSON] User
            -- TODO(#1): charge endpoint requires an authentication for the clients
            :<|> "charge" :> ReqBody '[JSON] Charge :> Post '[JSON] User

tsugarAPI :: Proxy TsugarAPI
tsugarAPI = Proxy

-- TODO(#9): getUserByName is not implemented
getUserByName :: Connection -> T.Text -> IO (Maybe User)
getUserByName _ _ = return Nothing

getUserEndpoint :: Connection -> T.Text -> Handler User
getUserEndpoint conn name =
    (liftIO $ getUserByName conn name) >>= maybe (Handler $ throwE err404) return

-- TODO(#3): chargeUser is not implemented
chargeUserEndpoint :: Charge -> Handler User
chargeUserEndpoint charge =
    return $ User { userName = chargeUser charge
                  , userPoints = 100 - chargeAmount charge
                  }

server :: Connection -> Server TsugarAPI
server conn = getUserEndpoint conn :<|> chargeUserEndpoint

-- TODO(#11): PostgreSQL connection is not automatically closed
mainWithArgs :: [String] -> IO ()
mainWithArgs (configPath:_) = do
  Config {configPgUrl = pgUrl, configHttpPort = port} <-
    configFromFile configPath
  conn <- connectPostgreSQL $ BS.pack pgUrl
  migrateDatabase conn []
  printf "Serving http://localhost:%d/\n" port
  run port $ serve tsugarAPI (server conn)
mainWithArgs _ = do
  hPutStrLn stderr "Usage: tsugar <config-file-path>"
  exitWith $ ExitFailure 1

main :: IO ()
main = getArgs >>= mainWithArgs
