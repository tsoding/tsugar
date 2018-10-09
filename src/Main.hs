{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import qualified Data.Text as T
import           Data.Time.Calendar
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

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

-- TODO(#2): getUser is not implemented
getUserEndpoint :: T.Text -> Handler User
getUserEndpoint name = return $ User { userName = name
                                     , userPoints = 100
                                     }

-- TODO(#3): chargeUser is not implemented
chargeUserEndpoint :: Charge -> Handler User
chargeUserEndpoint charge =
    return $ User { userName = chargeUser charge
                  , userPoints = 100 - chargeAmount charge
                  }

server :: Server TsugarAPI
server = getUserEndpoint :<|> chargeUserEndpoint

main :: IO ()
main = run 3000 $ serve tsugarAPI server
