{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson
import           Data.Proxy
import qualified Data.Text as T
import           Data.Time.Calendar
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User { name :: T.Text
                 , points :: Int
                 } deriving (Eq, Show, Generic)

data Charge = Charge { userName :: T.Text
                     , amount :: Int
                     } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON Charge

type TsugarAPI = "user" :> Capture "name" T.Text :> Get '[JSON] User
            -- TODO(#1): charge endpoint requires an authentication for the clients
            :<|> "charge" :> ReqBody '[JSON] Charge :> Post '[JSON] User

tsugarAPI :: Proxy TsugarAPI
tsugarAPI = Proxy

-- TODO(#2): getUser is not implemented
getUser :: T.Text -> Handler User
getUser userName = return $ User { name = userName
                                 , points = 100
                                 }

-- TODO(#3): chargeUser is not implemented
chargeUser :: Charge -> Handler User
chargeUser charge = return $ User { name = userName charge
                                  , points = 100 - amount charge
                                  }

server :: Server TsugarAPI
server = getUser :<|> chargeUser

main :: IO ()
main = run 3000 $ serve tsugarAPI server
