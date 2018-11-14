{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS

data Config = Config { configPgUrl :: T.Text
                     , configHttpPort :: Int
                     } deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Object obj) =
        Config <$> obj .: "pgUrl"
               <*> obj .: "httpPort"
    parseJSON invalid = typeMismatch "Config" invalid

configFromFile :: FilePath -> IO Config
configFromFile filePath =
  BS.readFile filePath >>= either error return . eitherDecode
