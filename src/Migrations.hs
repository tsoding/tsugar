{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveAnyClass #-}
module Migrations where

import           Control.Monad
import qualified Data.ByteString as BS
import           Data.Foldable
import           Data.List
import           Data.String
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Types
import           Text.InterpolatedString.QM
import Data.Semigroup
import Data.Monoid

newtype Migration = Migration { migrationQuery :: Query } deriving Eq

instance Semigroup Migration where
    m1 <> m2 = Migration (migrationQuery m1 <> migrationQuery m2)

instance Monoid Migration where
    mempty = Migration $ mempty

instance IsString Migration where
    fromString = Migration . fromString

instance FromRow Migration where
    fromRow = fromString <$> field

applyMigration :: Connection -> Migration -> IO ()
applyMigration conn migration = do
  execute_ conn $ migrationQuery migration
  void $ execute conn
                 [qms| INSERT INTO Migrations (
                       migrationQuery
                     ) VALUES (
                       ?
                     ); |]
                 [fromQuery $ migrationQuery migration]


createMigrationTablesIfNeeded :: Connection -> IO ()
createMigrationTablesIfNeeded conn =
    void $ execute_ conn [qms| CREATE TABLE IF NOT EXISTS Migrations (
                                id SERIAL PRIMARY KEY,
                                migrationQuery TEXT NOT NULL
                              ); |]

filterUnappliedMigrations :: Connection -> [Migration] -> IO [Migration]
filterUnappliedMigrations conn migrations = do
  appliedMigrations <- query_ conn "SELECT migrationQuery FROM Migrations;"
  maybe (error "Inconsistent migrations state! \
               \List of already applied migrations \
               \is not a prefix of required migrations.")
        return
        (stripPrefix appliedMigrations migrations)

migrateDatabase :: Connection -> [Migration] -> IO ()
migrateDatabase conn migrations = do
  createMigrationTablesIfNeeded conn
  unappliedMigrations <- filterUnappliedMigrations conn migrations
  traverse_ (applyMigration conn) unappliedMigrations
