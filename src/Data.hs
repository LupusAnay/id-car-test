module Data where

import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Project json
    name String
    description String
    url String
    deriving Show
|]