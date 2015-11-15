{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DB where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text.Lazy (Text)
import Data.Time (UTCTime, getCurrentTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Cache
    url String
    content Text
    UniqueUrl url
    deriving Show
|]