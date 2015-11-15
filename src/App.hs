{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module App (app) where

import Control.Applicative     ((<$>))
import Control.Monad.Trans
import Data.Data
import Data.Text ()
import Text.Hastache
import Text.Hastache.Context

import Web.Scotty hiding (body)

data SummonerPage = SummonerPage {
    name :: String
  , filter :: String
  , update :: Bool
} deriving (Data, Typeable)

mustache :: (Data a, Typeable a) => FilePath -> a -> ActionM ()
mustache path context =
  html =<< hastacheFile defaultConfig path (mkGenericContext context)

app :: ScottyM ()
app = do
    get "/" $ text "This is Root."

    get "/summoner/:name" $ do
        name <- param "name"
        filter <- param "filter" `rescue` (const $ return "all")
        update <- param "update" `rescue` (const $ return False)
        mustache "views/summoner.mustache" $ SummonerPage name filter update