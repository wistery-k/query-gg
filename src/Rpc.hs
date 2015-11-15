{-# LANGUAGE OverloadedStrings #-}
module Rpc (
    app
) where

import Control.Monad.Trans
import Riot

import Web.Scotty hiding (body)

import DB

import Data.Text.Lazy

import Network.HTTP.Conduit
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Database.Persist.Sqlite as P

import Data.Maybe (fromMaybe)

httpsGet :: String -> IO Text
httpsGet url = do
    res <- simpleHttp url
    return $ decodeUtf8 res

getUrl :: P.ConnectionPool -> String -> IO Text
getUrl pool url =
    flip P.runSqlPool pool $ do
        maybeContent <- P.getBy $ UniqueUrl url
        case maybeContent of
            Nothing -> do
                liftIO $ putStrLn $ "Not found in cache. Requesting: " ++ url
                tt <- liftIO $ httpsGet url
                id <- P.insert $ Cache url tt
                return tt
            Just (P.Entity rowId (Cache url content)) -> do
                liftIO $ putStrLn "Using cache."
                return content

app :: P.ConnectionPool -> ScottyM ()
app pool = do
    get "/rpc/summoner/by-name/:name" $ do
        name <- param "name"
        tt <- lift $ getUrl pool (getSummoner name)
        text tt
    get "/rpc/matchlist/:id" $ do
        id <- param "id"
        update <- param "update" `rescue` (const $ return False)
        tt <- lift $ if update then httpsGet (getMatchList id) else getUrl pool (getMatchList id)
        text tt
    get "/rpc/match/:id" $ do
        id <- param "id"
        tt <- lift $ getUrl pool (getMatch id)
        text tt
    get "/rpc/item/:id" $ do
        id <- param "id"
        tt <- lift $ getUrl pool (getItem id)
        text tt
    get "/rpc/champion/:id" $ do
        id <- param "id"
        tt <- lift $ getUrl pool (getChampion id)
        text tt
    get "/rpc/spell/:id" $ do
        id <- param "id"
        tt <- lift $ getUrl pool (getSpell id)
        text tt
    get "/rpc/realm" $ do
        tt <- lift $ getUrl pool getRealm
        text tt