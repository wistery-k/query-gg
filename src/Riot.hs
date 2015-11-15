module Riot (
    getSummoner
  , getMatchList
  , getMatch
  , getItem
  , getSpell
  , getChampion
  , getRealm
) where

apiKey = ""

getSummoner :: String -> String
getSummoner name =
    "https://na.api.pvp.net/api/lol/na/v1.4/summoner/by-name/" ++ name ++ "?api_key="

getMatchList :: String -> String
getMatchList summonerId =
    "https://na.api.pvp.net/api/lol/na/v2.2/matchlist/by-summoner/" ++ summonerId ++ "?rankedQueues=RANKED_SOLO_5x5&api_key="

getMatch :: String -> String
getMatch matchId =
    "https://na.api.pvp.net/api/lol/na/v2.2/match/" ++ matchId ++ "?api_key="

getSpell :: String -> String
getSpell spellId =
    "https://na.api.pvp.net/api/lol/static-data/na/v1.2/summoner-spell/" ++ spellId ++ "?spellData=all&api_key="

getItem :: String -> String
getItem itemId =
    "https://na.api.pvp.net/api/lol/static-data/na/v1.2/item/" ++ itemId ++ "?itemData=all&api_key="

getChampion :: String -> String
getChampion championId =
    "https://na.api.pvp.net/api/lol/static-data/na/v1.2/champion/" ++ championId ++ "?champData=all&api_key="

getRealm :: String
getRealm =
    "https://na.api.pvp.net/api/lol/static-data/na/v1.2/realm?api_key="
