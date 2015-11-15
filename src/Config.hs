module Config
    ( get
    ) where

import           Control.Applicative
import qualified Data.HashMap.Strict as M
import           Data.Maybe          (fromMaybe)
import           Data.Text
import qualified Data.Yaml as Y

{-
  most code is taken from Yesod's configuration handling.
  see original in:
    https://github.com/yesodweb/yesod/blob/master/yesod-default/Yesod/Default/Config.hs
-}


get :: FilePath -> Text -> Text -> IO Text
get path env configName = do
    file <- Y.decodeFile path
    allConfigs <- maybe (fail "Invalid YAML file") return file
    configs <- getObject env allConfigs

    lookupScalar configName configs

    where
        lookupScalar k m =
            case M.lookup k m of
                Just (Y.String t) -> return t
                Just _          -> fail $ "Invalid value for: " ++ show k
                Nothing         -> fail $ "Not found: " ++ show k

        getObject env v = do
          envs <- fromObject v
          maybe
              (error $ "Could not find environment: " ++ show env)
              return $ fromObject =<< M.lookup env envs

        fromObject m =
          case m of
              Y.Object o -> return o
              _        -> fail "Invalid JSON format"