{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Blog

import Data.HashMap.Strict as M
import Data.Aeson.Types as AT
import qualified Web.Heroku

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
    logWare   = if development then logStdoutDev
                               else logStdout

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    hconfig <- loadHerokuConfig
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              (Database.Persist.Store.loadConfig . combineMappings hconfig) >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    return $ App conf s p manager dbconf

canonicalizeKey :: (Text, val) -> (Text, val)
canonicalizeKey ("dbname", val) = ("database", val)
canonicalizeKey pair = pair

toMapping :: [(Text, Text)] -> AT.Value
toMapping xs = AT.Object $ M.fromList $ Import.map (\(key, val) -> (key, AT.String val)) xs

combineMappings :: AT.Value -> AT.Value -> AT.Value
combineMappings (AT.Object m1) (AT.Object m2) = AT.Object $ m1 `M.union` m2
combineMappings _ _ = error "Data.Object is not a Mapping."

loadHerokuConfig :: IO AT.Value
loadHerokuConfig = do
  Web.Heroku.dbConnParams >>= return . toMapping . Import.map canonicalizeKey

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
