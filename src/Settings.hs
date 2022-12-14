{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings where

import           ClassyPrelude.Yesod
import qualified Control.Exception          as Exception
import           Data.Aeson                 (Result (..), fromJSON, withObject,
                                             (.!=), (.:?))
import           Data.FileEmbed             (embedFile)
import           Data.Yaml                  (decodeEither')
import           Database.Persist.Sqlite    (SqliteConf)
import           Language.Haskell.TH.Syntax (Exp, Name, Q)
import           Network.Wai.Handler.Warp   (HostPreference)
import           Yesod.Default.Config2      (applyEnvValue, configSettingsYml)
import           Yesod.Default.Util         (WidgetFileSettings,
                                             widgetFileNoReload,
                                             widgetFileReload)

data AppSettings = AppSettings
    { appStaticDir              :: String
    , appDatabaseConf           :: SqliteConf
    , appRoot                   :: Maybe Text
    , appHost                   :: HostPreference
    , appPort                   :: Int
    , appIpFromHeader           :: Bool

    , appDetailedRequestLogging :: Bool
    , appShouldLogAll           :: Bool
    , appReloadTemplates        :: Bool
    , appMutableStatic          :: Bool
    , appSkipCombining          :: Bool

    , appCopyright              :: Text
    , appAnalytics              :: Maybe Text

    , appAuthDummyLogin         :: Bool
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev =
#ifdef DEVELOPMENT
                True
#else
                False
#endif
        appStaticDir              <- o .: "static-dir"
        appDatabaseConf           <- o .: "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .: "port"
        appIpFromHeader           <- o .: "ip-from-header"

        dev                       <- o .:? "development"      .!= defaultDev

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= dev
        appShouldLogAll           <- o .:? "should-log-all"   .!= dev
        appReloadTemplates        <- o .:? "reload-templates" .!= dev
        appMutableStatic          <- o .:? "mutable-static"   .!= dev
        appSkipCombining          <- o .:? "skip-combining"   .!= dev

        appCopyright              <- o .:  "copyright"
        appAnalytics              <- o .:? "analytics"

        appAuthDummyLogin         <- o .:? "auth-dummy-login"      .!= dev

        return AppSettings {..}

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

combineSettings :: CombineSettings
combineSettings = def


widgetFile :: String -> Q Exp
widgetFile = (if appReloadTemplates compileTimeAppSettings
                then widgetFileReload
                else widgetFileNoReload)
              widgetFileSettings

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id
                       $ decodeEither' configSettingsYmlBS

compileTimeAppSettings :: AppSettings
compileTimeAppSettings =
    case fromJSON $ applyEnvValue False mempty configSettingsYmlValue of
        Error e          -> error e
        Success settings -> settings


combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets'
    (appSkipCombining compileTimeAppSettings)
    combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts'
    (appSkipCombining compileTimeAppSettings)
    combineSettings
