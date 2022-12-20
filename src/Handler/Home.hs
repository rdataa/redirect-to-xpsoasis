{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import
import           Text.Julius (RawJS (..))

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Redirection"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Redirection!"
        $(widgetFile "homepage")

getDownloadR :: Handler Html
getDownloadR = do getHomeR

getForumUsersR :: Handler Html
getForumUsersR = do getHomeR

getForumGeneralTopicsR :: Handler Html
getForumGeneralTopicsR = do getHomeR

getFittingDatabaseR :: Handler Html
getFittingDatabaseR = do getHomeR

postFittingDatabaseR :: Handler Html
postFittingDatabaseR = do postHomeR

getAAnalyzerR :: Handler Html
getAAnalyzerR = do getHomeR

getAllFaqsR :: Handler Html
getAllFaqsR = do getHomeR

getXPSGeometryR :: Handler Html
getXPSGeometryR = do getHomeR

getAAlignerR :: Handler Html
getAAlignerR = do getHomeR
