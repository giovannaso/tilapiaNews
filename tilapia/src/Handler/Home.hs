{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        toWidgetHead [lucius|
            ul {
                display: inline;
                list-style: none;
            }
        |]

        [whamlet|          
            <ul>
                <li>
                    <a href=@{LoginR}>
                        ADMIN
                
                <li>
                    <a href=@{HomeNewsR}>
                        NOTICIAS
        |]

getHomeNewsR :: Handler Html
getHomeNewsR = do
    defaultLayout $ do
        addStylesheetRemote "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"
        $(whamletFile "templates/home.hamlet")

