{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

formLogin :: Form (Text, Text) 
formLogin = renderBootstrap $ (,) 
    <$> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Senha: " Nothing

    
getLoginR :: Handler Html
getLoginR = do 
    (widget,enctype) <- generateFormPost formLogin
    defaultLayout $ do
        [whamlet|            
            <h1>
                LOGIN
                                
            <form method=post action=@{LoginR}>
                ^{widget}
                <input type="submit" value="Entrar">
        |]


postLoginR :: Handler Html
postLoginR = do 
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess ("root@root.com","root") -> do 
            setSession "_NOME" "Root"
            redirect RegisterNewsR
        _ -> redirect HomeR
