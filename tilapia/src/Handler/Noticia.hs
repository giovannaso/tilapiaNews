{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Noticia where

import Import
--import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

-- renderDivs
formNoticia :: Form Noticia 
formNoticia = renderBootstrap $ Noticia 
    <$> areq textField "Titulo: " Nothing
    <*> areq textField "Autor: " Nothing
    <*> areq textField "Categoria: " Nothing
    <*> areq textField "Noticia: " Nothing


getRegisterNewsR :: Handler Html
getRegisterNewsR = do 
    (widget,enctype) <- generateFormPost formNoticia 
    defaultLayout $ do 
        addStylesheet (StaticR css_bootstrap_css)
        [whamlet|
        <header class="header-area">

            <div class="top-header-area">
                <div class="container">
                    <div class="row">
                        <div class="col-12">
                            <div class="top-header-content d-flex align-items-center justify-content-between">
                                <div class="logo" id="logo">
                                    <a><img src="static/core-img/logo.png">

    
        <div class="newspaper-main-menu" id="stickyMenu">
            <div class="classy-nav-container breakpoint-off">
                <div class="container">
    
                    <nav class="classy-navbar justify-content-between" id="newspaperNav">
                        <div class="logo">
                            <a><img src="static/core-img/logo.png">
                        <div class="classy-navbar-toggler">
                            <span class="navbarToggler"><span><span><span>
                        <div class="classy-menu">
                            <div class="classycloseIcon">
                                <div class="cross-wrap"><span class="top"><span class="bottom">
    
                            <div class="classynav">
                                <ul>
                                    <li class="active"><a href=@{HomeNewsR}>Home
                                    <li><a>Política
                                    <li><a href=@{ListaNewsR}>Notícias Recentes
                                    <li><a>Memes
                                    <li><a>Polêmicas
                                    <li><a>Contato
    
        <div class="hero-area">
            <div class="container">
                <div class="row align-items-center">
                    <div class="col-12 col-lg-8">
        
                        <div class="breaking-news-area d-flex align-items-center">
                            <div class="news-title">
                                <p>Notícias Recentes
        
                            <div id="breakingNewsTicker">
                                    <a>  Michael Jackson Revive dos mortos no feriado de finados
        
        
                        <div class="breaking-news-area d-flex align-items-center mt-15">
                            <div class="news-title title2">
                                <p>Polêmicas
        
                            <div id="internationalTicker">
                                <ul>
                                    <li><a>Deixe de ser gado! Apenas R$20 ao mês
        
                    <div class="col-12 col-lg-4">
                        <div class="hero-add">
                            <a><img src="static/hero-add.gif">
    
        <div class="contact-area section-padding-0-80">
            <div class="container">
                <div class="row">
                    <div class="col-12">
                        <div class="contact-title">
                            <h2>Cadastro de noticias
        
                <div class="row">
                    <div class="col-12 col-lg-8">
                        <div class="contact-form-area">
                            <form action=@{RegisterNewsR} method="post">
                                <div class="row">
                                    <input class="btn newspaper-btn mt-30 w-100" type="submit" value="Enviar">Enviar as noticias
    
        <footer class="footer-area">
        
            <div class="main-footer-area">
                <div class="container">
                    <div class="row">
        
        
                        <div class="col-12 col-sm-6 col-lg-4">
                            <div class="footer-widget-area mt-80">
        
                                <div class="footer-logo">
                                    <a><img src="static/core-img/logo.png">
        
                                <ul class="list">
                                    <li><a>contato@tilapianews.com
                                    <li><a>www.tilapia.tk
        
                        <div class="col-12 col-sm-4 col-lg-2">
        
                                <h4 class="widget-title">Menu
        
                                <ul class="list">
                                    <li><a href=@{HomeNewsR}>Home
                                    <li><a >Política
                                    <li><a href=@{ListaNewsR}>Notícias Recentes
                                    <li><a >Memes
                                    <li><a >Polêmicas
                                    <li><a >Contato
        
            <div class="bottom-footer-area">
                <div class="container h-100">
                    <div class="row h-100 align-items-center">
                        <div class="col-12">
        |]

postRegisterNewsR :: Handler Html
postRegisterNewsR = do 
    ((result,_),_) <- runFormPost formNoticia
    case result of 
        FormSuccess noticias -> do 
            runDB $ insert noticia
            setMessage [shamlet|
                <h2>
                    NOTICIA CADASTRADA COM SUCESSO
            |]
            redirect RegisterNewsR
        _ -> redirect ListNewsR

getListNewsR :: Handler Html 
getListProdR = do 
    -- select * from Noticias order by noticia.categoria
    noticias <- runDB $ selectList [] [Asc nmCategoria]
    defaultLayout $ do 
        $(whamletFile "templates/listNews.hamlet")

getNoticiaR :: NoticiaID -> Handler Html
getNoticiaR noticiaid = do

    let sql = "SELECT DISTINCT ON (noticias.idNoticia) ?? FROM noticias \
            \ WHERE noticias.idNoticia = ?"

    noticias <- runDB $ get404 noticiaid
    noticias <- runDB $ rawSql sql [toPersistValue noticiaid] :: Handler [(Entity Noticia)]
    
    defaultLayout $ do 
        [whamlet|
            <h2>
                #{noticiasnmNoticia noticia}

            <h3>
                #{noticiasnmCategoria noticia}

            <h4>
                #{noticiasnmAutor noticia}

            <p>
                #{noticiasnmTexto noticia}

            <a href=@{ListNewsR}>
                Voltar
        |]