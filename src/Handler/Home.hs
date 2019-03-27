{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where

import           Import


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    addReactScripts
    setAppPageTitle MsgHomePageTitle
    addReactBundle
    $(widgetFile "pages/home")

