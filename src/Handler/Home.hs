module Handler.Home where

import           Import


getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet||]
