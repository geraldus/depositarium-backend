{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Manage.Currency.List where

import           Import

import           Utils.Common       ( encodeStrictText )

import           Database.Esqueleto
import qualified Database.Persist   as P



getManageListCurrencyR :: Handler TypedContent
getManageListCurrencyR = do
    rootId <- newIdent
    list <- runDB $ do
        c <- P.selectList ([] :: [ Filter Currency ]) []
        d <- P.selectList ([] :: [ Filter CurrencyDesc ]) []
        return (c, d)
    validateRoute <- getCurrentRoute
    let listJ = object
            [ "currencies" .=
                toJSON (fst list)
            , "descriptions" .=
                toJSON (snd list) ]
    selectRep $ do
        provideRep . defaultLayout $ [whamlet|currency-list
            <script>
                ((window) => { window.CURRENT_ROUTE_CONFIG = {
                    $maybe route <- validateRoute
                        path: '@{route}',
                    exact: true,
                    componentName: "manageListCurrency",
                    props: {
                        id: '#{rootId}',
                        apiUrl: '@?{(ManageListCurrencyR, [("_accept", "application/json")])}',
                        items: #{preEscapedToMarkup (encodeStrictText listJ)},
                        createApi: '@?{(ManageCreateCurrencyTemporalR, [("_accept", "application/json")])}'
                    }
                } })(window)
            |]
        provideRep $ return listJ
