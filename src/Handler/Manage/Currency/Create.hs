{-# LANGUAGE OverloadedStrings #-}
module Handler.Manage.Currency.Create where

import           Import

import           Form.Currency      ( currencyDescIForm, currencyIForm )
import           Utils.App.Common   ( processAppFormJ )
import           Utils.Common       ( successResponseWithDataJ )

import           Database.Esqueleto
import qualified Database.Persist   as P



postManageCreateCurrencyTemporalR :: Handler TypedContent
postManageCreateCurrencyTemporalR = do
    currency <- runInputPostResult currencyIForm
    currencyDesc <- runInputPostResult (currencyDescIForm (toSqlKey (-1)))
    res <- processAppFormJ ((,) <$> currency <*> currencyDesc)  $ \(c, d) ->
        runDB $ do
            currencyId <- insert c
            let description = d { currencyDescCurrency = currencyId }
            descId <- insert description
            users <- P.selectList [] []
            walletsData <- mapM
                    (\u -> (,) u <$> liftHandler appNonce128urlT)
                    users
            let wallets = map
                    (\(u, t) -> Wallet (entityKey u) currencyId 0 t)
                    walletsData
            walletIds <- insertMany wallets
            return . successResponseWithDataJ $
                ( Entity currencyId c
                , Entity descId description
                , zipWith Entity walletIds wallets )
    selectRep . provideRep . pure $ res
