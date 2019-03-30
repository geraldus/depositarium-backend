{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.Common where

import           Import.NoFoundation

import           Utils.Time

import           Data.Aeson          ( encode )
import qualified Data.HashMap.Lazy   as HML
import           Data.Time.Format    ( TimeLocale (..) )
import           Text.Julius         ( RawJS (..), RawJavascript )


{- YESOD.  APP -}

setCompositeTitle
    :: (MonadWidget m, WidgetFor site ~ m, RenderMessage site msg)
    => [ msg ] -> m ()
setCompositeTitle ms = do
    r <- getMessageRender
    setTitle . toHtml . intercalate " | " . map r $ ms

-- ** Misc

txt :: Show a => a -> Text
txt = pack . show


-- *** JSON

-- encodeMap :: (Functor f, ToJSON a) => f a -> [a] -> a
rawEncodeMap
    :: (ToJSON (f b), Functor f)
    => (a -> b) -> f a -> RawJavascript
rawEncodeMap f = rawJS . encodeStrictText . map f

encodeStrictText :: ToJSON a => a -> Text
encodeStrictText = decodeUtf8 . toStrict . encode

jsonMerge :: [Value] -> Value
jsonMerge = Object . HML.unions . map (\(Object x) -> x)


errorResponseJ :: Text -> Value
errorResponseJ errorText = object
    [ "type"    .= String "error"
    , "status"  .= String "failure"
    , "message" .= toJSON errorText ]



{- TIME -}

dateTimeRowWM :: (WidgetFor s ~ m, MonadWidget m) => UTCTime -> m ()
dateTimeRowWM t = do
    fd <- getFormatDateRender
    ft <- getFormatTimeRender
    [whamlet|
        <span .text-uppercase>#{fd t}#
        &nbsp;&nbsp;#
        <span .text-muted>#{ft t}|]

getFormatDateRender :: MonadWidget m => m (UTCTime -> Html)
getFormatDateRender = (\(l, t) -> localeFormatDate l . offsetTime t)
    <$> getFormatParams

getFormatTimeRender :: MonadWidget m => m (UTCTime -> Html)
getFormatTimeRender = (\(l, t) -> localeFormatTime l . offsetTime t)
    <$> getFormatParams

getFormatParams :: MonadWidget m => m (TimeLocale, Int)
getFormatParams = (,)
    <$> liftHandler selectLocale
    <*> liftHandler timezoneOffsetFromCookie

selectLocale :: MonadHandler m => m TimeLocale
selectLocale = locale <$> languages
    where
    locale ("ru":_) = ruTimeLocale
    locale (_:rest) = locale rest
    locale []       = defaultTimeLocale

