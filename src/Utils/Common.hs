{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Utils.Common where

import           Import.NoFoundation


import           Data.Aeson          ( encode )
import qualified Data.HashMap.Lazy   as HML
import           Text.Julius         ( RawJS (..), RawJavascript )


-- * Yesod

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

formErrorsResponseJ
    :: (Functor f, ToJSON a, ToJSON (f Value))
    => Text -> f a -> Value
formErrorsResponseJ msg errors =
    errorResponseWithDataJ msg [ "form-errors" .= map toJSON errors ]

errorResponseWithDataJ
    :: Text -> [(Text, Value)] -> Value
errorResponseWithDataJ errorText =
    jsonMerge . (:) (errorResponseJ errorText) . (:[]) . object
