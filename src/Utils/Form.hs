module Utils.Form where

import           Utils.Common

import           Data.Aeson         ( Value )
import           Data.Text          ( Text )
import           Prelude            ( pure, ($) )
import           Yesod.Core.Handler ( HandlerFor )
import           Yesod.Form.Types   ( FormResult (..) )


-- | Helper form processor to handle missing data and form errors cases.
-- Takes a pair of Text, the former is for FormMissing case and the
-- latter is for form errors case
-- Returns Aeson Value
processFormJ
    :: (Text, Text)
    -> FormResult a
    -> (a -> HandlerFor s Value)
    -> HandlerFor s Value
processFormJ (msg, _) FormMissing _ = pure $ errorResponseJ msg
processFormJ (_, msg) (FormFailure errors) _ = pure $
        formErrorsResponseJ msg errors
processFormJ _ (FormSuccess a) handleSuccess = handleSuccess a
