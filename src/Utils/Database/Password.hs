module Utils.Database.Password where

import           ClassyPrelude.Yesod

import           Data.Text.Encoding            ( decodeUtf8With )
import           Data.Text.Encoding.Error      ( lenientDecode )
import qualified Yesod.Auth.Util.PasswordStore as PS


-- | Salt a password with a randomly generated salt.
saltPass :: Text -> IO Text
saltPass = fmap (decodeUtf8With lenientDecode)
         . flip PS.makePassword 16
         . encodeUtf8
