module Type.UserData where

import           Import.NoFoundation
import           Local.Persist.Access

data UserData = UserData
    { userDataFirstName  :: Text
    , userDataPatronymic :: Text
    , userDataLastName   :: Text
    , userDataIdent      :: Text
    , userDataEmail      :: Text
    , userDataPassword   :: Text
    , userDataRights     :: [AccessType] }
