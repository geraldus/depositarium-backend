{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.App where

import           Import.NoFoundation

import Data.Aeson as A


type GroupTitle = Text

data MenuGroup m
    = SingleItem (MenuItem m)
    | ItemGroup [ MenuItem m ] GroupTitle
    deriving Show

data MenuItem m where
    MenuItem
        :: RenderRoute m =>
        { menuItemLabel          :: Text
        , menuItemRoute          :: Route m }
        -- , menuItemAccessCallback :: Bool }
        -> MenuItem m
instance Show (MenuItem m) where
    show i = unpack $ "Menu Item " <> menuItemLabel i

menuGroupJson
    :: RenderRoute m
    => (Route m -> Text) -> MenuGroup m -> MenuGroupJ
menuGroupJson r (ItemGroup items t)
        = ItemGroupJ (map (menuItemJson r) items) t
menuGroupJson r (SingleItem i)
        = SingleItemJ (menuItemJson r i)

menuItemJson
    :: RenderRoute m
    => (Route m -> Text) -> MenuItem m -> MenuItemJ
menuItemJson render (MenuItem l r) = MenuItemJ l (render r)


data MenuGroupJ
    = SingleItemJ MenuItemJ
    | ItemGroupJ [ MenuItemJ ] GroupTitle

instance ToJSON MenuGroupJ where
    toJSON (SingleItemJ i)= A.object
        [ "type"     .= A.String "single"
        , "value"    .= toJSON i ]
    toJSON (ItemGroupJ is t)= A.object
        [ "type"     .= A.String "group"
        , "title"    .= A.String t
        , "items"    .= toJSON is ]

data MenuItemJ = MenuItemJ
        { menuItemJLabel          :: Text
        , menuItemJRoute          :: Text
        }
instance ToJSON MenuItemJ where
    toJSON (MenuItemJ l r) = A.object
        [ "label" .= A.String l
        , "url"   .= A.String r ]