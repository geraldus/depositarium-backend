{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Type.App where

import           Import.NoFoundation

import Data.Aeson as A


type GroupTitle = Text

data MenuGroup m
    = SingleItem (MenuItem m) MenuType MenuPositioning
    | ItemGroup [ MenuItem m ] MenuType MenuPositioning GroupTitle
    deriving Show

data MenuPositioning
    = MPLeft
    | MPCenter
    | MPRight
    deriving (Show)
instance ToJSON MenuPositioning where
    toJSON MPLeft = A.String "left"
    toJSON MPRight = A.String "right"
    toJSON MPCenter = A.String "center"


data MenuType
    = MTCollapsible
    | MTSticky
    deriving (Show)
instance ToJSON MenuType where
    toJSON MTCollapsible = A.String "collapsible"
    toJSON MTSticky = A.String "sticky"

data MenuItem m where
    MenuItem
        :: RenderRoute m =>
        { menuItemLabel          :: Text
        , menuItemRoute          :: Route m }
        -- , menuItemAccessCallback :: Bool }
        -> MenuItem m
instance Show (MenuItem m) where
    show i = unpack $ "Menu Item " <> menuItemLabel i

menuGroupJson :: RenderRoute m => (Route m -> Text) -> MenuGroup m -> MenuGroupJ
menuGroupJson r (ItemGroup items mt mp t)
        = ItemGroupJ (map (menuItemJson r) items) mt mp t
menuGroupJson r (SingleItem i mt mp)
        = SingleItemJ (menuItemJson r i) mt mp

menuItemJson :: RenderRoute m => (Route m -> Text) -> MenuItem m -> MenuItemJ
menuItemJson render (MenuItem l r) = MenuItemJ l (render r)


data MenuGroupJ
    = SingleItemJ MenuItemJ MenuType MenuPositioning
    | ItemGroupJ [ MenuItemJ ] MenuType MenuPositioning GroupTitle

instance ToJSON MenuGroupJ where
    toJSON (SingleItemJ i mt mp)= A.object
        [ "type"     .= A.String "single"
        , "menuType" .= toJSON mt
        , "pos"      .= toJSON mp
        , "value"    .= toJSON i ]
    toJSON (ItemGroupJ is mt mp t)= A.object
        [ "type"     .= A.String "group"
        , "menuType" .= toJSON mt
        , "pos"      .= toJSON mp
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