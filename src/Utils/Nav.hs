module Utils.Nav where

import           Import.NoFoundation
import           Type.App


type MG m = MenuGroup m

partCollapsibleMenues
    :: RenderRoute m
    => [ MG m]
    -> ([MG m], ([MG m], [MG m], [MG m]))
partCollapsibleMenues gs = let
    (c, s) = partition byGroupType gs
    in (c, partByPos s)
  where byGroupType (SingleItem _ MTCollapsible _)  = True
        byGroupType (ItemGroup _ MTCollapsible _ _) = True
        byGroupType _                               = False
        partByPos = step ([], [], [])
            where step (l, r, c) (x@(SingleItem _ _ MPLeft):rest)
                        = step (x : l, r, c) rest
                  step (l, r, c) (x@(ItemGroup _ _ MPLeft _):rest)
                        = step (x : l, r, c) rest
                  step (l, r, c) (x@(SingleItem _ _ MPRight):rest)
                        = step (l, x:r, c) rest
                  step (l, r, c) (x@(ItemGroup _ _ MPRight _):rest)
                        = step (l, x:r, c) rest
                  step (l, r, c) (x@(SingleItem _ _ MPCenter):rest)
                        = step (l, r, x:c) rest
                  step (l, r, c) (x@(ItemGroup _ _ MPCenter _):rest)
                        = step (l, r, x:c) rest
                  step acc [] = acc

