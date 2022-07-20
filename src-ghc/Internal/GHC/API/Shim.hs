{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
module Internal.GHC.API.Shim (
  UnhelpfulSpanReason(..)
  , pattern RealSrcSpan
  , pattern UnhelpfulSpan
  , GenWithIsBoot(..)
  , getDependenciesModuleNames
  , dataConExTyVars 
  ) where

import Internal.GHC.API.Common (FastString, fsLit, unpackFS, Dependencies, ModuleName, DataCon, TyVar)
import qualified SrcLoc
import GhcMake (IsBoot (..))
import GhcPlugins (Dependencies(dep_mods), dataConExTyCoVars)

data BufSpan

pattern RealSrcSpan :: SrcLoc.RealSrcSpan -> Maybe BufSpan -> SrcLoc.SrcSpan
pattern RealSrcSpan rss mbSpan <- ((,Nothing) -> (SrcLoc.RealSrcSpan rss, mbSpan))
  where
    RealSrcSpan rss _mbSpan = SrcLoc.RealSrcSpan rss

data UnhelpfulSpanReason
  = UnhelpfulNoLocationInfo
  | UnhelpfulWiredIn
  | UnhelpfulInteractive
  | UnhelpfulGenerated
  | UnhelpfulOther !FastString
  deriving (Eq, Show)

pattern UnhelpfulSpan :: UnhelpfulSpanReason -> SrcLoc.SrcSpan
pattern UnhelpfulSpan reason <- (toUnhelpfulReason -> Just reason)
  where
    UnhelpfulSpan reason = SrcLoc.UnhelpfulSpan (fromUnhelpfulReason reason)

fromUnhelpfulReason :: UnhelpfulSpanReason -> FastString
fromUnhelpfulReason = \case
  UnhelpfulNoLocationInfo -> fsLit "UnhelpfulNoLocationInfo"
  UnhelpfulWiredIn        -> fsLit "UnhelpfulWiredIn"
  UnhelpfulInteractive    -> fsLit "UnhelpfulInteractive"
  UnhelpfulGenerated      -> fsLit "UnhelpfulGenerated"
  UnhelpfulOther fs       -> fs

toUnhelpfulReason :: SrcLoc.SrcSpan -> Maybe UnhelpfulSpanReason
toUnhelpfulReason (SrcLoc.RealSrcSpan _) = Nothing
toUnhelpfulReason (SrcLoc.UnhelpfulSpan fs) = Just $ case unpackFS fs of
  "UnhelpfulNoLocationInfo" -> UnhelpfulNoLocationInfo
  "UnhelpfulWiredIn"        -> UnhelpfulWiredIn
  "UnhelpfulInteractive"    -> UnhelpfulInteractive
  "UnhelpfulGenerated"      -> UnhelpfulGenerated
  _                         -> UnhelpfulOther fs

--
-- Dependencies and Boot
--
type IsBootInterface = IsBoot

-- | This data type just pairs a value 'mod' with an IsBootInterface flag. In
-- practice, 'mod' is usually a @Module@ or @ModuleName@'.
data GenWithIsBoot mod = GWIB
  { gwib_mod :: mod
  , gwib_isBoot :: IsBootInterface
  } deriving ( Eq, Ord, Show
             , Functor, Foldable, Traversable
             )

type ModuleNameWithIsBoot = GenWithIsBoot ModuleName

getDependenciesModuleNames :: Dependencies -> [ModuleNameWithIsBoot]
getDependenciesModuleNames = map f . dep_mods
  where
    f :: (ModuleName, Bool) -> ModuleNameWithIsBoot
    f (modName, b) = let isBoot = if b then IsBoot else NotBoot in GWIB modName isBoot

dataConExTyVars :: DataCon -> [TyVar]
dataConExTyVars = dataConExTyCoVars