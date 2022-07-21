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
  , Mult
  , pattern Many
  , pattern FunTy
  , mkFunTy
  , ft_af, ft_mult, ft_arg, ft_res
  ) where

import Internal.GHC.API.Common (FastString, fsLit, unpackFS, Dependencies, ModuleName, DataCon, TyVar, Type, AnonArgFlag (..), mkTyConTy, TyCon, promoteDataCon, Name, Unique, mkPreludeTyConUnique, mkWiredInTyConName, BuiltInSyntax(..), mkPreludeDataConUnique, tyConAppTyCon_maybe, gHC_TYPES, Module, mkWiredInName, ConLike (..), mkDataOccFS, CType, mkAlgTyCon, mkAnonTyConBinders, mkDataTyConRhs, mkPrelTyConRepName, liftedTypeKind, AlgTyConFlav (..), Role (..), hasKey, mkDataCon, mkTyConApp, mkTyVarTys, mkTyConTagMap, mkDataConWorkId, dataConWorkerUnique, TyThing (..), nameModule, nameOccName, nameUnique, dataConName, dataConWorkId, DataConRep (..), RuntimeRepInfo (..), lookupNameEnv_NF, HsSrcBang (..), SrcUnpackedness (..), SrcStrictness (..), SourceText (..), mkDataConWorkerOcc)
import qualified SrcLoc
import GhcMake (IsBoot (..))
import GhcPlugins (Dependencies(dep_mods), dataConExTyCoVars)
import qualified TyCoRep as Ty

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

type Mult = Type

pcDataCon :: Name -> [TyVar] -> [Type] -> TyCon -> DataCon
pcDataCon n univs tys tycon = data_con
  where
    data_con = mkDataCon n
                         False
                         (mkPrelTyConRepName n)
                         (map (const (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)) tys)
                         []
                         univs
                         []
                         (error "[TyVarBinder]")
                         []
                         []
                         tys
                         (mkTyConApp tycon (mkTyVarTys univs))
                         NoRRI
                         tycon
                         (lookupNameEnv_NF (mkTyConTagMap tycon) n)
                         []
                         (mkDataConWorkId (mkDataConWorkerName data_con (dataConWorkerUnique (nameUnique n))) data_con)
                         NoDataConRep

mkDataConWorkerName :: DataCon -> Unique -> Name
mkDataConWorkerName data_con wrk_key =
    mkWiredInName modu wrk_occ wrk_key
                  (AnId (dataConWorkId data_con)) UserSyntax
  where
    modu    = nameModule dc_name
    dc_name = dataConName data_con
    dc_occ  = nameOccName dc_name
    wrk_occ = mkDataConWorkerOcc dc_occ

pcTyCon :: Name -> Maybe CType -> [TyVar] -> [DataCon] -> TyCon
pcTyCon name cType tyvars cons
  = mkAlgTyCon name
                (mkAnonTyConBinders VisArg tyvars)
                liftedTypeKind
                (map (const Representational) tyvars)
                cType
                []              -- No stupid theta
                (mkDataTyConRhs cons)
                (VanillaAlgTyCon (mkPrelTyConRepName name))
                False           -- Not in GADT syntax

mkWiredInDataConName :: BuiltInSyntax -> Module -> FastString -> Unique -> DataCon -> Name
mkWiredInDataConName built_in modu fs unique datacon
  = mkWiredInName modu (mkDataOccFS fs) unique
                  (AConLike (RealDataCon datacon))    -- Relevant DataCon
                  built_in

multiplicityTyConKey :: Unique
multiplicityTyConKey = mkPreludeTyConUnique 192

multiplicityTyConName :: Name
multiplicityTyConName = mkWiredInTyConName UserSyntax gHC_TYPES (fsLit "Multiplicity")
                          multiplicityTyConKey multiplicityTyCon

manyDataConName :: Name
manyDataConName = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit "Many") manyDataConKey manyDataCon

multiplicityTyCon :: TyCon
multiplicityTyCon = pcTyCon multiplicityTyConName Nothing [] [manyDataCon]

manyDataCon :: DataCon
manyDataCon = pcDataCon manyDataConName [] [] multiplicityTyCon

manyDataConKey :: Unique
manyDataConKey = mkPreludeDataConUnique 116

manyDataConTy :: Type
manyDataConTy = mkTyConTy manyDataConTyCon

manyDataConTyCon :: TyCon
manyDataConTyCon = promoteDataCon manyDataCon

pattern Many :: Mult
pattern Many <- (isManyDataConTy -> True)
  where Many = manyDataConTy

isManyDataConTy :: Mult -> Bool
isManyDataConTy ty
  | Just tc <- tyConAppTyCon_maybe ty
  = tc `hasKey` manyDataConKey
isManyDataConTy _ = False

pattern FunTy :: AnonArgFlag -> Mult -> Type -> Type -> Type
pattern FunTy { ft_af, ft_mult, ft_arg, ft_res } <- ((Many,) -> (ft_mult, Ty.FunTy ft_af ft_arg ft_res)) where
    FunTy ft_af' _ft_mult' ft_arg' ft_res' = Ty.FunTy ft_af' ft_arg' ft_res'

mkFunTy :: AnonArgFlag -> Mult -> Type -> Type -> Type
mkFunTy af _ arg res = Ty.FunTy af arg res