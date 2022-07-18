{-| This module re-exports a bunch of the GHC API.

The intended use of this module is to shelter LiquidHaskell from changes to the GHC API, so this is the
/only/ module LiquidHaskell should import when trying to access any ghc-specific functionality.

--}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

module Internal.GHC.API (
    module Ghc
  , module StableModule

  , pattern Bndr
  , pattern LitString
  , pattern LitFloat
  , pattern LitDouble
  , pattern LitChar
  , VarBndr

  , pattern AnonTCB
  , ft_af, ft_arg, ft_res
  , noExtField
  , Mult
  , pattern Many

  , tyConRealArity
  , dataConExTyVars

  , pattern RealSrcSpan
  , pattern UnhelpfulSpan
  , UnhelpfulSpanReason(..)
  , scaledThing
  , Scaled(..)
  , mkScaled
  , irrelevantMult
  , dataConInstArgTys
  , dataConOrigArgTys
  , dataConRepArgTys
  , mkLocalVar
  , DataConAppContext(..)
  , deepSplitProductType_maybe
  , splitFunTys
  , mkUserLocal
  , dataConWrapperType
  , apiAnnComments
  , getDependenciesModuleNames
  , GenWithIsBoot(..)
  , ModuleNameWithIsBoot
  , IsBootInterface
  , isBootSummary
  , mkIntExprInt
  , dataConFullSig

  , pattern LitNumber
  , getDependenciesModuleNames
  ) where

import           Language.Haskell.Liquid.GHC.API.StableModule      as StableModule
import           GHC                                               as Ghc hiding ( Warning
                                                                                 , SrcSpan(RealSrcSpan, UnhelpfulSpan)
                                                                                 , exprType
                                                                                 )

import CoreFVs                  as Ghc (exprFreeVarsList)
import OccurAnal                as Ghc (occurAnalysePgm)
import Annotations              as Ghc
import ApiAnnotation            as Ghc
import Avail                    as Ghc
import Bag                      as Ghc
import BasicTypes               as Ghc
import Class                    as Ghc
import CoAxiom                  as Ghc
import Coercion                 as Ghc
import ConLike                  as Ghc
import CoreLint                 as Ghc hiding (dumpIfSet)
import CoreMonad                as Ghc (CoreToDo(..))
import CoreSubst                as Ghc (deShadowBinds, substExpr, emptySubst, extendCvSubst)
import CoreSyn                  as Ghc hiding (AnnExpr, AnnExpr' (..), AnnRec, AnnCase)
import CoreUtils                as Ghc (exprType)
import CostCentre               as Ghc
import Data.Map.Strict (Map)
import DataCon                  as Ghc hiding (dataConInstArgTys, dataConOrigArgTys, dataConRepArgTys, dataConFullSig)
import qualified DataCon        as Ghc
import Digraph                  as Ghc
import DriverPhases             as Ghc (Phase(StopLn))
import DriverPipeline           as Ghc hiding (P, getLocation)
import DsMonad                  as Ghc
import DynFlags                 as Ghc
import ErrUtils                 as Ghc
import FamInst                  as Ghc
import FamInstEnv               as Ghc hiding (pprFamInst)
import Finder                   as Ghc
import GHC                      as Ghc (SrcSpan)
import GhcMonad                 as Ghc (withSession)
import GhcPlugins               as Ghc (deserializeWithData , fromSerialized , toSerialized, extendIdSubst)
import HscMain                  as Ghc
import HscTypes                 as Ghc hiding (IsBootInterface, isBootSummary)
import Id                       as Ghc hiding (lazySetIdInfo, setIdExported, setIdNotExported, mkUserLocal)
import IdInfo                   as Ghc
import IfaceSyn                 as Ghc
import InstEnv                  as Ghc
import Literal                  as Ghc
import MkCore                   as Ghc hiding (mkIntExprInt)
import MkId                     (mkDataConWorkId)
import Module                   as Ghc
import Name                     as Ghc hiding (varName)
import NameEnv                  (lookupNameEnv_NF)
import NameSet                  as Ghc
import Outputable               as Ghc hiding ((<>))
import Pair                     as Ghc
import Panic                    as Ghc
import Plugins                  as Ghc (defaultPlugin, Plugin(..), CommandLineOption, purePlugin)
import PrelInfo                 as Ghc
import PrelNames                as Ghc hiding (wildCardName)
import RdrName                  as Ghc
import SrcLoc                   as Ghc hiding (RealSrcSpan, SrcSpan(UnhelpfulSpan))
import TcRnDriver               as Ghc
import TcRnMonad                as Ghc hiding (getGHCiMonad)
import TysPrim                  as Ghc
import TysWiredIn               as Ghc
import Unify                    as Ghc
import UniqDFM                  as Ghc
import UniqFM                   as Ghc
import UniqSet                  as Ghc
import UniqSupply               as Ghc
import Unique                   as Ghc
import Var                      as Ghc hiding (mkLocalVar)
import VarEnv                   as Ghc
import VarSet                   as  Ghc
import qualified                SrcLoc
import qualified Data.Bifunctor as Bi
import qualified Data.Data      as Data
import qualified GhcMake
import qualified HscTypes       as Ghc
import qualified Id             as Ghc
import qualified MkCore         as Ghc
import qualified Var            as Ghc
import qualified WwLib          as Ghc
import           RnExpr         as Ghc (rnLExpr)
import           TcExpr         as Ghc (tcInferSigma)
import           TcBinds        as Ghc (tcValBinds)
import           Inst           as Ghc (deeplyInstantiate)
import           TcSimplify     as Ghc ( simplifyInfer, simplifyInteractive
                                       , InferMode (..))
import           TcHsSyn        as Ghc (zonkTopLExpr)
import           TcEvidence     as Ghc ( TcEvBinds (EvBinds))
import           DsExpr         as Ghc (dsLExpr)

import qualified Literal as Lit
import FastString        as Ghc hiding (bytesFS, LitString)
import TcType            as Ghc hiding (typeKind, mkFunTy)
import Type              as Ghc hiding (typeKind, mkFunTy, splitFunTys, extendCvSubst)
import qualified Type    as Ghc
import qualified Var     as Var
import qualified GHC.Real

import                   Binary
import                   Data.ByteString (ByteString)
import                   Data.Data (Data)
import TyCoRep           as Ghc hiding (Type (FunTy), mkFunTy, extendCvSubst)
import TyCon             as Ghc hiding (mkAnonTyConBinders, TyConBndrVis(AnonTCB))
import qualified TyCoRep as Ty hiding (extendCvSubst)
import qualified TyCon   as Ty

import FastString           as Ghc hiding (bytesFS)
import TcType               as Ghc hiding (typeKind, mkFunTy, isEqPred)
import Type                 as Ghc hiding (typeKind, mkFunTy, isEvVarType, isEqPred, splitFunTys, extendCvSubst)
import qualified Type       as Ghc
import qualified PrelNames  as Ghc
import Data.Foldable        (asum)

import GHC.Platform      as  Ghc (Platform)
import Type              as  Ghc hiding (mapType, typeKind, isPredTy, splitFunTys, extendCvSubst)
import qualified Type    as  Ghc hiding (extendCvSubst)
import TyCon             as  Ghc
import qualified TyCoRep as  Ty
import TcType            as  Ghc
import TyCoRep           as  Ghc hiding (Type (FunTy), mkFunTy, ft_arg, ft_res, ft_af)
import FastString        as  Ghc
import Predicate      as Ghc (getClassPredTys_maybe, isEvVarType, getClassPredTys, isDictId)
import TcOrigin       as Ghc (lexprCtOrigin)

import Optics
import qualified Control.Monad.Catch as Ex

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

-- Backporting multiplicity

data Scaled a = Scaled Mult a
  deriving (Data.Data)

instance (Outputable a) => Outputable (Scaled a) where
   ppr (Scaled _cnt t) = ppr t

irrelevantMult :: Scaled a -> a
irrelevantMult = scaledThing

mkScaled :: Mult -> a -> Scaled a
mkScaled = Scaled

scaledThing :: Scaled a -> a
scaledThing (Scaled _ t) = t

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

--
-- Dependencies and Boot
--
type IsBootInterface = GhcMake.IsBoot

-- | This data type just pairs a value 'mod' with an IsBootInterface flag. In
-- practice, 'mod' is usually a @Module@ or @ModuleName@'.
data GenWithIsBoot mod = GWIB
  { gwib_mod :: mod
  , gwib_isBoot :: IsBootInterface
  } deriving ( Eq, Ord, Show
             , Functor, Foldable, Traversable
             )

type ModuleNameWithIsBoot = GenWithIsBoot ModuleName

isBootSummary :: ModSummary -> IsBootInterface
isBootSummary ms = case Ghc.isBootSummary ms of
  True  -> GhcMake.IsBoot
  False -> GhcMake.NotBoot

getDependenciesModuleNames :: Dependencies -> [ModuleNameWithIsBoot]
getDependenciesModuleNames = map f . dep_mods
  where
    f :: (ModuleName, Bool) -> ModuleNameWithIsBoot
    f (modName, b) = let isBoot = if b then GhcMake.IsBoot else GhcMake.NotBoot in GWIB modName isBoot

dataConInstArgTys :: DataCon -> [Type] -> [Scaled Type]
dataConInstArgTys dc tys = map (mkScaled Many) (Ghc.dataConInstArgTys dc tys)

dataConOrigArgTys :: DataCon -> [Scaled Type]
dataConOrigArgTys dc = map (mkScaled Many) (Ghc.dataConOrigArgTys dc)

dataConRepArgTys :: DataCon -> [Scaled Type]
dataConRepArgTys dc = map (mkScaled Many) (Ghc.dataConRepArgTys dc)

mkLocalVar :: IdDetails -> Name -> Mult -> Type -> IdInfo -> Id
mkLocalVar idDetails' name _ ty info = Ghc.mkLocalVar idDetails' name ty info

mkUserLocal :: OccName -> Unique -> Mult -> Type -> SrcSpan -> Id
mkUserLocal occName' u _mult ty srcSpan = Ghc.mkUserLocal occName' u ty srcSpan

dataConWrapperType :: DataCon -> Type
dataConWrapperType = dataConUserType

-- WWlib

data DataConAppContext
  = DataConAppContext
  { dcac_dc      :: !DataCon
  , dcac_tys     :: ![Type]
  , dcac_arg_tys :: ![(Scaled Type, StrictnessMark)]
  , dcac_co      :: !Coercion
  }

deepSplitProductType_maybe :: FamInstEnvs -> Type -> Maybe DataConAppContext
deepSplitProductType_maybe famInstEnv ty = do
  (dc, tys, tysWithStricts, co) <- Ghc.deepSplitProductType_maybe famInstEnv ty
  pure $ DataConAppContext dc tys (map (Bi.first (mkScaled Many)) tysWithStricts) co

splitFunTys :: Type -> ([Scaled Type], Type)
splitFunTys ty = Bi.first (map (mkScaled Many)) $ Ghc.splitFunTys ty

apiAnnComments :: (Map ApiAnnKey [SrcSpan], Map SrcSpan [Located AnnotationComment])
               -> Map SrcSpan [Located AnnotationComment]
apiAnnComments = snd

mkIntExprInt :: Platform -> Int -> CoreExpr
mkIntExprInt _ = Ghc.mkIntExprInt unsafeGlobalDynFlags

dataConFullSig :: DataCon -> ([TyVar], [TyCoVar], [EqSpec], ThetaType, [Scaled Type], Type)
dataConFullSig dc =
  let (tyvars, tycovars, eqspecs, theta, tys, ty) = Ghc.dataConFullSig dc
  in  (tyvars, tycovars, eqspecs, theta, map (mkScaled Many) tys, ty)

isEqPrimPred :: Type -> Bool
isEqPrimPred = Ghc.isPredTy

-- See NOTE [isEvVarType].
isEvVarType :: Type -> Bool
isEvVarType = Ghc.isPredTy

tyConRealArity :: TyCon -> Int
tyConRealArity = tyConArity

-- | The non-dependent version of 'ArgFlag'.

-- Appears here partly so that it's together with its friend ArgFlag,
-- but also because it is used in IfaceType, rather early in the
-- compilation chain
-- See Note [AnonArgFlag vs. ForallVisFlag]
data AnonArgFlag
  = VisArg    -- ^ Used for @(->)@: an ordinary non-dependent arrow.
              --   The argument is visible in source code.
  | InvisArg  -- ^ Used for @(=>)@: a non-dependent predicate arrow.
              --   The argument is invisible in source code.
  deriving (Eq, Ord, Data)

bytesFS :: FastString -> ByteString
bytesFS = fastStringToByteString

isEqPred :: Type -> Bool
isEqPred ty
  | Just tc <- tyConAppTyCon_maybe ty
  , Just cls <- tyConClass_maybe tc
  = cls `hasKey` Ghc.eqTyConKey || cls `hasKey` Ghc.heqTyConKey
  | otherwise
  = False

{- | [NOTE:tyConRealArity]

The semantics of 'tyConArity' changed between GHC 8.6.5 and GHC 8.10, mostly due to the
Visible Dependent Quantification (VDQ). As a result, given the following:

data family EntityField record :: * -> *

Calling `tyConArity` on this would yield @2@ for 8.6.5 but @1@ an 8.10, so we try to backport
the old behaviour in 8.10 by \"looking\" at the 'Kind' of the input 'TyCon' and trying to recursively
split the type apart with either 'splitFunTy_maybe' or 'splitForAllTy_maybe'.

-}

{- | [NOTE:isEvVarType]

For GHC < 8.8.1 'isPredTy' is effectively the same as the new 'isEvVarType', which covers the cases
for coercion types and \"normal\" type coercions. The 8.6.5 version of 'isPredTy' had a special case to
handle a 'TyConApp' in the case of type equality (i.e. ~ ) which was removed in the implementation
for 8.8.1, which essentially calls 'tcIsConstraintKind' straight away.
-}

dataConExTyVars :: DataCon -> [TyVar]
dataConExTyVars = dataConExTyCoVars

-- 'fsToUnitId' is gone in GHC 9, but we can bring code it in terms of 'fsToUnit' and 'toUnitId'.
fsToUnitId :: FastString -> UnitId
fsToUnitId = toUnitId . fsToUnit

moduleUnitId :: Module -> UnitId
moduleUnitId = toUnitId . moduleUnit

thisPackage :: DynFlags -> UnitId
thisPackage = toUnitId . homeUnit

renderWithStyle :: DynFlags -> SDoc -> PprStyle -> String
renderWithStyle dynflags sdoc style = Ghc.renderWithStyle (Ghc.initSDocContext dynflags style) sdoc

mkUserStyle :: DynFlags -> PrintUnqualified -> Depth -> PprStyle
mkUserStyle _ = Ghc.mkUserStyle

--
-- Literal
--

-- In GHC 9 'LitNumber' doesn't have the extra 3rd argument, so we simply ignore it in the construction.

-- This function is gone in GHC 9.
dataConSig :: DataCon -> ([TyCoVar], ThetaType, [Type], Type)
dataConSig dc
  = (dataConUnivAndExTyCoVars dc, dataConTheta dc, map irrelevantMult $ dataConOrigArgTys dc, dataConOrigResTy dc)

gcatch :: (Ex.MonadCatch m, Exception e) => m a -> (e -> m a) -> m a
gcatch = Ex.catch