module Internal.GHC.API.Common (
      ErrMsg(..)
    , SourceError
    , SourceText(..)
    , RealSrcSpan(..)
    , SrcSpan
    , SrcLoc(..)
    , GenLocated(..)
    , Located
    , FastString
    , bagToList
    , srcErrorMessages
    , noSrcSpan
    , fsLit, unpackFS
    , mkRealSrcLoc
    , srcSpanStartLine, srcSpanEndLine
    , srcSpanStartCol, srcSpanEndCol
    , mkRealSrcSpan
    , Name
    , NamedThing
    , RdrName
    , AvailInfo
    , CoreProgram
    , Dependencies
    , GlobalRdrEnv
    , FindResult(..)
    , Module(..)
    , ModuleName
    , ModuleGraph
    , HomeModInfo(..)
    , DesugaredModule(..)
    , TypecheckedModule(..)
    , ParsedModule(..)
    , HsModule(..)
    , HsParsedModule(..)
    , ModSummary(..)
    , ModLocation
    , ModGuts(..)
    , NameSet
    , UniqFM
    , IfM
    , RenamedSource
    , ClsInst
    , FamInst
    , Id
    , IdDetails(..)
    , TyCon
    , TyThing(..)
    , DynFlags(..)
    , HasDynFlags
    , PromotionFlag(..)
    , TopLevelFlag(..)
    , TcGblEnv
    , IsBootInterface
    , Hsc
    , HscEnv
    , TcM
    , TcRn
    , TcRnExprMode(..)
    , GhcT
    , Ghc
    , GhcRn
    , GhcPs
    , ApiAnns
    , AnnotationComment
    , moduleNameString
    , nameModule
    , nameOccName
    , nameUnique
    , occNameString
    , nameStableString
    , nameSrcLoc
    , nameSrcSpan
    , nameSetElemsStable
    , getName
    , mkInternalName
    , mkVarOcc
    , availNames
    , alwaysQualifyNames
    , queryQualifyName
    , neverQualify
    , hsc_dflags
    , findExposedPackageModule
    , throwOneError
    , throwGhcExceptionIO
    , lookupHpt
    , thisPackage
    , fsToUnitId
    , getDynFlags
    , hscDesugar
    , hscTypecheckRename
    , hscParse
    , hsc_HPT
    , hscTcRcLookupName
    , lookupTypeHscEnv
    , lookupType
    , lookupTypeEnv
    , isBootSummary
    , getTopEnv
    , GhcMonad(..)
    , ParsedMod(..)

    , Outputable
    , SDoc
    , MsgDoc
    , Messages
    , showSDoc
    , showSDocDump
    , showSDocDebug
    , putLogMsg
    , ppr
    , PprStyle
    , Depth(..)
    , PrintUnqualified
    , Severity(..)
    , WarnReason(..)
    , defaultErrStyle
    , text
    , ptext
    , hsep
    , parens
    , quotes
    , (<+>)
    , panic
    , mkLongErrAt
    , mkUserStyle
    , renderWithStyle
    , FixityDirection(..)
    , Fixity(..)
    , Arity
    , Bag

    , Expr(..)
    , Class
    , Var(..)
    , Type(..)
    , Kind
    , LHsExpr
    , LHsType
    , LHsDecl
    , PredType
    , CoreExpr
    , CoreBind
    , Bind(..)
    , DataCon
    , AltCon(..)
    , TyVar
    , TyConBinder
    , Tickish(..)
    , Unique
    , Uniquable
    , HsDecl(..)
    , Sig(..)
    , AnonArgFlag(..)
    , noExtField
    , dataConInstArgTys
    , exprType
    , lintCoreBindings
    , synTyConRhs_maybe
    , isPrimTyCon
    , tyConTyVars
    , mkKindTyCon
    , mkTyConTy
    , tyConDataCons_maybe
    , lexprCtOrigin
    , getClassPredTys
    , tcRnLookupRdrName
    , promoteDataCon
    , isPromotedDataCon
    , isTupleDataCon
    , mkPreludeTyConUnique
    , mkPreludeDataConUnique
    , mkWiredInName
    , mkWiredInTyConName
    , tyConAppTyCon_maybe
    , gHC_TYPES
    , mkDataOccFS
    , mkAlgTyCon
    , mkAnonTyConBinders
    , mkDataTyConRhs
    , mkPrelTyConRepName
    , liftedTypeKind
    , hasKey
    , mkDataCon
    , mkTyConApp
    , mkTyVarTys
    , mkTyConTagMap
    , mkDataConWorkId
    , dataConWorkerUnique
    , dataConName
    , dataConWorkId
    , lookupNameEnv_NF
    , mkDataConWorkerOcc
    , HsSrcBang(..)
    , BuiltInSyntax(..)
    , ConLike(..)
    , CType
    , AlgTyConFlav(..)
    , Role(..)
    , DataConRep(..)
    , RuntimeRepInfo(..)
    , SrcUnpackedness(..)
    , SrcStrictness(..)
    ) where

import GhcPlugins
import ErrUtils (ErrMsg(..), Severity(..), MsgDoc, Messages)
import Bag (bagToList, Bag)
import Avail (AvailInfo, availNames)
import GHC
    ( Ghc, ApiAnns, AnnotationComment, RenamedSource
    , ParsedModule(..), TypecheckedModule(..)
    , GhcT, ClsInst, FamInst, GhcRn, Class, LHsExpr, LHsType, LHsDecl
    , GhcPs, DesugaredModule(..), HsModule(..), HsDecl(..), Sig(..), GhcMonad (..), ParsedMod(..)
    )
import TcRnTypes (IfM, TcGblEnv, TcM)
import TcRnMonad (TcRn, mkLongErrAt, getTopEnv)
import TcRnDriver (TcRnExprMode(..), tcRnLookupRdrName)
import GHC.Hs (noExtField)
import CoreLint (lintCoreBindings)
import TcOrigin (lexprCtOrigin)
import Predicate (getClassPredTys)
import Finder (findExposedPackageModule)
import Panic (throwGhcExceptionIO)
import HscMain (hscDesugar, hscTypecheckRename, hscParse, hscTcRcLookupName)
import TyCoRep (Type(TyVarTy, AppTy, TyConApp, ForAllTy))
import Unique (mkPreludeTyConUnique, mkPreludeDataConUnique, hasKey, dataConWorkerUnique)
import PrelNames (gHC_TYPES)
import ConLike (ConLike(..))
import ForeignCall (CType)
import MkId (mkDataConWorkId)
{-
import HscTypes (SourceError, srcErrorMessages, ModGuts(..), Dependencies, HsParsedModule(..), IsBootInterface, Hsc, HscEnv, hsc_dflags)
import GHC (RealSrcSpan(..), SrcSpan, noSrcSpan, srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol
   , ModuleName, FamInst, ClsInst, TyCon, Module(..), Name, moduleNameString, nameModule
   , ModSummary(..), TypecheckedModule(..), ParsedModule(..), DynFlags(..), ModLocation, ModuleGraph, TyThing, ApiAnns, AnnotationComment, RenamedSource, GhcT, Ghc)
import GHC.Exception ( SrcLoc(..) )
import FastString ( FastString, fsLit, unpackFS )
import Bag (bagToList)
import NameSet ( NameSet, nameSetElemsStable )
import Avail (AvailInfo, availNames)
import CoreSyn (CoreProgram)
import RdrName (GlobalRdrEnv)
import Name (nameOccName, nameStableString, occNameString, nameSrcSpan, nameSrcLoc)
import DynFlags (HasDynFlags)
import TcRnTypes ( TcGblEnv, IfM )
import TcRnMonad (TcM)
import Finder (FindResult(..))
import UniqFM (UniqFM)
import SrcLoc (mkRealSrcSpan)
-}
