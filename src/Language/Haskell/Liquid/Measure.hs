{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}

module Language.Haskell.Liquid.Measure (
    Spec (..)
  , BareSpec
  , MSpec (..)
  , mkM, mkMSpec, mkMSpec'
  , qualifySpec
  , dataConTypes
  , defRefType
  , strLen
  , wiredInMeasures
  ) where

import GHC hiding (Located)
import Var
import Type
import TysPrim
import TysWiredIn
import Text.PrettyPrint.HughesPJ hiding (first)
import Text.Printf (printf)
import DataCon

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import Data.List (foldl', partition)

import Data.Monoid hiding ((<>))
import Data.Bifunctor
import Control.Applicative      ((<$>))

import Data.Maybe (fromMaybe, isNothing, fromJust, catMaybes)

import Language.Fixpoint.Misc
import Language.Fixpoint.Types hiding (Def, R)
import Language.Haskell.Liquid.GHC.Misc
import Language.Haskell.Liquid.Types    hiding (GhcInfo(..), GhcSpec (..))
import Language.Haskell.Liquid.Misc     (mapSnd)
import Language.Haskell.Liquid.Types.RefType
import Language.Haskell.Liquid.Types.Variance
import Language.Haskell.Liquid.Types.Bounds

-- MOVE TO TYPES
type BareSpec      = Spec BareType LocSymbol

data Spec ty bndr  = Spec
  { measures   :: ![Measure ty bndr]            -- ^ User-defined properties for ADTs
  , asmSigs    :: ![(LocSymbol, ty)]            -- ^ Assumed (unchecked) types
  , sigs       :: ![(LocSymbol, ty)]            -- ^ Imported functions and types
  , localSigs  :: ![(LocSymbol, ty)]            -- ^ Local type signatures
  , invariants :: ![Located ty]                 -- ^ Data type invariants
  , ialiases   :: ![(Located ty, Located ty)]   -- ^ Data type invariants to be checked
  , imports    :: ![Symbol]                     -- ^ Loaded spec module names
  , dataDecls  :: ![DataDecl]                   -- ^ Predicated data definitions
  , includes   :: ![FilePath]                   -- ^ Included qualifier files
  , aliases    :: ![RTAlias Symbol BareType]    -- ^ RefType aliases
  , ealiases   :: ![RTAlias Symbol Expr]        -- ^ Expression aliases
  , embeds     :: !(TCEmb (LocSymbol))          -- ^ GHC-Tycon-to-fixpoint Tycon map
  , qualifiers :: ![Qualifier]                  -- ^ Qualifiers in source/spec files
  , decr       :: ![(LocSymbol, [Int])]         -- ^ Information on decreasing arguments
  , lvars      :: ![(LocSymbol)]                -- ^ Variables that should be checked in the environment they are used
  , lazy       :: !(S.HashSet LocSymbol)        -- ^ Ignore Termination Check in these Functions
  , axioms     :: !(S.HashSet LocSymbol)        -- ^ Binders to turn into axiomatized functions
  , hmeas      :: !(S.HashSet LocSymbol)        -- ^ Binders to turn into measures using haskell definitions
  , hbounds    :: !(S.HashSet LocSymbol)        -- ^ Binders to turn into bounds using haskell definitions
  , inlines    :: !(S.HashSet LocSymbol)        -- ^ Binders to turn into logic inline using haskell definitions
  , autosize   :: !(S.HashSet LocSymbol)        -- ^ Type Constructors that get automatically sizing info
  , pragmas    :: ![Located String]             -- ^ Command-line configurations passed in through source
  , cmeasures  :: ![Measure ty ()]              -- ^ Measures attached to a type-class
  , imeasures  :: ![Measure ty bndr]            -- ^ Mappings from (measure,type) -> measure
  , classes    :: ![RClass ty]                  -- ^ Refined Type-Classes
  , termexprs  :: ![(LocSymbol, [Expr])]        -- ^ Terminating Conditions for functions
  , rinstance  :: ![RInstance ty]
  , dvariance  :: ![(LocSymbol, [Variance])]
  , bounds     :: !(RRBEnv ty)
  }


qualifySpec name sp = sp { sigs      = [ (tx x, t)  | (x, t)  <- sigs sp]
                         , asmSigs   = [ (tx x, t)  | (x, t)  <- asmSigs sp]
--                          , termexprs = [ (tx x, es) | (x, es) <- termexprs sp]
                         }
  where
    tx = fmap (qualifySymbol name)

mkM ::  LocSymbol -> ty -> [Def ty bndr] -> Measure ty bndr
mkM name typ eqns
  | all ((name ==) . measure) eqns
  = M name typ eqns
  | otherwise
  = errorstar $ "invalid measure definition for " ++ (show name)

-- mkMSpec :: [Measure ty LocSymbol] -> [Measure ty ()] -> [Measure ty LocSymbol]
--         -> MSpec ty LocSymbol

mkMSpec' ms = MSpec cm mm M.empty []
  where
    cm     = groupMap (symbol . ctor) $ concatMap eqns ms
    mm     = M.fromList [(name m, m) | m <- ms ]

mkMSpec ms cms ims = MSpec cm mm cmm ims
  where
    cm     = groupMap (val . ctor) $ concatMap eqns (ms'++ims)
    mm     = M.fromList [(name m, m) | m <- ms' ]
    cmm    = M.fromList [(name m, m) | m <- cms ]
    ms'    = checkDuplicateMeasure ms
    -- ms'    = checkFail "Duplicate Measure Definition" (distinct . fmap name) ms

--checkFail ::  [Char] -> (a -> Bool) -> a -> a
--checkFail msg f x
--  | f x
--  = x
--  | otherwise
--  = errorstar $ "Check-Failure: " ++ msg

--distinct ::  Ord a => [a] -> Bool
--distinct xs = length xs == length (sortNub xs)


checkDuplicateMeasure ms
  = case M.toList dups of
      []         -> ms
      mms        -> errorstar $ concatMap err mms
    where
      gms        = group [(name m , m) | m <- ms]
      dups       = M.filter ((1 <) . length) gms
      err (m,ms) = printf "\nDuplicate Measure Definitions for %s\n%s" (showpp m) (showpp $ map (loc . name) ms)




-- MOVE TO TYPES
instance Monoid (Spec ty bndr) where
  mappend s1 s2
    = Spec { measures   =           measures s1   ++ measures s2
           , asmSigs    =           asmSigs s1    ++ asmSigs s2
           , sigs       =           sigs s1       ++ sigs s2
           , localSigs  =           localSigs s1  ++ localSigs s2
           , invariants =           invariants s1 ++ invariants s2
           , ialiases   =           ialiases s1   ++ ialiases s2
           , imports    = sortNub $ imports s1    ++ imports s2
           , dataDecls  = dataDecls s1            ++ dataDecls s2
           , includes   = sortNub $ includes s1   ++ includes s2
           , aliases    =           aliases s1    ++ aliases s2
           , ealiases   =           ealiases s1   ++ ealiases s2
           , embeds     = M.union   (embeds s1)      (embeds s2)
           , qualifiers =           qualifiers s1 ++ qualifiers s2
           , decr       =           decr s1       ++ decr s2
           , lvars      =           lvars s1      ++ lvars s2
           , lazy       = S.union   (lazy s1)        (lazy s2)
           , axioms     = S.union   (axioms s1)      (axioms s2)
           , hmeas      = S.union   (hmeas s1)       (hmeas s2)
           , hbounds    = S.union   (hbounds s1)     (hbounds s2)
           , inlines    = S.union   (inlines s1)     (inlines s2)
           , autosize   = S.union   (autosize s1)    (autosize s2)
           , pragmas    =           pragmas s1    ++ pragmas s2
           , cmeasures  =           cmeasures s1  ++ cmeasures s2
           , imeasures  =           imeasures s1  ++ imeasures s2
           , classes    =           classes s1    ++ classes s1
           , termexprs  =           termexprs s1  ++ termexprs s2
           , rinstance  =           rinstance s1  ++ rinstance s2
           , dvariance  =           dvariance s1  ++ dvariance s2
           , bounds     = M.union   (bounds s1)      (bounds s2)
           }

  mempty
    = Spec { measures   = []
           , asmSigs    = []
           , sigs       = []
           , localSigs  = []
           , invariants = []
           , ialiases   = []
           , imports    = []
           , dataDecls  = []
           , includes   = []
           , aliases    = []
           , ealiases   = []
           , embeds     = M.empty
           , qualifiers = []
           , decr       = []
           , lvars      = []
           , lazy       = S.empty
           , hmeas      = S.empty
           , axioms     = S.empty
           , hbounds    = S.empty
           , inlines    = S.empty
           , autosize   = S.empty
           , pragmas    = []
           , cmeasures  = []
           , imeasures  = []
           , classes    = []
           , termexprs  = []
           , rinstance  = []
           , dvariance  = []
           , bounds     = M.empty
           }

dataConTypes :: MSpec (RRType Reft) DataCon -> ([(Var, RRType Reft)], [(LocSymbol, RRType Reft)])
dataConTypes  s = (ctorTys, measTys)
  where
    measTys     = [(name m, sort m) | m <- M.elems (measMap s) ++ imeas s]
    ctorTys     = concatMap makeDataConType (snd <$> (M.toList $ ctorMap s))



makeDataConType :: [Def (RRType Reft) DataCon] -> [(Var, RRType Reft)]
makeDataConType [] 
  = []
makeDataConType ds | isNothing (dataConWrapId_maybe dc)
  = [(woId, combineDCTypes t ts)]
  where
    dc   = ctor $ head ds  
    woId = dataConWorkId dc 
    t    = varType woId
    ts   = defRefType t <$> ds 

makeDataConType ds 
  = [(woId, extend loci woRType wrRType), (wrId, extend loci wrRType woRType)]
  where
    (wo, wr) = partition isWorkerDef ds 
    dc       = ctor $ head ds 
    loci     = loc $ measure $ head ds 
    woId     = dataConWorkId dc 
    wot      = varType woId
    wrId     = dataConWrapId dc 
    wrt      = varType wrId
    wots     = defRefType wot <$> wo 
    wrts     = defRefType wrt <$> wr 

    wrRType  = combineDCTypes wrt wrts
    woRType  = combineDCTypes wot wots


    isWorkerDef def
      -- types are missing for arguments, so definition came from a logical measure
      -- and it is for the worker datacon
      | any isNothing (snd <$> binds def)
      = True 
      | otherwise 
      = length (binds def) == length (fst $ splitFunTys $ snd $ splitForAllTys wot)


extend lc t1' t2 
  | Just su <- mapArgumens lc t1 t2 
  = t1 `strengthenResult` (subst su $ fromMaybe mempty (stripRTypeBase $ resultTy t2))
  | otherwise
  = t1
  where 
    t1 = noDummySyms t1' 


resultTy = ty_res . toRTypeRep 

strengthenResult t r = fromRTypeRep $ rep{ty_res = ty_res rep `strengthen` r}
  where
    rep = toRTypeRep t


noDummySyms t 
  | any isDummy (ty_binds rep)
  = subst su $ fromRTypeRep $ rep{ty_binds = xs'} 
  | otherwise
  = t 
  where
    rep = toRTypeRep t
    xs' = zipWith (\_ i -> symbol ("x" ++ show i)) (ty_binds rep) [1..]
    su  = mkSubst $ zip (ty_binds rep) (EVar <$> xs')

combineDCTypes t = foldl' strengthenRefTypeGen (ofType t) 

mapArgumens :: SourcePos -> RRType Reft -> RRType Reft -> Maybe Subst  
mapArgumens lc t1 t2 = go xts1' xts2' 
  where
    xts1 = zip (ty_binds rep1) (ty_args rep1)
    xts2 = zip (ty_binds rep2) (ty_args rep2)
    rep1 = toRTypeRep t1 
    rep2 = toRTypeRep t2 

    xts1' = dropWhile canDrop xts1 
    xts2' = dropWhile canDrop xts2 

    canDrop (_, t) = isClassType t || isEqType t

    go xs ys 
      | length xs == length ys && and (zipWith (==) (toRSort . snd <$> xts1') (toRSort . snd <$> xts2')) 
      = Just $ mkSubst $ zipWith (\y x -> (fst x, EVar $ fst y)) xts1' xts2'
      | otherwise
      = panic (Just $ sourcePosSrcSpan lc) ("The types for the wrapper and worker data constroctors cannot be merged\n"
          ++ show t1 ++ "\n" ++ show t2 )  

defRefType :: Type -> Def (RRType Reft) DataCon -> RRType Reft
defRefType tdc (Def f args dc mt xs body) = generalize $ mkArrow [] [] [] xts t'
  where
    t   = fromMaybe (ofType tr) mt
    xts = safeZipWith msg g xs $ ofType `fmap` ts
    g (x, Nothing) t = (x, t, mempty)
    g (x, Just t)  _ = (x, t, mempty)
    t'  = mkForAlls args $ refineWithCtorBody dc f (fst <$> args) body t
    msg = "defRefType dc = " ++ showPpr dc 

    mkForAlls xts t = foldl' (\t (x, tx) -> RAllE x tx t) t xts

    (ts, tr) = splitFunTys $ snd $ splitForAllTys tdc

refineWithCtorBody dc f as body t =
  case stripRTypeBase t of
    Just (Reft (v, _)) ->
      strengthen t $ Reft (v, bodyPred (mkEApp f (eVar <$> (as ++ [v]))) body)
    Nothing ->
      errorstar $ "measure mismatch " ++ showpp f ++ " on con " ++ showPpr dc


bodyPred ::  Expr -> Body -> Expr
bodyPred fv (E e)    = PAtom Eq fv e
bodyPred fv (P p)    = PIff  fv p
bodyPred fv (R v' p) = subst1 p (v', fv)


-- | A wired-in measure @strLen@ that describes the length of a string
-- literal, with type @Addr#@.
strLen :: Measure SpecType ctor
strLen = M { name = dummyLoc "strLen"
           , sort = ofType (mkFunTy addrPrimTy intTy)
           , eqns = []
           }

wiredInMeasures :: MSpec SpecType DataCon
wiredInMeasures = mkMSpec' [strLen]
