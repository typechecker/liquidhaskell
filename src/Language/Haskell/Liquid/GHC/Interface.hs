{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}

module Language.Haskell.Liquid.GHC.Interface (

  -- * extract all information needed for verification
    getGhcInfos
  , runLiquidGhc

  -- * printer
  , pprintCBs

  -- * predicates
  , isExportedVar
  , exportedVars
  ) where

import Prelude hiding (error)

import qualified Outputable as O
import GHC hiding (Target, Located, desugarModule)
import qualified GHC
import GHC.Paths (libdir)
import GHC.Serialized

import qualified Language.Haskell.Liquid.GHC.API as Ghc
import Annotations
import Class
import CoreMonad
import CoreSyn
import DataCon
import Digraph
import DriverPhases
import DriverPipeline
import DynFlags
import Finder
import HscTypes hiding (Target)
import IdInfo
import InstEnv
import Module
import Panic (throwGhcExceptionIO)
-- import Serialized
import TcRnTypes
import Var
import NameSet
import FastString
import FamInstEnv
import FamInst
import qualified TysPrim
import GHC.LanguageExtensions

import Control.Exception
import Control.Monad

import Data.Bifunctor
import Data.Data
import Data.List hiding (intersperse)
import Data.Maybe

import Data.Generics.Aliases (mkT)
import Data.Generics.Schemes (everywhere)

import qualified Data.HashSet        as S
import qualified Data.Map            as M

import System.Console.CmdArgs.Verbosity hiding (Loud)
import System.Directory
import System.FilePath
import System.IO.Temp
import Text.Parsec.Pos
import Text.PrettyPrint.HughesPJ        hiding (first, (<>))
import Language.Fixpoint.Types          hiding (panic, Error, Result, Expr)
import qualified Language.Fixpoint.Misc as Misc
import Language.Haskell.Liquid.Bare
import Language.Haskell.Liquid.GHC.Misc
import Language.Haskell.Liquid.GHC.Play
import Language.Haskell.Liquid.WiredIn (isDerivedInstance) 
import qualified Language.Haskell.Liquid.Measure  as Ms
import qualified Language.Haskell.Liquid.Misc     as Misc
import Language.Haskell.Liquid.Parse
import Language.Haskell.Liquid.Transforms.ANF
import Language.Haskell.Liquid.Types hiding (Spec)
-- import Language.Haskell.Liquid.Types.PrettyPrint
-- import Language.Haskell.Liquid.Types.Visitors
import Language.Haskell.Liquid.UX.CmdLine
import Language.Haskell.Liquid.UX.Config (totalityCheck)
import Language.Haskell.Liquid.UX.QuasiQuoter
import Language.Haskell.Liquid.UX.Tidy
import Language.Fixpoint.Utils.Files

import qualified Debug.Trace as Debug 

--------------------------------------------------------------------------------
-- | GHC Interface Pipeline ----------------------------------------------------
--------------------------------------------------------------------------------

getGhcInfos :: Maybe HscEnv -> Config -> [FilePath] -> IO ([GhcInfo], HscEnv)
getGhcInfos hscEnv cfg tgtFiles' = do
  tgtFiles <- mapM canonicalizePath tgtFiles'
  _        <- mapM checkFilePresent tgtFiles
  _        <- mapM_ createTempDirectoryIfMissing tgtFiles
  logicMap <- liftIO makeLogicMap
  runLiquidGhc hscEnv cfg (getGhcInfos' cfg logicMap tgtFiles)

checkFilePresent :: FilePath -> IO ()
checkFilePresent f = do
  b <- doesFileExist f
  when (not b) $ panic Nothing ("Cannot find file: " ++ f)

getGhcInfos' :: Config -> LogicMap -> [FilePath] -> Ghc ([GhcInfo], HscEnv)
getGhcInfos' cfg logicMap tgtFiles = do
  _           <- compileCFiles cfg
  homeModules <- configureGhcTargets tgtFiles
  depGraph    <- buildDepGraph homeModules
  ghcInfos    <- processModules cfg logicMap tgtFiles depGraph homeModules
  hscEnv      <- getSession
  return (ghcInfos, hscEnv)

createTempDirectoryIfMissing :: FilePath -> IO ()
createTempDirectoryIfMissing tgtFile = Misc.tryIgnore "create temp directory" $
  createDirectoryIfMissing False $ tempDirectory tgtFile

--------------------------------------------------------------------------------
-- | GHC Configuration & Setup -------------------------------------------------
--------------------------------------------------------------------------------

runLiquidGhc :: Maybe HscEnv -> Config -> Ghc a -> IO a
runLiquidGhc hscEnv cfg act =
  withSystemTempDirectory "liquid" $ \tmp ->
    runGhc (Just libdir) $ do
      maybe (return ()) setSession hscEnv
      df <- configureDynFlags cfg tmp
      prettyPrintGhcErrors df act

configureDynFlags :: Config -> FilePath -> Ghc DynFlags
configureDynFlags cfg tmp = do
  df <- getSessionDynFlags
  (df',_,_) <- parseDynamicFlags df $ map noLoc $ ghcOptions cfg
  loud <- liftIO isLoud
  let df'' = df' { importPaths  = nub $ idirs cfg ++ importPaths df'
                 , libraryPaths = nub $ idirs cfg ++ libraryPaths df'
                 , includePaths = nub $ idirs cfg ++ includePaths df'
                 , packageFlags = ExposePackage ""
                                                (PackageArg "ghc-prim")
                                                (ModRenaming True [])
                                : (packageFlags df')

                 -- , profAuto     = ProfAutoCalls
                 , ghcLink      = LinkInMemory
                 , hscTarget    = HscInterpreted
                 , ghcMode      = CompManager
                 -- prevent GHC from printing anything, unless in Loud mode
                 , log_action   = if loud
                                    then defaultLogAction
                                    else \_ _ _ _ _ _ -> return ()
                 -- redirect .hi/.o/etc files to temp directory
                 , objectDir    = Just tmp
                 , hiDir        = Just tmp
                 , stubDir      = Just tmp
                 } `gopt_set` Opt_ImplicitImportQualified
                   `gopt_set` Opt_PIC
                   `xopt_set` MagicHash
                   `xopt_set` DeriveGeneric
                   `xopt_set` StandaloneDeriving
  _ <- setSessionDynFlags df''
  return df''

configureGhcTargets :: [FilePath] -> Ghc ModuleGraph
configureGhcTargets tgtFiles = do
  targets         <- mapM (`guessTarget` Nothing) tgtFiles
  _               <- setTargets targets
  moduleGraph     <- depanal [] False
                     -- NOTE: drop hs-boot files from the graph.
                     -- we do it manually rather than using the flag to topSortModuleGraph
                     -- because otherwise the order of mutually recursive modules depends
                     -- on the modulename, e.g. given
                     --   Bar.hs --> Foo.hs --> Bar.hs-boot
                     -- we'll get
                     --   [Bar.hs, Foo.hs]
                     -- wich is backwards..
  let homeModules  = filter (not . isBootSummary) $
                     flattenSCCs $ topSortModuleGraph False moduleGraph Nothing
  let homeNames    = moduleName . ms_mod <$> homeModules
  _               <- setTargetModules homeNames
  return $ mkModuleGraph homeModules

setTargetModules :: [ModuleName] -> Ghc ()
setTargetModules modNames = setTargets $ mkTarget <$> modNames
  where
    mkTarget modName = GHC.Target (TargetModule modName) True Nothing

compileCFiles :: Config -> Ghc ()
compileCFiles cfg = do
  df  <- getSessionDynFlags
  _   <- setSessionDynFlags $
           df { includePaths = nub $ idirs cfg ++ includePaths df
              , importPaths  = nub $ idirs cfg ++ importPaths df
              , libraryPaths = nub $ idirs cfg ++ libraryPaths df }
  hsc <- getSession
  os  <- mapM (\x -> liftIO $ compileFile hsc StopLn (x,Nothing)) (nub $ cFiles cfg)
  df  <- getSessionDynFlags
  void $ setSessionDynFlags $ df { ldInputs = nub $ map (FileOption "") os ++ ldInputs df }

--------------------------------------------------------------------------------
-- Home Module Dependency Graph ------------------------------------------------
--------------------------------------------------------------------------------

type DepGraph = Graph DepGraphNode
type DepGraphNode = Node Module ()

reachableModules :: DepGraph -> Module -> [Module]
reachableModules depGraph mod =
  node_key <$> tail (reachableG depGraph (DigraphNode () mod []))

buildDepGraph :: ModuleGraph -> Ghc DepGraph
buildDepGraph homeModules =
  graphFromEdgedVerticesOrd <$> mapM mkDepGraphNode (mgModSummaries homeModules)

mkDepGraphNode :: ModSummary -> Ghc DepGraphNode
mkDepGraphNode modSummary = 
  DigraphNode () (ms_mod modSummary) <$> (filterM isHomeModule =<< modSummaryImports modSummary)

isHomeModule :: Module -> Ghc Bool
isHomeModule mod = do
  homePkg <- thisPackage <$> getSessionDynFlags
  return   $ moduleUnitId mod == homePkg

modSummaryImports :: ModSummary -> Ghc [Module]
modSummaryImports modSummary =
  mapM (importDeclModule (ms_mod modSummary))
       (ms_textual_imps modSummary)

importDeclModule :: Module -> (Maybe FastString,  GHC.Located ModuleName) -> Ghc Module
importDeclModule fromMod (pkgQual, locModName) = do
  hscEnv <- getSession
  let modName = unLoc locModName
  result <- liftIO $ findImportedModule hscEnv modName pkgQual
  case result of
    Finder.Found _ mod -> return mod
    _ -> do
      dflags <- getSessionDynFlags
      liftIO $ throwGhcExceptionIO $ ProgramError $
        O.showPpr dflags (moduleName fromMod) ++ ": " ++
        O.showSDoc dflags (cannotFindModule dflags modName result)

--------------------------------------------------------------------------------
-- | Extract Ids ---------------------------------------------------------------
--------------------------------------------------------------------------------

exportedVars :: GhcSrc -> [Var]
exportedVars src = filter (isExportedVar src) (giDefVars src)

isExportedVar :: GhcSrc -> Var -> Bool
isExportedVar info v = n `elemNameSet` ns
  where
    n                = getName v
    ns               = gsExports info


classCons :: Maybe [ClsInst] -> [Id]
classCons Nothing   = []
classCons (Just cs) = concatMap (dataConImplicitIds . head . tyConDataCons . classTyCon . is_cls) cs

derivedVars :: Config -> MGIModGuts -> [Var]  
derivedVars cfg mg  = tracepp "DERIVED-VARS" 
                    $ concatMap (dFunIdVars cbs . is_dfun) derInsts 
  where 
    derInsts        
      | checkDer    = insts 
      | otherwise   = filter isDerivedInstance insts
    insts           = mgClsInstances mg 
    checkDer        = checkDerived cfg
    cbs             = mgi_binds mg
               

mgClsInstances :: MGIModGuts -> [ClsInst]
mgClsInstances = fromMaybe [] . mgi_cls_inst 

dFunIdVars :: CoreProgram -> DFunId -> [Id]
dFunIdVars cbs fd  = tracepp msg $ concatMap bindersOf cbs' ++ deps
  where
    msg            = "DERIVED-VARS-OF: " ++ showpp fd
    cbs'           = filter f cbs
    f (NonRec x _) = eqFd x
    f (Rec xes)    = any eqFd (fst <$> xes)
    eqFd x         = varName x == varName fd
    deps           = concatMap unfoldDep unfolds
    unfolds        = unfoldingInfo . idInfo <$> concatMap bindersOf cbs'

unfoldDep :: Unfolding -> [Id]
unfoldDep (DFunUnfolding _ _ e)       = concatMap exprDep e
unfoldDep CoreUnfolding {uf_tmpl = e} = exprDep e
unfoldDep _                           = []

exprDep :: CoreExpr -> [Id]
exprDep = freeVars S.empty

importVars :: CoreProgram -> [Id]
importVars = freeVars S.empty

_definedVars :: CoreProgram -> [Id]
_definedVars = concatMap defs
  where
    defs (NonRec x _) = [x]
    defs (Rec xes)    = map fst xes

--------------------------------------------------------------------------------
-- | Per-Module Pipeline -------------------------------------------------------
--------------------------------------------------------------------------------

type SpecEnv = ModuleEnv (ModName, Ms.BareSpec)

processModules :: Config -> LogicMap -> [FilePath] -> DepGraph -> ModuleGraph -> Ghc [GhcInfo]
processModules cfg logicMap tgtFiles depGraph homeModules = do
  -- DO NOT DELETE: liftIO $ putStrLn $ "Process Modules: TargetFiles = " ++ show tgtFiles
  catMaybes . snd <$> Misc.mapAccumM go emptyModuleEnv (mgModSummaries homeModules)
  where                                             
    go = processModule cfg logicMap (S.fromList tgtFiles) depGraph

processModule :: Config -> LogicMap -> S.HashSet FilePath -> DepGraph -> SpecEnv -> ModSummary
              -> Ghc (SpecEnv, Maybe GhcInfo)
processModule cfg logicMap tgtFiles depGraph specEnv modSummary = do
  let mod              = ms_mod modSummary
  -- DO-NOT-DELETE _                <- liftIO $ putStrLn $ "Process Module: " ++ showPpr (moduleName mod)
  file                <- liftIO $ canonicalizePath $ modSummaryHsFile modSummary
  let isTarget         = file `S.member` tgtFiles
  _                   <- loadDependenciesOf $ moduleName mod
  parsed              <- parseModule $ keepRawTokenStream modSummary
  let specComments     = extractSpecComments parsed
  typechecked         <- typecheckModule $ ignoreInline parsed
  let specQuotes       = extractSpecQuotes typechecked
  _                   <- loadModule' typechecked
  (modName, commSpec) <- either throw return $ hsSpecificationP (moduleName mod) specComments specQuotes
  liftedSpec          <- liftIO $ if isTarget then return mempty else loadLiftedSpec cfg file -- modName
  let bareSpec         = updLiftedSpec commSpec liftedSpec
  _                   <- checkFilePragmas $ Ms.pragmas bareSpec
  let specEnv'         = extendModuleEnv specEnv mod (modName, noTerm bareSpec)
  (specEnv', ) <$> if isTarget
                     then Just <$> processTargetModule cfg logicMap depGraph specEnv file typechecked bareSpec
                     else return Nothing

updLiftedSpec :: Ms.BareSpec -> Ms.BareSpec -> Ms.BareSpec 
updLiftedSpec s1 s2 = s1' `mappend` s2 
  where 
    s1'             = s1 { sigs = filter notRefl (Ms.sigs s1) }
    notRefl         = not . (`S.member` refls) . val . fst 
    refls           = S.fromList [ val lx | (lx, _) <- Ms.asmSigs s2 ]

keepRawTokenStream :: ModSummary -> ModSummary
keepRawTokenStream modSummary = modSummary
  { ms_hspp_opts = ms_hspp_opts modSummary `gopt_set` Opt_KeepRawTokenStream }

loadDependenciesOf :: ModuleName -> Ghc ()
loadDependenciesOf modName = do
  loadResult <- load $ LoadDependenciesOf modName
  when (failed loadResult) $ liftIO $ throwGhcExceptionIO $ ProgramError $
   "Failed to load dependencies of module " ++ showPpr modName

loadModule' :: TypecheckedModule -> Ghc TypecheckedModule
loadModule' tm = loadModule tm'
  where
    pm   = tm_parsed_module tm
    ms   = pm_mod_summary pm
    df   = ms_hspp_opts ms
    df'  = df { hscTarget = HscNothing, ghcLink = NoLink }
    ms'  = ms { ms_hspp_opts = df' }
    pm'  = pm { pm_mod_summary = ms' }
    tm'  = tm { tm_parsed_module = pm' }


processTargetModule :: Config -> LogicMap -> DepGraph -> SpecEnv -> FilePath -> TypecheckedModule -> Ms.BareSpec
                    -> Ghc GhcInfo
processTargetModule cfg0 logicMap depGraph specEnv file typechecked bareSpec = do
  cfg        <- liftIO $ withPragmas cfg0 file (Ms.pragmas bareSpec)
  let modSum  = pm_mod_summary (tm_parsed_module typechecked)
  ghcSrc     <- makeGhcSrc    cfg file     typechecked modSum
  bareSpecs  <- makeBareSpecs cfg depGraph specEnv     modSum bareSpec
  let ghcSpec = makeGhcSpec   cfg ghcSrc   logicMap           bareSpecs  
  _          <- liftIO $ saveLiftedSpec ghcSrc ghcSpec 
  return      $ GI ghcSrc ghcSpec

---------------------------------------------------------------------------------------
-- | @makeGhcSrc@ builds all the source-related information needed for consgen 
---------------------------------------------------------------------------------------

makeGhcSrc :: Config -> FilePath -> TypecheckedModule -> ModSummary -> Ghc GhcSrc 
makeGhcSrc cfg file typechecked modSum = do
  desugared         <- desugarModule  typechecked
  let modGuts        = makeMGIModGuts desugared   
  let modGuts'       = dm_core_module desugared
  hscEnv            <- getSession
  -- _                 <- liftIO $ dumpRdrEnv hscEnv modGuts
  -- _                 <- liftIO $ dumpTypeEnv typechecked 
  coreBinds         <- liftIO $ anormalize cfg hscEnv modGuts'
  _                 <- liftIO $ whenNormal $ Misc.donePhase Misc.Loud "A-Normalization"
  let dataCons       = concatMap (map dataConWorkId . tyConDataCons) (mgi_tcs modGuts)
  -- let defVs          = definedVars coreBinds
  (fiTcs, fiDcs)    <- liftIO $ makeFamInstEnv hscEnv 
  things            <- lookupTyThings hscEnv typechecked modGuts 
  -- _                 <- liftIO $ print (showpp things)
  let impVars        = importVars coreBinds ++ classCons (mgi_cls_inst modGuts)
  return $ Src 
    { giTarget    = file
    , giTargetMod = ModName Target (moduleName (ms_mod modSum))
    , giCbs       = coreBinds
    , giImpVars   = impVars 
    , giDefVars   = dataCons ++ (letVars coreBinds) 
    , giUseVars   = readVars coreBinds
    , giDerVars   = S.fromList (derivedVars cfg modGuts) 
    , gsExports   = mgi_exports  modGuts 
    , gsTcs       = mgi_tcs      modGuts
    , gsCls       = mgi_cls_inst modGuts 
    , gsFiTcs     = fiTcs 
    , gsFiDcs     = fiDcs
    , gsPrimTcs   = TysPrim.primTyCons
    , gsQualImps  = qualifiedImports typechecked 
    , gsAllImps   = allImports       typechecked
    , gsTyThings  = {- impThings impVars -} [ t | (_, Just t) <- things ] 
    }

    
_impThings :: [Var] -> [TyThing] -> [TyThing]
_impThings vars  = filter ok
  where
    vs          = S.fromList vars 
    ok (AnId x) = S.member x vs  
    ok _        = True 

allImports :: TypecheckedModule -> S.HashSet Symbol 
allImports tm = case tm_renamed_source tm of 
  Nothing           -> Debug.trace "WARNING: Missing RenamedSource" mempty 
  Just (_,imps,_,_) -> S.fromList (symbol . unLoc . ideclName . unLoc <$> imps) 

qualifiedImports :: TypecheckedModule -> QImports 
qualifiedImports tm = case tm_renamed_source tm of 
  Nothing           -> Debug.trace "WARNING: Missing RenamedSource" mempty 
  Just (_,imps,_,_) -> Misc.group [ (qn, n) | i         <- imps
                                            , let decl   = unLoc i
                                            , let m      = unLoc (ideclName decl)  
                                            , qm        <- maybeToList (unLoc <$> ideclAs decl) 
                                            , let [n,qn] = symbol <$> [m, qm] 
                                            ]

---------------------------------------------------------------------------------------
-- | @lookupTyThings@ grabs all the @Name@s and associated @TyThing@ known to GHC 
--   for this module; we will use this to create our name-resolution environment 
--   (see `Bare.Resolve`)                                          
---------------------------------------------------------------------------------------
lookupTyThings :: HscEnv -> TypecheckedModule -> MGIModGuts -> Ghc [(Name, Maybe TyThing)] 
lookupTyThings hscEnv tcm mg =
  forM (mgNames mg) $ \n -> do 
    tt1 <-          lookupName                   n 
    tt2 <- liftIO $ Ghc.hscTcRcLookupName hscEnv n 
    tt3 <-          modInfoLookupName mi         n 
    tt4 <-          lookupGlobalName             n 
    return (n, Misc.firstMaybes [tt1, tt2, tt3, tt4])
    where 
      mi = tm_checked_module_info tcm

-- lookupName        :: GhcMonad m => Name -> m (Maybe TyThing) 
-- hscTcRcLookupName :: HscEnv -> Name -> IO (Maybe TyThing)
-- modInfoLookupName :: GhcMonad m => ModuleInfo -> Name -> m (Maybe TyThing)  
-- lookupGlobalName  :: GhcMonad m => Name -> m (Maybe TyThing)  

_dumpTypeEnv :: TypecheckedModule -> IO () 
_dumpTypeEnv tm = do 
  print "DUMP-TYPE-ENV"
  print (showpp <$> tcmTyThings tm)

tcmTyThings :: TypecheckedModule -> Maybe [Name] 
tcmTyThings 
  = id 
  -- typeEnvElts 
  -- . tcg_type_env . fst 
  -- . md_types . snd
  -- . tm_internals_
  . modInfoTopLevelScope
  . tm_checked_module_info


dumpRdrEnv :: HscEnv -> MGIModGuts -> IO () 
dumpRdrEnv _hscEnv modGuts = do 
  print "DUMP-RDR-ENV" 
  print (mgNames modGuts)
  -- print (hscNames hscEnv) 
  -- print (mgDeps modGuts) 
  where 
    _mgDeps   = Ghc.dep_mods . mgi_deps 
    _hscNames = fmap showPpr . Ghc.ic_tythings . Ghc.hsc_IC

mgNames :: MGIModGuts -> [Ghc.Name] 
mgNames  = fmap Ghc.gre_name . Ghc.globalRdrEnvElts .  mgi_rdr_env 

---------------------------------------------------------------------------------------
-- | @makeBareSpecs@ loads BareSpec for target and imported modules 
---------------------------------------------------------------------------------------
makeBareSpecs :: Config -> DepGraph -> SpecEnv -> ModSummary -> Ms.BareSpec 
              -> Ghc [(ModName, Ms.BareSpec)]
makeBareSpecs cfg depGraph specEnv modSum tgtSpec = do 
  let paths     = nub $ idirs cfg ++ importPaths (ms_hspp_opts modSum)
  _            <- liftIO $ whenLoud $ putStrLn $ "paths = " ++ show paths
  let reachable = reachableModules depGraph (ms_mod modSum)
  specSpecs    <- findAndParseSpecFiles cfg paths modSum reachable
  let homeSpecs = cachedBareSpecs specEnv reachable
  let impSpecs  = specSpecs ++ homeSpecs
  let tgtMod    = ModName Target (moduleName (ms_mod modSum))
  return        $ (tgtMod, tgtSpec) : impSpecs

{- 
  (spc, imps, _incs, exports) <- toGhcSpec cfg file coreBinds (impVs ++ defVs) letVs modName modGuts bareSpec logicMap impSpecs
  _                 <- liftIO $ whenLoud $ putStrLn $ "Module Imports: " ++ show imps
  -- FIXME hqualsFiles       <- moduleHquals modGuts paths file imps incs
            , giSpec      = spc
            -- , giHqFiles   = hqualsFiles
            -- , env       = hscEnv
            -- , imports   = imps
            -- , includes  = incs
            }
 
toGhcSpec :: GhcMonad m
          => Config
          -> FilePath
          -> [CoreBind]
          -> [Var]
          -> [Var]
          -> ModName
          -> MGIModGuts
          -> Ms.BareSpec
          -> Either Error LogicMap
          -> [(ModName, Ms.BareSpec)]
          -> m (GhcSpec, [String], [FilePath], NameSet)
toGhcSpec cfg file cbs vars letVs tgtMod mgi tgtSpec logicMap impSpecs = do
  -- REBARE: let tgtCxt    = IIModule $ getModName tgtMod
  -- REBARE: let impCxt    = map (IIDecl . qualImportDecl . getModName . fst) impSpecs
  -- REBARE: CONTEXT _            <- setContext ({- tracePpr "SET-CONTEXT" $ -} tgtCxt : impCxt)
  -- REBARE: CONTEXT hsc          <- getSession
  -- REBARE: let impNames  = map (getModString . fst) impSpecs
  -- let exports   = mgi_exports mgi
  let specs     = (tgtMod, tgtSpec) : impSpecs

  let imps      = sortNub $ impNames ++ [ symbolString x | (_, sp) <- specs, x <- Ms.imports sp ]
  ghcSpec      <- liftIO $ makeGhcSpec cfg file tgtMod cbs (mgi_tcs mgi) (mgi_cls_inst mgi) vars letVs exports hsc logicMap specs
  return (ghcSpec, imps, Ms.includes tgtSpec, exports)

  -- REBARE: let impNames  = map (getModString . fst) impSpecs
  let imps      = sortNub $ impNames ++ [ symbolString x | (_, sp) <- specs, x <- Ms.imports sp ]
 imps, Ms.includes tgtSpec

-} 
modSummaryHsFile :: ModSummary -> FilePath
modSummaryHsFile modSummary =
  fromMaybe
    (panic Nothing $
      "modSummaryHsFile: missing .hs file for " ++
      showPpr (ms_mod modSummary))
    (ml_hs_file $ ms_location modSummary)

cachedBareSpecs :: SpecEnv -> [Module] -> [(ModName, Ms.BareSpec)]
cachedBareSpecs specEnv mods = lookupBareSpec <$> mods
  where
    lookupBareSpec m         = fromMaybe (err m) (lookupModuleEnv specEnv m)
    err m                    = impossible Nothing ("lookupBareSpec: missing module " ++ showPpr m)

checkFilePragmas :: [Located String] -> Ghc ()
checkFilePragmas = Misc.applyNonNull (return ()) throw . mapMaybe err
  where
    err pragma
      | check (val pragma) = Just (ErrFilePragma $ fSrcSpan pragma :: Error)
      | otherwise          = Nothing
    check pragma           = any (`isPrefixOf` pragma) bad
    bad =
      [ "-i", "--idirs"
      , "-g", "--ghc-option"
      , "--c-files", "--cfiles"
      ]

--------------------------------------------------------------------------------
-- | Family instance information
--------------------------------------------------------------------------------
makeFamInstEnv :: HscEnv -> IO ([GHC.TyCon], [(Symbol, DataCon)])
makeFamInstEnv env = do
  famInsts <- getFamInstances env
  let fiTcs = [ tc            | FamInst { fi_flavor = DataFamilyInst tc } <- famInsts ]
  let fiDcs = [ (symbol d, d) | tc <- fiTcs, d <- tyConDataCons tc ]
  return (fiTcs, fiDcs)

getFamInstances :: HscEnv -> IO [FamInst]
getFamInstances env = do
  (_, Just (pkg_fie, home_fie)) <- runTcInteractive env tcGetFamInstEnvs
  return $ famInstEnvElts home_fie ++ famInstEnvElts pkg_fie
 
--------------------------------------------------------------------------------
-- | Extract Specifications from GHC -------------------------------------------
--------------------------------------------------------------------------------
extractSpecComments :: ParsedModule -> [(SourcePos, String)]
extractSpecComments = mapMaybe extractSpecComment
                    . concat
                    . M.elems
                    . snd
                    . pm_annotations


-- | 'extractSpecComment' pulls out the specification part from a full comment
--   string, i.e. if the string is of the form:
--   1. '{-@ S @-}' then it returns the substring 'S',
--   2. '{-@ ... -}' then it throws a malformed SPECIFICATION ERROR, and
--   3. Otherwise it is just treated as a plain comment so we return Nothing.
extractSpecComment :: GHC.Located AnnotationComment -> Maybe (SourcePos, String)

extractSpecComment (GHC.L sp (AnnBlockComment text))
  | isPrefixOf "{-@" text && isSuffixOf "@-}" text          -- valid   specification
  = Just (offsetPos, take (length text - 6) $ drop 3 text)
  | isPrefixOf "{-@" text                                   -- invalid specification
  = uError $ ErrParseAnn sp "A valid specification must have a closing '@-}'."
  where
    offsetPos = incSourceColumn (srcSpanSourcePos sp) 3
extractSpecComment _ = Nothing

extractSpecQuotes :: TypecheckedModule -> [BPspec]
extractSpecQuotes typechecked = mapMaybe extractSpecQuote anns
  where
    anns = map ann_value $
           filter (isOurModTarget . ann_target) $
           tcg_anns $ fst $ tm_internals_ typechecked

    isOurModTarget (ModuleTarget mod1) = mod1 == mod
    isOurModTarget _ = False

    mod = ms_mod $ pm_mod_summary $ tm_parsed_module typechecked

extractSpecQuote :: AnnPayload -> Maybe BPspec
extractSpecQuote payload = 
  case fromSerialized deserializeWithData payload of
    Nothing -> Nothing
    Just qt -> Just $ refreshSymbols $ liquidQuoteSpec qt

refreshSymbols :: Data a => a -> a
refreshSymbols = everywhere (mkT refreshSymbol)

refreshSymbol :: Symbol -> Symbol
refreshSymbol = symbol . symbolText

--------------------------------------------------------------------------------
-- | Finding & Parsing Files ---------------------------------------------------
--------------------------------------------------------------------------------

-- | Handle Spec Files ---------------------------------------------------------

findAndParseSpecFiles :: Config
                      -> [FilePath]
                      -> ModSummary
                      -> [Module]
                      -> Ghc [(ModName, Ms.BareSpec)]
findAndParseSpecFiles cfg paths modSummary reachable = do
  impSumms <- mapM getModSummary (moduleName <$> reachable)
  imps''   <- nub . concat <$> mapM modSummaryImports (modSummary : impSumms)
  imps'    <- filterM ((not <$>) . isHomeModule) imps''
  let imps  = m2s <$> imps'
  fs'      <- moduleFiles Spec paths imps
  -- liftIO    $ print ("moduleFiles-imps'': "  ++ show (m2s <$> imps''))
  -- liftIO    $ print ("moduleFiles-imps' : "  ++ show (m2s <$> imps'))
  -- liftIO    $ print ("moduleFiles-imps  : "  ++ show imps)
  -- liftIO    $ print ("moduleFiles-Paths : "  ++ show paths)
  -- liftIO    $ print ("moduleFiles-Specs : "  ++ show fs')
  patSpec  <- getPatSpec paths  $ totalityCheck cfg
  rlSpec   <- getRealSpec paths $ not (linear cfg)
  let fs    = patSpec ++ rlSpec ++ fs'
  transParseSpecs paths mempty mempty fs
  where
    m2s = moduleNameString . moduleName

getPatSpec :: [FilePath] -> Bool -> Ghc [FilePath]
getPatSpec paths totalitycheck
 | totalitycheck = moduleFiles Spec paths [patErrorName]
 | otherwise     = return []
 where
  patErrorName   = "PatErr"

getRealSpec :: [FilePath] -> Bool -> Ghc [FilePath]
getRealSpec paths freal
  | freal     = moduleFiles Spec paths [realSpecName]
  | otherwise = moduleFiles Spec paths [notRealSpecName]
  where
    realSpecName    = "Real"
    notRealSpecName = "NotReal"

transParseSpecs :: [FilePath]
                -> S.HashSet FilePath -> [(ModName, Ms.BareSpec)]
                -> [FilePath]
                -> Ghc [(ModName, Ms.BareSpec)]
transParseSpecs _ _ specs [] = return specs
transParseSpecs paths seenFiles specs newFiles = do
  newSpecs      <- liftIO $ mapM parseSpecFile newFiles
  impFiles      <- moduleFiles Spec paths $ specsImports newSpecs
  let seenFiles' = seenFiles `S.union` S.fromList newFiles
  let specs'     = specs ++ map (second noTerm) newSpecs
  let newFiles'  = filter (not . (`S.member` seenFiles')) impFiles
  transParseSpecs paths seenFiles' specs' newFiles'
  where
    specsImports ss = nub $ concatMap (map symbolString . Ms.imports . snd) ss

noTerm :: Ms.BareSpec -> Ms.BareSpec
noTerm spec = spec { Ms.decr = mempty, Ms.lazy = mempty, Ms.termexprs = mempty }

parseSpecFile :: FilePath -> IO (ModName, Ms.BareSpec)
parseSpecFile file = either throw return . specSpecificationP file =<< readFile file

-- Find Hquals Files -----------------------------------------------------------

_moduleHquals :: MGIModGuts
             -> [FilePath]
             -> FilePath
             -> [String]
             -> [FilePath]
             -> Ghc [FilePath]
_moduleHquals mgi paths target imps incs = do
  hqs   <- specIncludes Hquals paths incs
  hqs'  <- moduleFiles Hquals paths (mgi_namestring mgi : imps)
  hqs'' <- liftIO $ filterM doesFileExist [extFileName Hquals target]
  return $ Misc.sortNub $ hqs'' ++ hqs ++ hqs'

-- Find Files for Modules ------------------------------------------------------

moduleFiles :: Ext -> [FilePath] -> [String] -> Ghc [FilePath]
moduleFiles ext paths names = catMaybes <$> mapM (moduleFile ext paths) names

moduleFile :: Ext -> [FilePath] -> String -> Ghc (Maybe FilePath)
moduleFile ext paths name
  | ext `elem` [Hs, LHs] = do
    graph <- mgModSummaries <$> getModuleGraph
    case find (\m -> not (isBootSummary m) &&
                     name == moduleNameString (ms_mod_name m)) graph of
      Nothing -> liftIO $ getFileInDirs (extModuleName name ext) paths
      Just ms -> return $ normalise <$> ml_hs_file (ms_location ms)
  | otherwise = liftIO $ getFileInDirs (extModuleName name ext) paths

specIncludes :: Ext -> [FilePath] -> [FilePath] -> Ghc [FilePath]
specIncludes ext paths reqs = do
  let libFile = extFileNameR ext $ symbolString preludeName
  let incFiles = catMaybes $ reqFile ext <$> reqs
  liftIO $ forM (libFile : incFiles) $ \f -> do
    mfile <- getFileInDirs f paths
    case mfile of
      Just file -> return file
      Nothing   -> panic Nothing $ "cannot find " ++ f ++ " in " ++ show paths

reqFile :: Ext -> FilePath -> Maybe FilePath
reqFile ext s
  | isExtFile ext s = Just s
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Assemble Information for Spec Extraction ------------------------------------
--------------------------------------------------------------------------------

makeMGIModGuts :: DesugaredModule -> MGIModGuts
makeMGIModGuts desugared = miModGuts deriv modGuts
  where
    modGuts = coreModule desugared
    deriv   = Just $ instEnvElts $ mg_inst_env modGuts

makeLogicMap :: IO LogicMap
makeLogicMap = do
  lg    <- Misc.getCoreToLogicPath
  lspec <- readFile lg
  case parseSymbolToLogic lg lspec of 
    Left e   -> throw e 
    Right lm -> return (lm <> listLMap)

listLMap :: LogicMap -- TODO-REBARE: move to wiredIn
listLMap  = toLogicMap [ (dummyLoc nilName , []     , hNil)
                       , (dummyLoc consName, [x, xs], hCons (EVar <$> [x, xs])) ]
  where
    x     = "x"
    xs    = "xs"
    hNil  = mkEApp (dcSym Ghc.nilDataCon ) []
    hCons = mkEApp (dcSym Ghc.consDataCon)
    dcSym = dummyLoc . dropModuleUnique . symbol



--------------------------------------------------------------------------------
-- | Pretty Printing -----------------------------------------------------------
--------------------------------------------------------------------------------

instance PPrint GhcSpec where
  pprintTidy k spec = vcat
    [ "******* Target Variables ********************"
    , pprintTidy k $ gsTgtVars (gsVars spec)
    , "******* Type Signatures *********************"
    , pprintLongList k (gsTySigs (gsSig spec))
    , "******* Assumed Type Signatures *************"
    , pprintLongList k (gsAsmSigs (gsSig spec))
    , "******* DataCon Specifications (Measure) ****"
    , pprintLongList k (gsCtors (gsData spec))
    , "******* Measure Specifications **************"
    , pprintLongList k (gsMeas (gsData spec))       ]

instance PPrint GhcInfo where
  pprintTidy k info = vcat
    [ -- "*************** Imports *********************"
      -- , intersperse comma $ text <$> imports info
      -- , "*************** Includes ********************"
      -- , intersperse comma $ text <$> includes info
      "*************** Imported Variables **********"
    , pprDoc $ giImpVars (giSrc info)
    , "*************** Defined Variables ***********"
    , pprDoc $ giDefVars (giSrc info)
    , "*************** Specification ***************"
    , pprintTidy k $ giSpec info
    , "*************** Core Bindings ***************"
    , pprintCBs $ giCbs (giSrc info)                ]

-- RJ: the silly guards below are to silence the unused-var checker
pprintCBs :: [CoreBind] -> Doc
pprintCBs
  | otherwise = pprintCBsTidy
  | otherwise = pprintCBsVerbose
  where
    pprintCBsTidy    = pprDoc . tidyCBs
    pprintCBsVerbose = text . O.showSDocDebug unsafeGlobalDynFlags . O.ppr . tidyCBs

instance Show GhcInfo where
  show = showpp

instance PPrint TargetVars where
  pprintTidy _ AllVars   = text "All Variables"
  pprintTidy k (Only vs) = text "Only Variables: " <+> pprintTidy k vs

------------------------------------------------------------------------
-- Dealing with Errors ---------------------------------------------------
------------------------------------------------------------------------

instance Result SourceError where
  result = (`Crash` "Invalid Source") . sourceErrors ""
