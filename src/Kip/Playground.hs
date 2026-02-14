{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Kip.Playground
Description : Reusable one-shot execution API for playground invocations.

This module is the /single-run boundary/ for playground execution.

It intentionally provides a request/response API that can be called repeatedly
by different hosts:

- the classic command-style executable (`app/Playground.hs`)
- the reactor-style exported function (`app/PlaygroundReactor.hs`)

Each call to 'runPlaygroundRequest' allocates fresh parser/type/eval state.
That is the key invariant required by the playground runtime:
definitions created in one run must not leak into the next run unless they
come from files loaded again in that run.
-}
module Kip.Playground
  ( PlaygroundMode(..)
  , PlaygroundRequest(..)
  , PlaygroundOutput(..)
  , runPlaygroundRequest
  ) where

import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class
import Control.Monad.Reader (runReaderT)
import Data.List (nub)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashTable.IO as HT
import Paths_kip (getDataFileName)
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath ((</>), takeDirectory)

import Language.Foma
import Kip.AST
import Kip.Cache
import Kip.Codegen.JS (codegenProgram)
import Kip.Parser
import Kip.Render
import Kip.Runner
import Kip.TypeCheck

-- | Supported execution modes for a single playground request.
data PlaygroundMode
  = PlaygroundExec
  | PlaygroundBuild
  -- | Currently supports @\"js\"@.
  | PlaygroundCodegen Text
  deriving (Eq, Show)

{- |
Input for one isolated playground invocation.

The host is expected to supply @prFiles@ per run. In the WASI playground this
is usually a virtual file path like @/main.kip@.
-}
data PlaygroundRequest =
  PlaygroundRequest
    { -- | Requested run behavior.
      prMode :: PlaygroundMode
      -- | Input file(s) for the invocation.
    , prFiles :: [FilePath]
      -- | Additional module search directories.
    , prIncludeDirs :: [FilePath]
      -- | Diagnostic language.
    , prLang :: Lang
      -- | Whether to skip implicit prelude loading.
    , prNoPrelude :: Bool
    }

-- | Structured output of a single playground execution.
data PlaygroundOutput
  -- | No text payload (e.g. normal @--exec@ / @--build@ flow).
  = PlaygroundNoOutput
  -- | Text payload (currently for code generation mode).
  | PlaygroundTextOutput Text
  deriving (Eq, Show)

-- | Bootstrap-time playground failures before full runtime init.
data PlaygroundBootstrapError
  = PlaygroundTrmorphMissing
  | PlaygroundLibMissing

-- | Render bootstrap failures for the selected language.
renderPlaygroundBootstrapError :: Lang -> PlaygroundBootstrapError -> Text
renderPlaygroundBootstrapError lang err =
  case (lang, err) of
    (LangTr, PlaygroundTrmorphMissing) -> "vendor/trmorph.fst bulunamadı."
    (LangEn, PlaygroundTrmorphMissing) -> "vendor/trmorph.fst not found."
    (LangTr, PlaygroundLibMissing) -> "lib/temel.kip bulunamadı."
    (LangEn, PlaygroundLibMissing) -> "lib/temel.kip not found."

{- |
Execute one request with fresh compiler and evaluator state.

Execution steps:

1. Resolve data files (@trmorph@ and stdlib root).
2. Build fresh morphology/render caches for this invocation.
3. Load prelude state once for this invocation.
4. Execute requested mode.

This function does /not/ persist user definitions across invocations by design.
-}
runPlaygroundRequest :: PlaygroundRequest -> IO PlaygroundOutput
runPlaygroundRequest req = do
  trmorphPath <- locateTrmorph (prLang req)
  libDir <- locateLibDir (prLang req)
  fsm <- fsmReadBinaryFile trmorphPath
  upsCache <- HT.new
  downsCache <- HT.new
  let renderCache = mkRenderCache upsCache downsCache
      moduleDirs = nub (libDir : prIncludeDirs req)
      renderCtx = RenderCtx (prLang req) renderCache fsm upsCache downsCache
  (preludePst, preludeTC, preludeEval, preludeLoaded) <-
    runReaderT (loadPreludeState (prNoPrelude req) moduleDirs renderCache fsm upsCache downsCache) renderCtx
  case prMode req of
    PlaygroundExec -> do
      when (null (prFiles req)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFile) renderCtx
      _ <- runReaderT (runFiles False False False preludePst preludeTC preludeEval moduleDirs preludeLoaded (prFiles req)) renderCtx
      return PlaygroundNoOutput
    PlaygroundBuild -> do
      when (null (prFiles req)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFileOrDir) renderCtx
      buildTargets <- resolveBuildTargets (prFiles req)
      let extraDirs = nub (map takeDirectory buildTargets)
          buildModuleDirs = nub (moduleDirs ++ extraDirs)
      (preludeBuildPst, preludeBuildTC, preludeBuildEval, preludeBuildLoaded) <-
        runReaderT (loadPreludeState (prNoPrelude req) buildModuleDirs renderCache fsm upsCache downsCache) renderCtx
      _ <- runReaderT (runFiles False False True preludeBuildPst preludeBuildTC preludeBuildEval buildModuleDirs preludeBuildLoaded buildTargets) renderCtx
      return PlaygroundNoOutput
    PlaygroundCodegen target ->
      case target of
        "js" -> do
          js <- runReaderT (emitJsFilesWithDeps moduleDirs preludePst preludeTC Set.empty (prFiles req)) renderCtx
          return (PlaygroundTextOutput js)
        _ ->
          die . T.unpack =<< runReaderT (renderMsg (MsgUnknownCodegenTarget target)) renderCtx

-- | Locate @trmorph.fst@ path with data-dir fallback behavior.
locateTrmorph :: Lang -> IO FilePath
locateTrmorph lang = do
  path <- locateDataFile "vendor/trmorph.fst"
  exists <- doesFileExist path
  if exists
    then return path
    else die . T.unpack $ renderPlaygroundBootstrapError lang PlaygroundTrmorphMissing

-- | Locate stdlib root (@lib/temel.kip@) with data-dir fallback behavior.
locateLibDir :: Lang -> IO FilePath
locateLibDir lang = do
  path <- locateDataFile "lib/temel.kip"
  exists <- doesFileExist path
  if exists
    then return (takeDirectory path)
    else die . T.unpack $ renderPlaygroundBootstrapError lang PlaygroundLibMissing

{- |
Resolve a packaged data file path from runtime candidates.

Lookup order:

1. @KIP_DATADIR/<rel>@
2. Cabal data-files path via 'getDataFileName'
3. Relative path in current working directory
-}
locateDataFile :: FilePath -> IO FilePath
locateDataFile rel = do
  mEnv <- lookupEnv "KIP_DATADIR"
  cabalPath <- getDataFileName rel
  let envPaths = maybe [] (\base -> [base </> rel]) mEnv
      candidates = envPaths ++ [cabalPath, rel]
  found <- filterM doesFileExist candidates
  case found of
    p:_ -> return p
    [] -> return cabalPath

{- |
Generate one JS program text from entry files and transitive dependencies.

This intentionally starts from @giriş@ to mirror the runtime prelude that
normal execution uses.
-}
emitJsFilesWithDeps :: [FilePath] -> ParserState -> TCState -> Set FilePath -> [FilePath] -> RenderM Text
emitJsFilesWithDeps moduleDirs basePst baseTC _preludeLoaded files = do
  preludePath <- resolveModulePath moduleDirs ([], T.pack "giriş")
  (preludeStmts, pst', tcSt', loaded') <- emitJsFileWithDeps moduleDirs ([], basePst, baseTC, Set.empty) preludePath
  (stmts, _, _, _) <- foldM' (emitJsFileWithDeps moduleDirs) (preludeStmts, pst', tcSt', loaded') files
  return (codegenProgram stmts)

{- |
Parse/typecheck one file and recursively include dependencies for codegen.

Unlike runtime execution, this path accumulates typed statements and then emits
a single JS program.
-}
emitJsFileWithDeps :: [FilePath] -> ([Stmt Ann], ParserState, TCState, Set FilePath) -> FilePath -> RenderM ([Stmt Ann], ParserState, TCState, Set FilePath)
emitJsFileWithDeps moduleDirs (acc, pst, tcSt, loaded) path = do
  exists <- liftIO (doesFileExist path)
  unless exists $ do
    msg <- renderMsg (MsgFileNotFound path)
    liftIO (die (T.unpack msg))
  absPath <- liftIO (canonicalizePath path)
  if Set.member absPath loaded
    then return (acc, pst, tcSt, loaded)
    else do
      input <- liftIO (TIO.readFile path)
      liftIO (parseFromFile pst input) >>= \case
        Left err -> do
          msg <- renderMsg (MsgParseError err)
          liftIO (die (T.unpack msg))
        Right (fileStmts, pst') -> do
          let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
              tyMods = parserTyMods pst'
              loaded' = Set.insert absPath loaded
          let loadStmts = [name | Load name <- fileStmts]
          (depStmts, pst'', tcSt', loaded'') <- foldM' (emitJsLoad moduleDirs paramTyCons tyMods) ([], pst', tcSt, loaded') loadStmts
          liftIO (runTCM (registerForwardDecls fileStmts) tcSt') >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (_, tcStWithDecls) ->
              liftIO (runTCM (mapM tcStmt fileStmts) tcStWithDecls) >>= \case
                Left tcErr -> do
                  msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons tyMods)
                  liftIO (die (T.unpack msg))
                Right (typedStmts, tcSt'') ->
                  let filteredStmts = filter (not . isLoadStmt) typedStmts
                  in return (acc ++ depStmts ++ filteredStmts, pst'', tcSt'', loaded'')

-- | Check whether a statement is @Load@.
isLoadStmt :: Stmt Ann -> Bool
isLoadStmt (Load _) = True
isLoadStmt _ = False

-- | Load one dependency module for JS code generation.
emitJsLoad :: [FilePath] -> [Identifier] -> [(Identifier, [Identifier])] -> ([Stmt Ann], ParserState, TCState, Set FilePath) -> Identifier -> RenderM ([Stmt Ann], ParserState, TCState, Set FilePath)
emitJsLoad moduleDirs _paramTyCons _tyMods (acc, pst, tcSt, loaded) name = do
  path <- resolveModulePath moduleDirs name
  emitJsFileWithDeps moduleDirs (acc, pst, tcSt, loaded) path
