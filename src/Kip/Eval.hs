{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluator for Kip expressions and statements.
module Kip.Eval where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Kip.AST
import qualified Kip.Primitive as Prim

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Exception (SomeException, try)
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import System.FilePath (takeFileName, takeDirectory, (</>), isRelative)
import System.Random (randomRIO)
import Data.Word (Word32)
import Control.Monad (guard, zipWithM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (find, foldl', intersect, nub)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.MultiMap as MultiMap
import Data.Fixed (mod')

-- | Evaluator state: runtime bindings plus render function.
data EvalState =
  MkEvalState
    { evalVals :: Map.Map Identifier (Exp Ann) -- ^ Value bindings.
    , evalFuncs :: MultiMap.MultiMap Identifier ([Arg Ann], [Clause Ann]) -- ^ Function clauses (can be overloaded).
    , evalPrimFuncs :: MultiMap.MultiMap Identifier ([Arg Ann], [Exp Ann] -> EvalM (Exp Ann)) -- ^ Primitive implementations (can be overloaded).
    , evalSelectors :: MultiMap.MultiMap Identifier Int -- ^ Record selector indices.
    , evalCtors :: Map.Map Identifier ([Ty Ann], Ty Ann) -- ^ Constructor signatures.
    , evalTyCons :: Map.Map Identifier Int -- ^ Type constructor arities.
    , evalCurrentFile :: Maybe FilePath -- ^ Current file path for relative I/O.
    , evalRender :: EvalState -> Exp Ann -> IO String -- ^ Render function for values.
    , evalRandState :: Maybe Word32 -- ^ Optional deterministic random state.
    }

-- | Empty evaluator state with a simple pretty-printer.
emptyEvalState :: EvalState -- ^ Default evaluator state.
emptyEvalState = MkEvalState Map.empty MultiMap.empty MultiMap.empty MultiMap.empty Map.empty Map.empty Nothing (\_ e -> return (prettyExp e)) Nothing

-- | Evaluation errors (currently minimal).
data EvalError =
   Unknown
   | UnboundVariable Identifier
   | NoMatchingFunction Identifier
   | NoMatchingClause
   deriving (Show, Eq, Generic, Binary)
-- | Evaluator monad stack.
type EvalM = StateT EvalState (ExceptT EvalError IO)

-- | Evaluate an expression in the current evaluator state.
evalExp :: Exp Ann -- ^ Expression to evaluate.
        -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExp = evalExpWith []

{- | Check if a variable can be resolved in an application context.

Variables in Kip can refer to different kinds of bindings stored in separate namespaces:
- Values/definitions (evalVals)
- Functions (evalFuncs)
- Primitive functions (evalPrimFuncs)
- Type constructors (evalTyCons)
- Data constructors (evalCtors)
- Record selectors (evalSelectors)

When evaluating a standalone Var, we only look in evalVals. If not found there,
we check if it exists in any other namespace - if so, it's a function/constructor
reference that will be resolved when used in an App. Otherwise, it's truly undefined.
-}
isResolvableInAppContext :: [(Identifier, Case)] -- ^ Variable candidates.
                         -> EvalState -- ^ Current evaluation state.
                         -> Bool -- ^ True if resolvable in App context.
isResolvableInAppContext varCandidates st =
  let fnCandidates = map fst varCandidates
      fnExists = any (\n -> not (null (MultiMap.lookup n (evalFuncs st)))) fnCandidates
      primExists = any (\n -> not (null (MultiMap.lookup n (evalPrimFuncs st)))) fnCandidates
      selectorExists = any (\n -> not (null (MultiMap.lookup n (evalSelectors st)))) fnCandidates
      -- Type constructors use full candidate list (not just first elements)
      tyExists = any (\(ident, _) -> Map.member ident (evalTyCons st)) varCandidates
      ctorExists = any (\n -> Map.member n (evalCtors st)) fnCandidates
      randomExists = isRandomCandidate varCandidates
  in fnExists || primExists || selectorExists || tyExists || ctorExists || randomExists

-- | A single evaluation step for trampolining tail calls.
-- | Done: final value.
-- | Continue: evaluate a new (env, exp) without growing the Haskell call stack.
data EvalStep
  = Done (Exp Ann)
  | Continue (HM.HashMap Identifier (Exp Ann)) (Exp Ann)

-- | Evaluate an expression with a local environment.
-- |
-- | This is a trampoline: tail-position transitions return 'Continue' so the
-- | evaluator can loop without consuming Haskell stack frames. Non-tail work
-- | still evaluates recursively, but tail calls become iterative.
evalExpWith :: [(Identifier, Exp Ann)] -- ^ Local environment bindings.
            -> Exp Ann -- ^ Expression to evaluate.
            -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExpWith bindings = evalExpLoop (HM.fromList bindings) 

-- | Main trampoline loop.
-- |
-- | The loop is strict in the next step: it only recurses in Haskell when the
-- | evaluation cannot be in tail position (e.g., computing subexpressions).
evalExpLoop :: HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
            -> Exp Ann -- ^ Expression to evaluate.
            -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExpLoop localEnv e = do
  evalStep localEnv e >>= \case
    Done v -> return v
    Continue env' e' -> evalExpLoop env' e'

-- | One evaluation step.
-- |
-- | IMPORTANT: Only tail-position transitions should return 'Continue':
-- | - Sequence second position
-- | - Match clause body
-- | - Let/Ascribe bodies
-- | - Function bodies
-- | - Variable evaluation that expands to another expression
-- |
-- | Everything else returns a 'Done' value (possibly after non-tail recursion).
-- |
-- | Optimization notes:
-- | 1) Overload resolution is the hottest path for most programs. It performs
-- |    'inferType' on every argument to select a definition. That is correct,
-- |    but wasteful when no candidate functions/primops/selectors exist for the
-- |    name. In that case, the semantics are: leave the application untouched
-- |    (constructor application or unevaluated call). We therefore short-circuit
-- |    before calling 'pickPrimByTypes'/'pickFunctionByTypes'. This avoids all
-- |    type inference work for obviously non-callable identifiers.
-- |
-- |    Safety: We only skip resolution when there are no candidates in any of
-- |    the relevant namespaces (functions, primops, selectors, random). If a
-- |    candidate exists, we fall back to the full resolution logic.
-- |
-- | 2) Selector and random lookups are *name-based* and do not depend on
-- |    argument types. If there are no function/primitive candidates, we can
-- |    resolve selectors (single-arg) and the random primitive without calling
-- |    'inferType'. This removes a full round of type inference in a common
-- |    case for record access and random number generation.
-- |
-- | 3) We keep 'fn' and 'args' evaluation strict (non-tail) so evaluation order
-- |    and effects are unchanged. The only change is *what happens after* the
-- |    arguments are evaluated: we build a 'Done' or 'Continue' step rather than
-- |    performing another recursive call in Haskell.
-- |
-- | 4) The trampoline structure is intentionally minimal: we only introduce
-- |    iteration for tail positions, leaving non-tail recursion untouched. This
-- |    preserves current behavior while eliminating stack growth for tail calls.
evalStep :: HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
         -> Exp Ann -- ^ Expression to evaluate.
         -> EvalM EvalStep -- ^ Trampoline step.
evalStep localEnv e =
  case e of
    Var {annExp, varName, varCandidates} ->
      case lookupByCandidatesHM localEnv varCandidates of
        Just v -> return (Done v)
        Nothing ->
          case lookupBySuffixHM localEnv varName of
            Just v -> return (Done v)
            Nothing -> do
              st@MkEvalState{evalVals} <- get
              case lookupByCandidates evalVals varCandidates of
                Nothing ->
                  -- Not a value binding. Check if it's a function/constructor/etc.
                  -- that will be resolved when applied in an App context.
                  if isResolvableInAppContext varCandidates st
                    then return (Done (Var annExp varName varCandidates))
                    else throwError (UnboundVariable varName)
                Just v ->
                  -- Tail-position indirection: keep evaluating the bound value.
                  return (Continue localEnv v)
    App {annExp = annApp, fn, args} -> do
      -- Non-tail: we must compute function and arguments before applying.
      fn' <- evalExpLoop localEnv fn
      args' <- mapM (evalExpLoop localEnv) args
      let (fnResolved, preAppliedArgs) = flattenApplied fn'
          allArgs = preAppliedArgs ++ args'
      case fnResolved of
        Var {varName, varCandidates} -> do
          -- Pull state once for all resolution steps.
          MkEvalState{evalFuncs, evalPrimFuncs, evalSelectors, evalTyCons} <- get
          case allArgs of
            [arg] | any (\(ident, _) -> Map.member ident evalTyCons) varCandidates ->
              return (Done (applyTypeCase (annCase (annExp fnResolved)) arg))
            _ -> do
              let fnCandidates = map fst varCandidates
                  matches = [(n, def) | n <- fnCandidates, def <- MultiMap.lookup n evalFuncs]
                  primMatches = [(n, def) | n <- fnCandidates, def <- MultiMap.lookup n evalPrimFuncs]
                  selectorMatches = [idx | n <- fnCandidates, idx <- MultiMap.lookup n evalSelectors]
              -- Optimization: if there are no function/primitive candidates, we can
              -- decide selector/random/constructor outcomes without type inference.
              if null matches && null primMatches
                then
                  case (selectorMatches, allArgs) of
                    -- Fast-path selectors when the only possible resolution is a selector.
                    (idx:_, [arg]) ->
                      Done <$> applySelector idx arg (App annApp fnResolved allArgs)
                    _ ->
                      if isRandomCandidate varCandidates
                        -- Fast-path random primitive: no type inference needed.
                        then Done <$> primIntRandom allArgs
                        else return (Done (App annApp fnResolved allArgs)) -- Constructor application or unevaluated call.
                else do
                  let partialCall = not (null preAppliedArgs)
                      callArgs = reorderSectionArgs preAppliedArgs allArgs
                      pickPrim = if partialCall then pickPrimByTypesPartial else pickPrimByTypes
                      pickFn = if partialCall then pickFunctionByTypesPartial else pickFunctionByTypes
                  pickPrim primMatches callArgs >>= \case
                    Just (primImpl, primArgs) -> Done <$> primImpl primArgs
                    Nothing ->
                      pickFn matches callArgs >>= \case
                        Just (def, fnArgs) -> applyFunctionStep fnResolved localEnv def fnArgs
                        Nothing ->
                          case (selectorMatches, allArgs) of
                            (idx:_, [arg]) ->
                              Done <$> applySelector idx arg (App annApp fnResolved allArgs)
                            _ ->
                              if isRandomCandidate varCandidates
                                then Done <$> primIntRandom allArgs
                                else return (Done (App annApp fnResolved allArgs)) -- Constructor application or unevaluated call
        _ -> return (Done (App annApp fnResolved allArgs))
    StrLit {annExp, lit} ->
      return (Done (StrLit annExp lit))
    IntLit {annExp, intVal} ->
      return (Done (IntLit annExp intVal))
    FloatLit {annExp, floatVal} ->
      return (Done (FloatLit annExp floatVal))
    Bind {annExp, bindName, bindNameAnn, bindExp} -> do
      -- Non-tail: evaluate the binding expression, but the bind itself is a value.
      v <- evalExpLoop localEnv bindExp
      return (Done (Bind annExp bindName bindNameAnn v))
    Seq {annExp, first, second} -> do
      case first of
        Bind {bindName, bindNameAnn, bindExp} -> do
          -- Tail position: continue with the extended environment and second.
          v <- evalExpLoop localEnv bindExp
          return (Continue (HM.insert bindName v localEnv) second)
        _ -> do
          -- Evaluate the first expression, then tail-continue into second.
          _ <- evalExpLoop localEnv first
          return (Continue localEnv second)
    Match {annExp, scrutinee, clauses} -> do
      -- Non-tail: we need the scrutinee value to select the clause.
      scrutinee' <- evalExpLoop localEnv scrutinee
      case findClause scrutinee' clauses of
        Nothing -> throwError NoMatchingClause
        Just (Clause _ body, patBindings) -> do
          let env = HM.fromList patBindings `HM.union` localEnv
          -- Tail position: continue with the clause body.
          return (Continue env body)
    Let {annExp, varName, body} ->
      -- Tail position: the body is the result of the let.
      return (Continue localEnv body)
    Ascribe {ascExp} ->
      -- Tail position: ascriptions do not affect evaluation.
      return (Continue localEnv ascExp)
  where
    -- | Find the first matching clause for a scrutinee.
    findClause :: Exp Ann -- ^ Scrutinee expression.
               -> [Clause Ann] -- ^ Clauses to search.
               -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
    findClause scrut = go
      where
        -- | Walk clauses left-to-right until one matches.
        go :: [Clause Ann] -- ^ Remaining clauses.
           -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
        go [] = Nothing
        go (c@(Clause pat _):rest) =
          case matchPat pat (Just scrut) of
            Just binds -> Just (c, binds)
            Nothing -> go rest

-- | Apply a function definition to evaluated arguments.
-- |
-- | This is a tail-position producer: when a clause matches, we return a
-- | 'Continue' step so the trampoline can evaluate the body without growing
-- | the Haskell stack.
applyFunctionStep :: Exp Ann -- ^ Function expression.
                  -> HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
                  -> ([Arg Ann], [Clause Ann]) -- ^ Function signature and clauses.
                  -> [Exp Ann] -- ^ Evaluated arguments.
                  -> EvalM EvalStep -- ^ Trampoline step.
applyFunctionStep fn localEnv (args, clauses) values = do
  let argNames = map argIdent args
      argBindings = zip argNames values
  case findClause values clauses of
    Nothing -> return (Done (App (annExp fn) fn values))
    Just (Clause pat body, patBindings) -> do
      let env = HM.fromList patBindings `HM.union` HM.fromList argBindings `HM.union` localEnv
      return (Continue env body)
  where
    -- | Find the first matching clause for argument values.
    findClause :: [Exp Ann] -- ^ Argument values.
               -> [Clause Ann] -- ^ Clauses to search.
               -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
    findClause vs = go
      where
        -- | Walk clauses left-to-right until one matches.
        go :: [Clause Ann] -- ^ Remaining clauses.
           -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
        go [] = Nothing
        go (c@(Clause pat _):rest) =
          case matchPat pat (scrutinee vs) of
            Just binds -> Just (c, binds)
            Nothing -> go rest
    -- | Use the first argument as the match scrutinee.
    scrutinee :: [Exp Ann] -- ^ Argument values.
              -> Maybe (Exp Ann) -- ^ Scrutinee expression.
    scrutinee vs =
      case vs of
        [] -> Nothing
        (v:_) -> Just v

-- | Match a pattern against a possible expression.
matchPat :: Pat Ann -- ^ Pattern to match.
         -> Maybe (Exp Ann) -- ^ Scrutinee expression.
         -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchPat pat mval =
  case pat of
    PWildcard _ -> Just []
    PVar n _ ->
      case mval of
        Nothing -> Nothing
        Just v -> Just [(n, v)]
    PCtor (ctor, _) pats ->
      case mval of
        Nothing -> Nothing
        Just v -> matchCtor ctor pats v
    PIntLit n _ ->
      case mval of
        Just (IntLit _ n') | n == n' -> Just []
        _ -> Nothing
    PFloatLit n _ ->
      case mval of
        Just (FloatLit _ n') | n == n' -> Just []
        _ -> Nothing
    PStrLit s _ ->
      case mval of
        Just (StrLit _ s') | s == s' -> Just []
        _ -> Nothing
    PListLit pats ->
      case mval of
        Nothing -> Nothing
        Just v -> matchList pats v

-- | Match a constructor pattern against an expression.
matchCtor :: Identifier -- ^ Constructor identifier.
          -> [Pat Ann] -- ^ Sub-patterns.
          -> Exp Ann -- ^ Scrutinee expression.
          -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchCtor ctor pats v =
  case v of
    Var {varCandidates, varName} ->
      if ctorMatches ctor (Just varName) (map fst varCandidates)
        -- A bare constructor can be used as a unary "tag check" or as a
        -- one-argument pattern application; anything beyond that cannot
        -- match a Var without args.
        then case pats of
          [] -> Just []
          [p] -> matchPat p (Just v)
          _ -> Nothing
        else Nothing
    App {fn, args} ->
      case fn of
        Var {varCandidates, varName} | ctorMatches ctor (Just varName) (map fst varCandidates) ->
          -- Constructor patterns are right-aligned with actual args so that
          -- nested patterns (especially for list literals) match the tail.
          if length pats <= length args
            then do
              -- Drop leading args when the pattern list is shorter.
              let args' = drop (length args - length pats) args
              -- Recursively match each sub-pattern
              bindings <- zipWithM matchPat pats (map Just args')
              return (concat bindings)
            else
              Nothing
        _ -> Nothing
    _ -> Nothing
  where
    -- | Check constructor identity with possessive fallback.
    -- Prefers exact candidate matches before heuristic normalization.
    ctorMatches :: Identifier -- ^ Constructor name.
                -> Maybe Identifier -- ^ Optional variable name.
                -> [Identifier] -- ^ Candidate identifiers.
                -> Bool -- ^ True when constructors match.
    ctorMatches name mVarName candidates =
      let candidates' = candidates ++ maybe [] normalizeIdent mVarName
      in name `elem` candidates'
         || any (identMatchesPoss name) candidates'

    -- | Normalize identifiers by removing copula suffixes.
    normalizeIdent :: Identifier -- ^ Identifier to normalize.
                   -> [Identifier] -- ^ Normalized identifiers.
    normalizeIdent ident@(mods, word) =
      case stripCopulaSuffix word of
        Just stripped -> [(mods, stripped)]
        Nothing -> [ident]

    -- | Compare identifiers, allowing possessive/root normalization.
    identMatchesPoss :: Identifier -- ^ Left identifier.
                     -> Identifier -- ^ Right identifier.
                     -> Bool -- ^ True when identifiers match loosely.
    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2) &&
      not (null (roots x1 `intersect` roots x2))

    -- | Produce candidate roots for heuristic matching.
    roots :: Text -- ^ Surface word.
          -> [Text] -- ^ Candidate roots.
    roots txt =
      nub
        (catMaybes
          [ Just txt
          , dropTrailingVowel txt >>= dropTrailingSoftG
          ])

    -- | Drop a trailing Turkish vowel for heuristic matching.
    dropTrailingVowel :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Word without trailing vowel.
    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing

    -- | Replace trailing soft g with k for heuristic matching.
    dropTrailingSoftG :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Word with trailing soft g normalized.
    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing

    -- | Strip copula suffixes from a surface word.
    stripCopulaSuffix :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Stripped word.
    stripCopulaSuffix txt =
      let lowerTxt = T.toLower txt
          suffixes = ["dir","dır","dur","dür","tir","tır","tur","tür"]
          match = find (`T.isSuffixOf` lowerTxt) suffixes
      in case match of
           Nothing -> Nothing
           Just suff ->
             let len = T.length suff
             in if T.length txt > len
                  then Just (T.take (T.length txt - len) txt)
                  else Nothing

-- | Try resolving an ip-converb function to its base name when prim lookup fails.
-- | Recognize the random primitive in either split or dashed identifier form.

-- | Match a list pattern against an expression.
matchList :: [Pat Ann] -- ^ Element patterns.
          -> Exp Ann -- ^ Scrutinee expression.
          -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchList [] (Var _ ([], name) _)
  | name == T.pack "boş" = Just []
matchList (p:ps) (App _ (Var _ ([], name) _) [elem, rest])
  | name == T.pack "eki" = do
      elemBinds <- matchPat p (Just elem)
      restBinds <- matchList ps rest
      return (elemBinds ++ restBinds)
matchList _ _ = Nothing
isRandomCandidate :: [(Identifier, Case)] -> Bool
isRandomCandidate =
  any (\(ident, _) -> ident == (["sayı"], "çek") || ident == ([], "sayı-çek"))

-- | Apply a record selector or fall back when out of range.
applySelector :: Int -- ^ Selector index.
              -> Exp Ann -- ^ Argument expression.
              -> Exp Ann -- ^ Fallback expression.
              -> EvalM (Exp Ann) -- ^ Selected expression.
applySelector idx arg fallback =
  case arg of
    App {args} ->
      if idx < length args
        then return (args !! idx)
        else return fallback
    _ -> return fallback

-- | Reorder arguments for one-argument section applications.
-- Instrumental fixed arguments are left sections; other fixed arguments are
-- treated as right sections.
reorderSectionArgs :: [Exp Ann] -> [Exp Ann] -> [Exp Ann]
reorderSectionArgs preApplied args =
  case (preApplied, args) of
    ([fixed], [x, y])
      | annCase (annExp fixed) /= Ins -> [y, x]
    _ -> args

-- | Choose a function definition based on inferred argument types.
pickFunctionByTypes :: [(Identifier, ([Arg Ann], [Clause Ann]))] -- ^ Candidate function definitions.
                    -> [Exp Ann] -- ^ Evaluated arguments.
                    -> EvalM (Maybe (([Arg Ann], [Clause Ann]), [Exp Ann])) -- ^ Selected function and args.
pickFunctionByTypes defs args = do
  MkEvalState{evalTyCons} <- get
  argTys <- mapM inferType args
  let matches =
        [ (def, args)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
        , length tys == length args
        , and (zipWith (typeMatchesAllowUnknown (Map.toList evalTyCons)) argTys tys)
        ]
      fallback =
        [ (def, args)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
        , length tys == length args
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> case fallback of
      d:_ -> Just d
      [] -> Nothing

-- | Choose a primitive implementation based on inferred argument types.
pickPrimByTypes :: [(Identifier, ([Arg Ann], [Exp Ann] -> EvalM (Exp Ann)))] -- ^ Primitive candidates.
                -> [Exp Ann] -- ^ Evaluated arguments.
                -> EvalM (Maybe ([Exp Ann] -> EvalM (Exp Ann), [Exp Ann])) -- ^ Selected primitive and args.
pickPrimByTypes defs args = do
  MkEvalState{evalTyCons} <- get
  argTys <- mapM inferType args
  let matches =
        [ (impl, args)
        | (_, (args', impl)) <- defs
        , let tys = map snd args'
        , length tys == length args
        , and (zipWith (typeMatchesAllowUnknown (Map.toList evalTyCons)) argTys tys)
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> Nothing

-- | Choose a function definition for calls that originated from partial application.
-- Reorders arguments by expected case, allowing nominative values to fill gaps.
pickFunctionByTypesPartial :: [(Identifier, ([Arg Ann], [Clause Ann]))]
                           -> [Exp Ann]
                           -> EvalM (Maybe (([Arg Ann], [Clause Ann]), [Exp Ann]))
pickFunctionByTypesPartial defs args = do
  MkEvalState{evalTyCons} <- get
  argTys <- mapM inferType args
  let argCases = map (annCase . annExp) args
      matches =
        [ (def, argsForSig)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
              expCases = map (annCase . annTy . snd) args'
        , length tys == length args
        , Just argsForSig <- [reorderByCasesForEval expCases argCases args]
        , Just argTysForSig <- [reorderByCasesForEval expCases argCases argTys]
        , and (zipWith (typeMatchesAllowUnknown (Map.toList evalTyCons)) argTysForSig tys)
        ]
      fallback =
        [ (def, argsForSig)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
              expCases = map (annCase . annTy . snd) args'
        , length tys == length args
        , Just argsForSig <- [reorderByCasesForEval expCases argCases args]
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> case fallback of
      d:_ -> Just d
      [] -> Nothing

-- | Choose a primitive implementation for calls that originated from partial application.
pickPrimByTypesPartial :: [(Identifier, ([Arg Ann], [Exp Ann] -> EvalM (Exp Ann)))]
                       -> [Exp Ann]
                       -> EvalM (Maybe ([Exp Ann] -> EvalM (Exp Ann), [Exp Ann]))
pickPrimByTypesPartial defs args = do
  MkEvalState{evalTyCons} <- get
  argTys <- mapM inferType args
  let argCases = map (annCase . annExp) args
      matches =
        [ (impl, argsForSig)
        | (_, (args', impl)) <- defs
        , let tys = map snd args'
              expCases = map (annCase . annTy . snd) args'
        , length tys == length args
        , Just argsForSig <- [reorderByCasesForEval expCases argCases args]
        , Just argTysForSig <- [reorderByCasesForEval expCases argCases argTys]
        , and (zipWith (typeMatchesAllowUnknownPartial (Map.toList evalTyCons)) argTysForSig tys)
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> Nothing

-- | Type comparison used in partial-application primitive dispatch.
-- Unknown argument types are accepted to avoid dropping valid sections.
typeMatchesAllowUnknownPartial :: [(Identifier, Int)]
                               -> Maybe (Ty Ann)
                               -> Ty Ann
                               -> Bool
typeMatchesAllowUnknownPartial tyCons mTy ty =
  case mTy of
    Nothing -> True
    Just _ -> typeMatchesAllowUnknown tyCons mTy ty

-- | Reorder values for evaluator call matching.
-- Uses shared nominative fallback and additionally allows instrumental slots
-- to consume genitive values when no exact or nominative match exists.
reorderByCasesForEval :: [Case] -> [Case] -> [a] -> Maybe [a]
reorderByCasesForEval expected actual xs =
  case reorderByCasesNomFallback expected actual xs of
    Just reordered -> Just reordered
    Nothing -> reorderInsFromGen expected actual xs
  where
    reorderInsFromGen expCases actCases vals
      | length expCases /= length actCases || length actCases /= length vals = Nothing
      | otherwise = map snd <$> go (zip actCases vals) expCases
    go rems [] = Just []
    go rems (c:cs) =
      case pick c rems of
        Nothing -> Nothing
        Just (v, rems') -> (v :) <$> go rems' cs
    pick c rems =
      case break (\(ac, _) -> ac == c) rems of
        (before, m:after) -> Just (m, before ++ after)
        (_, []) ->
          if c == Nom
            then Nothing
            else case break (\(ac, _) -> ac == Nom) rems of
              (before, m:after) -> Just (m, before ++ after)
              (_, []) ->
                if c == Ins
                  then case break (\(ac, _) -> ac == Gen) rems of
                    (before, m:after) -> Just (m, before ++ after)
                    (_, []) ->
                      case rems of
                        m:after -> Just (m, after)
                        [] -> Nothing
                  else Nothing

-- | Type comparison allowing unknowns for primitive resolution.
typeMatchesAllowUnknown :: [(Identifier, Int)] -- ^ Type constructor arities.
                        -> Maybe (Ty Ann) -- ^ Possibly unknown type.
                        -> Ty Ann -- ^ Expected type.
                        -> Bool -- ^ True when types match.
typeMatchesAllowUnknown tyCons mTy ty =
  case mTy of
    Nothing ->
      case ty of
        TyVar {} -> True
        TySkolem {} -> False
        _ -> False
    Just t ->
      case ty of
        TyVar {} -> True
        TySkolem {} -> tyEq tyCons t ty
        _ -> typeMatches tyCons (Just t) ty

-- | Lookup a binding by candidate identifiers.
-- | Lookup by candidates in a list-based environment.
lookupByCandidatesList :: forall a.
                          [(Identifier, a)] -- ^ Candidate bindings.
                       -> [(Identifier, Case)] -- ^ Candidate identifiers.
                       -> Maybe a -- ^ Matching binding when found.
lookupByCandidatesList env candidates =
  let names = map fst candidates
  in go names
  where
    -- | Try candidates in order.
    go :: [Identifier] -- ^ Remaining candidate names.
       -> Maybe a -- ^ Matching binding.
    go [] = Nothing
    go (n:ns) =
      case lookup n env of
        Just v -> Just v
        Nothing -> go ns

-- | Lookup by candidates in a Map-based environment.
lookupByCandidatesMap :: forall a.
                         Map.Map Identifier a -- ^ Candidate bindings.
                      -> [(Identifier, Case)] -- ^ Candidate identifiers.
                      -> Maybe a -- ^ Matching binding when found.
lookupByCandidatesMap env candidates =
  let names = map fst candidates
  in go names
  where
    go [] = Nothing
    go (n:ns) =
      case Map.lookup n env of
        Just v -> Just v
        Nothing -> go ns

-- | Lookup by candidates in a Map-based environment.
lookupByCandidates :: forall a.
                      Map.Map Identifier a -- ^ Candidate bindings.
                   -> [(Identifier, Case)] -- ^ Candidate identifiers.
                   -> Maybe a -- ^ Matching binding when found.
lookupByCandidates env candidates =
  let names = map fst candidates
  in go names
  where
    -- | Try candidates in order.
    go :: [Identifier] -- ^ Remaining candidate names.
       -> Maybe a -- ^ Matching binding.
    go [] = Nothing
    go (n:ns) =
      case Map.lookup n env of
        Just v -> Just v
        Nothing -> go ns

-- | Heuristic fallback for matching inflected variables in list-based local bindings.
lookupBySuffixList :: [(Identifier, a)] -- ^ Local environment bindings.
                   -> Identifier -- ^ Surface identifier.
                   -> Maybe a -- ^ Matching binding when found.
lookupBySuffixList env (mods, word) =
  let stripped =
        [ (mods, root)
        | suf <- bareCaseSuffixes
        , Just root <- [T.stripSuffix suf word]
        ]
  in findMatch stripped
  where
    findMatch [] = Nothing
    findMatch (ident:rest) =
      case lookup ident env of
        Just v -> Just v
        Nothing -> findMatch rest

-- | Heuristic fallback for matching inflected variables in Map-based local bindings.
lookupBySuffixMap :: Map.Map Identifier a -- ^ Local environment bindings.
                  -> Identifier -- ^ Surface identifier.
                  -> Maybe a -- ^ Matching binding when found.
lookupBySuffixMap env (mods, word) =
  let stripped =
        [ (mods, root)
        | suf <- bareCaseSuffixes
        , Just root <- [T.stripSuffix suf word]
        ]
  in findMatch stripped
  where
    findMatch [] = Nothing
    findMatch (ident:rest) =
      case Map.lookup ident env of
        Just v -> Just v
        Nothing -> findMatch rest

-- | Lookup by candidates in a HashMap-based environment (O(1) average).
lookupByCandidatesHM :: forall a.
                        HM.HashMap Identifier a -- ^ Candidate bindings.
                     -> [(Identifier, Case)] -- ^ Candidate identifiers.
                     -> Maybe a -- ^ Matching binding when found.
lookupByCandidatesHM env candidates =
  let names = map fst candidates
  in go names
  where
    go [] = Nothing
    go (n:ns) =
      case HM.lookup n env of
        Just v -> Just v
        Nothing -> go ns

-- | Heuristic fallback for matching inflected variables in HashMap-based local bindings (O(1) average).
lookupBySuffixHM :: HM.HashMap Identifier a -- ^ Local environment bindings.
                 -> Identifier -- ^ Surface identifier.
                 -> Maybe a -- ^ Matching binding when found.
lookupBySuffixHM env (mods, word) =
  let stripped =
        [ (mods, root)
        | suf <- bareCaseSuffixes
        , Just root <- [T.stripSuffix suf word]
        ]
  in findMatch stripped
  where
    findMatch [] = Nothing
    findMatch (ident:rest) =
      case HM.lookup ident env of
        Just v -> Just v
        Nothing -> findMatch rest

bareCaseSuffixes :: [Text]
bareCaseSuffixes =
  [ "yı", "yi", "yu", "yü"
  , "ı", "i", "u", "ü"
  , "ya", "ye", "a", "e"
  , "dan", "den", "tan", "ten"
  , "da", "de", "ta", "te"
  , "nın", "nin", "nun", "nün"
  , "ın", "in", "un", "ün"
  , "la", "le"
  ]

-- | Lookup a constructor binding by candidates.
lookupCtorByCandidates :: Map.Map Identifier a -- ^ Candidate constructors.
                       -> [(Identifier, Case)] -- ^ Candidate identifiers.
                       -> Maybe a -- ^ Matching constructor.
lookupCtorByCandidates = lookupByCandidates

-- | Infer a type for an expression when possible.
inferType :: Exp Ann -- ^ Expression to infer.
          -> EvalM (Maybe (Ty Ann)) -- ^ Inferred type.
inferType e =
  case e of
    IntLit {} -> return (Just (TyInt (mkAnn Nom NoSpan)))
    FloatLit {} -> return (Just (TyFloat (mkAnn Nom NoSpan)))
    StrLit {} -> return (Just (TyString (mkAnn Nom NoSpan)))
    Bind {bindExp} -> inferType bindExp
    Seq {second} -> inferType second
    Var {varCandidates} -> do
      MkEvalState{evalVals, evalCtors} <- get
      case lookupByCandidates evalVals varCandidates of
        Just v -> inferType v
        Nothing ->
          case lookupCtorByCandidates evalCtors varCandidates of
            Just ([], ty) -> return (Just ty)
            _ -> return Nothing
    App {fn, args} -> do
      fn' <- evalExpWith [] fn
      case fn' of
        Var {varCandidates} -> do
          MkEvalState{evalCtors, evalTyCons} <- get
          case lookupCtorByCandidates evalCtors varCandidates of
            Just (tys, resTy)
              | length tys == length args -> do
                  argTys <- mapM inferType args
                  if Nothing `elem` argTys
                    then return Nothing
                    else do
                      let actuals = catMaybes argTys
                      case unifyTypes (Map.toList evalTyCons) tys actuals of
                        Just subst -> return (Just (applySubst subst resTy))
                        Nothing -> return Nothing
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

-- | Apply a case annotation to an expression if it is a value.
applyTypeCase :: Case -- ^ Case to apply.
              -> Exp Ann -- ^ Expression to update.
              -> Exp Ann -- ^ Updated expression.
applyTypeCase cas exp =
  case exp of
    Var ann name candidates ->
      let filtered = filter (\(_, c) -> c == cas) candidates
          candidates' = if null filtered then candidates else filtered
      in Var (setAnnCase ann cas) name candidates'
    IntLit ann n ->
      IntLit (setAnnCase ann cas) n
    FloatLit ann n ->
      FloatLit (setAnnCase ann cas) n
    _ -> exp

-- | Check whether an inferred type matches an expected type.
typeMatches :: [(Identifier, Int)] -- ^ Type constructor arities.
            -> Maybe (Ty Ann) -- ^ Possibly unknown type.
            -> Ty Ann -- ^ Expected type.
            -> Bool -- ^ True when types match.
typeMatches tyCons mTy ty =
  case mTy of
    Nothing -> False
    Just t -> tyEq tyCons t ty

-- | Compare two types for compatibility.
tyEq :: [(Identifier, Int)] -- ^ Type constructor arities.
     -> Ty Ann -- ^ Left type.
     -> Ty Ann -- ^ Right type.
     -> Bool -- ^ True when types are compatible.
tyEq tyCons t1 t2 =
  let n1 = normalizeTy tyCons t1
      n2 = normalizeTy tyCons t2
  in case (n1, n2) of
    (TyString _, TyString _) -> True
    (TyInt _, TyInt _) -> True
    (TyFloat _, TyFloat _) -> True
    (Arr _ d1 i1, Arr _ d2 i2) -> tyEq tyCons d1 d2 && tyEq tyCons i1 i2
    (TyInd _ n1', TyInd _ n2') -> identMatches n1' n2'
    (TySkolem _ n1', TySkolem _ n2') -> n1' == n2'
    (TySkolem {}, TyVar {}) -> True
    (TyVar {}, TySkolem {}) -> True
    (TySkolem {}, _) -> False
    (_, TySkolem {}) -> False
    (TyVar _ _, _) -> True
    (_, TyVar _ _) -> True
    (TyApp _ c1 as1, TyApp _ c2 as2) ->
      tyEq tyCons c1 c2 && length as1 == length as2 && and (zipWith (tyEq tyCons) as1 as2)
    _ -> False

-- | Normalize type applications using constructor arities.
normalizeTy :: [(Identifier, Int)] -- ^ Type constructor arities.
            -> Ty Ann -- ^ Type to normalize.
            -> Ty Ann -- ^ Normalized type.
normalizeTy tyCons ty =
  case ty of
    TyInt {} -> ty
    TyFloat {} -> ty
    TySkolem {} -> ty
    TyApp ann (TyInd _ name) args ->
      case lookup name tyCons of
        Just arity | arity > 0 -> TyApp ann (TyInd (mkAnn Nom NoSpan) name) (map (normalizeTy tyCons) args)
        _ -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizeTy tyCons ctor) (map (normalizeTy tyCons) args)
    Arr ann d i ->
      Arr ann (normalizeTy tyCons d) (normalizeTy tyCons i)
    _ -> ty

-- | Compare identifiers, allowing missing namespace prefixes.
identMatches :: Identifier -- ^ Left identifier.
             -> Identifier -- ^ Right identifier.
             -> Bool -- ^ True when identifiers match loosely.
identMatches (xs1, x1) (xs2, x2) =
  x1 == x2 && (xs1 == xs2 || null xs1 || null xs2)

-- | Unify expected types with actual types, returning substitutions.
unifyTypes :: [(Identifier, Int)] -- ^ Type constructor arities.
           -> [Ty Ann] -- ^ Expected types.
           -> [Ty Ann] -- ^ Actual types.
           -> Maybe [(Identifier, Ty Ann)] -- ^ Substitution when unification succeeds.
unifyTypes tyCons expected actual =
  foldl' go (Just []) (zip expected actual)
  where
    -- | Fold step for unification.
    go :: Maybe [(Identifier, Ty Ann)] -- ^ Current substitution.
       -> (Ty Ann, Ty Ann) -- ^ Expected and actual types.
       -> Maybe [(Identifier, Ty Ann)] -- ^ Updated substitution.
    go Nothing _ = Nothing
    go (Just subst) (e, a) =
      unifyOne subst (normalizeTy tyCons e) (normalizeTy tyCons a)

    -- | Unify a single expected/actual type pair.
    unifyOne :: [(Identifier, Ty Ann)] -- ^ Current substitution.
             -> Ty Ann -- ^ Expected type.
             -> Ty Ann -- ^ Actual type.
             -> Maybe [(Identifier, Ty Ann)] -- ^ Updated substitution.
    unifyOne subst e a =
      case e of
        TyInt _ ->
          case a of
            TyInt _ -> Just subst
            _ -> Nothing
        TyVar _ name ->
          case lookup name subst of
            Just bound ->
              if tyEq tyCons bound a
                then Just subst
                else Nothing
            Nothing -> Just ((name, a) : subst)
        TySkolem _ name ->
          case a of
            TySkolem _ name' | name == name' -> Just subst
            TyVar {} -> Just subst
            _ -> Nothing
        TyInd _ n1 ->
          case a of
            TyInd _ n2 | n1 == n2 -> Just subst
            _ -> Nothing
        TyString _ ->
          case a of
            TyString _ -> Just subst
            _ -> Nothing
        Arr _ d1 i1 ->
          case a of
            Arr _ d2 i2 -> do
              subst' <- unifyOne subst d1 d2
              unifyOne subst' i1 i2
            _ -> Nothing
        TyApp _ c1 as1 ->
          case a of
            TyApp _ c2 as2
              | length as1 == length as2 -> do
                  subst' <- unifyOne subst c1 c2
                  foldl' go (Just subst') (zip as1 as2)
            _ -> Nothing

-- | Apply a type substitution to a type.
applySubst :: [(Identifier, Ty Ann)] -- ^ Substitution bindings.
           -> Ty Ann -- ^ Type to rewrite.
           -> Ty Ann -- ^ Rewritten type.
applySubst subst ty =
  case ty of
    TyVar ann name ->
      case lookup name subst of
        Just t -> t
        Nothing -> TyVar ann name
    TySkolem {} -> ty
    TyInt {} -> ty
    TyFloat {} -> ty
    TyInd {} -> ty
    TyString {} -> ty
    Arr ann d i -> Arr ann (applySubst subst d) (applySubst subst i)
    TyApp ann ctor args ->
      TyApp ann (applySubst subst ctor) (map (applySubst subst) args)

-- | Evaluate a statement with optional module context.
evalStmtInFile :: Maybe FilePath -- ^ Current file path.
               -> Stmt Ann -- ^ Statement to evaluate.
               -> EvalM () -- ^ No result.
evalStmtInFile mPath stmt =
  do
    modify (\s -> s { evalCurrentFile = mPath })
    case stmt of
      Defn name _ e ->
        modify (\s -> s { evalVals = Map.insert name e (evalVals s) })
      Function name args _ body _ ->
        modify (\s -> s { evalFuncs = MultiMap.insert name (args, body) (evalFuncs s) })
      PrimFunc name args _ _ ->
        case primImpl mPath name args of
          Nothing -> return ()
          Just impl ->
            modify (\s -> s { evalPrimFuncs = MultiMap.insert name (args, impl) (evalPrimFuncs s) })
      Load _ ->
        return ()
      NewType name params ctors -> do
        let selectors = []
            resultTy =
              case params of
                [] -> TyInd (mkAnn Nom NoSpan) name
                _ -> TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) params
            ctorSigs =
              [ (ctorName, (ctorArgs, resultTy))
              | ((ctorName, _), ctorArgs) <- ctors
              ]
        modify (\s -> s { evalSelectors = foldr (uncurry MultiMap.insert) (evalSelectors s) selectors
                        , evalCtors = Map.union (Map.fromList ctorSigs) (evalCtors s)
                        , evalTyCons = Map.insert name (length params) (evalTyCons s)
                        })
      PrimType name ->
        modify (\s -> s { evalTyCons = Map.insert name 0 (evalTyCons s) })
      ExpStmt e -> do
        _ <- evalExp e
        return ()

-- | Evaluate a statement in the global context.
evalStmt :: Stmt Ann -- ^ Statement to evaluate.
         -> EvalM () -- ^ No result.
evalStmt = evalStmtInFile Nothing

-- | Evaluate a statement inside the REPL.
replStmt :: Stmt Ann -- ^ Statement to evaluate.
         -> EvalM () -- ^ No result.
replStmt stmt =
  case stmt of
    ExpStmt e -> do
      _ <- evalExp e
      liftIO (putStrLn "")
      -- liftIO (putStrLn (prettyExp e'))
    PrimFunc {} -> evalStmt stmt
    PrimType {} -> evalStmt stmt
    _ -> evalStmt stmt

-- | Lookup the primitive implementation for a name and argument list.
primImpl :: Maybe FilePath -- ^ Current file path.
         -> Identifier -- ^ Primitive name.
         -> [Arg Ann] -- ^ Argument types.
         -> Maybe ([Exp Ann] -> EvalM (Exp Ann)) -- ^ Primitive implementation when known.
primImpl mPath ident args = do
  guard (primFileMatches mPath ident)
  case ident of
    ([], "yaz")
      | [(_, TyInt _)] <- args -> Just primWrite
      | [(_, TyFloat _)] <- args -> Just primWrite
      | [(_, TyString _)] <- args -> Just primWrite
      | [_, _] <- args -> Just primWriteFile
      | otherwise -> Nothing
    ([], "oku")
      | [] <- args -> Just primRead
      | [(_, TyString _)] <- args -> Just primReadFile
      | otherwise -> Nothing
    ([], "uzunluk") -> Just primStringLength
    ([], "birleşim") -> Just primStringConcat
    (["tam", "sayı"], "hal") -> Just primStringToInt
    (["ondalık", "sayı"], "hal") -> Just primStringToFloat
    ([], "ters")
      | [(_, TyString _)] <- args -> Just primStringReverse
      | otherwise -> Nothing
    ([], "toplam")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatBin "toplam" (+))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntBin "toplam" (+))
      | otherwise ->
          Nothing
    ([], "çarpım")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatBin "çarpım" (*))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntBin "çarpım" (*))
      | otherwise ->
          Nothing
    ([], "fark")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatBin "fark" (-))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntBin "fark" (-))
      | otherwise ->
          Nothing
    ([], "bölüm")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just primFloatDiv
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just primIntDiv
      | otherwise ->
          Nothing
    ([], "kalan")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just primFloatMod
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just primIntMod
      | otherwise ->
          Nothing
    (["dizge"], "hal")
      | [(_, TyFloat _)] <- args ->
          Just primFloatToString
      | [(_, TyInt _)] <- args ->
          Just primIntToString
      | otherwise ->
          Nothing
    ([], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "eşitlik" (==))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "eşitlik" (==))
      | otherwise ->
          Nothing
    ([], "küçüklük")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "küçüklük" (<))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "küçüklük" (<))
      | otherwise ->
          Nothing
    (["küçük"], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "küçük-eşitlik" (<=))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "küçük-eşitlik" (<=))
      | otherwise ->
          Nothing
    ([], "büyüklük")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "büyüklük" (>))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "büyüklük" (>))
      | otherwise ->
          Nothing
    (["büyük"], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "büyük-eşitlik" (>=))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "büyük-eşitlik" (>=))
      | otherwise ->
          Nothing
    (["sayı"], "çek") -> Just primIntRandom
    ([], "sayı-çek") -> Just primIntRandom
    _ -> Nothing

-- | Check whether a primitive belongs to a given file context.
primFileMatches :: Maybe FilePath -- ^ Current file path.
                -> Identifier -- ^ Primitive name.
                -> Bool -- ^ True when primitive belongs to file.
primFileMatches mPath ident =
  case mPath of
    Just path -> takeFileName path `elem` primFiles ident
    _ -> False

-- | Map a primitive identifier to the files that define it.
primFiles :: Identifier -- ^ Primitive identifier.
          -> [FilePath] -- ^ Source file paths when present.
primFiles = Prim.primFiles

-- | Primitive print for integers and strings.
primWrite :: [Exp Ann] -- ^ Arguments.
          -> EvalM (Exp Ann) -- ^ Result expression.
primWrite args =
  case args of
    [StrLit _ s] -> do
      liftIO (putStrLn (T.unpack s))
      liftIO (hFlush stdout)
      return (Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)])
    [IntLit _ n] -> do
      liftIO (print n)
      liftIO (hFlush stdout)
      return (Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)])
    [FloatLit _ n] -> do
      liftIO (print n)
      liftIO (hFlush stdout)
      return (Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "yaz") []) args)

-- | Primitive read from standard input.
primRead :: [Exp Ann] -- ^ Arguments.
         -> EvalM (Exp Ann) -- ^ Result expression.
primRead args =
  case args of
    [] -> do
      line <- liftIO getLine
      return (StrLit (mkAnn Nom NoSpan) (T.pack line))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "oku") []) args)

-- | Primitive read from a file path.
primReadFile :: [Exp Ann] -- ^ Arguments.
             -> EvalM (Exp Ann) -- ^ Result expression.
primReadFile args =
  case args of
    [StrLit _ path] -> do
      st <- get
      content <- liftIO (readFirstPath (resolveReadCandidates st path))
      case content of
        Nothing -> return (Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)])
        Just text ->
          return
            (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)])
              [StrLit (mkAnn Nom NoSpan) text])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "oku") []) args)

-- | Primitive write to a file path.
primWriteFile :: [Exp Ann] -- ^ Arguments.
              -> EvalM (Exp Ann) -- ^ Result expression.
primWriteFile args =
  case args of
    [StrLit _ path, StrLit _ content] -> do
      st <- get
      let resolved = resolvePath st path
      res <- liftIO (try (TIO.writeFile resolved content) :: IO (Either SomeException ()))
      case res of
        Left _ -> return (boolToExp False)
        Right _ -> return (boolToExp True)
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "yaz") []) args)

-- | Resolve a file path relative to the current file when needed.
resolvePath :: EvalState -- ^ Current evaluator state.
            -> Text -- ^ Input path.
            -> FilePath -- ^ Resolved path.
resolvePath st path =
  let raw = T.unpack path
  in case evalCurrentFile st of
       Just base | isRelative raw -> takeDirectory base </> raw
       _ -> raw

-- | Build read candidates by walking up parent directories.
resolveReadCandidates :: EvalState -- ^ Current evaluator state.
                      -> Text -- ^ Input path.
                      -> [FilePath] -- ^ Candidate paths.
resolveReadCandidates st path =
  let raw = T.unpack path
  in case evalCurrentFile st of
       Just base | isRelative raw ->
         let start = takeDirectory base
         in map (</> raw) (parentDirs start)
       _ -> [raw]

-- | Try reading from the first existing candidate path.
readFirstPath :: [FilePath] -- ^ Candidate paths.
              -> IO (Maybe Text) -- ^ First readable contents.
readFirstPath paths =
  case paths of
    [] -> return Nothing
    p:ps -> do
      res <- try (TIO.readFile p) :: IO (Either SomeException Text)
      case res of
        Right content -> return (Just content)
        Left _ -> readFirstPath ps

-- | Collect parent directories up to the filesystem root.
parentDirs :: FilePath -- ^ Directory path.
           -> [FilePath] -- ^ Parent directories.
parentDirs dir =
  let parent = takeDirectory dir
  in if parent == dir
       then [dir]
       else dir : parentDirs parent

-- | Primitive to compute string length.
primStringLength :: [Exp Ann] -- ^ Arguments.
                 -> EvalM (Exp Ann) -- ^ Result expression.
primStringLength args =
  case args of
    [StrLit ann s] ->
      return (IntLit ann (fromIntegral (T.length s)))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "uzunluk") []) args)

-- | Primitive string concatenation.
primStringConcat :: [Exp Ann] -- ^ Arguments.
                 -> EvalM (Exp Ann) -- ^ Result expression.
primStringConcat args =
  case args of
    [StrLit ann a, StrLit _ b] ->
      return (StrLit ann (a <> b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "birleşim") []) args)

-- | Primitive string reverse.
primStringReverse :: [Exp Ann] -- ^ Arguments.
                  -> EvalM (Exp Ann) -- ^ Result expression.
primStringReverse args =
  case args of
    [StrLit ann s] ->
      return (StrLit ann (T.reverse s))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "ters") []) args)

-- | Primitive to parse a string into an integer option.
primStringToInt :: [Exp Ann] -- ^ Arguments.
                -> EvalM (Exp Ann) -- ^ Result expression.
primStringToInt args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n ->
          return
            (App ann (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)])
              [IntLit ann n])
        Nothing ->
          return (Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "tam-sayı-hali") []) args)

-- | Primitive to parse a string into a floating-point option.
primStringToFloat :: [Exp Ann] -- ^ Arguments.
                  -> EvalM (Exp Ann) -- ^ Result expression.
primStringToFloat args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n ->
          return
            (App ann (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)])
              [FloatLit ann n])
        Nothing ->
          return (Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "ondalık-sayı-hali") []) args)

-- | Primitive integer binary operator.
primIntBin :: Text -- ^ Operator name.
           -> (Integer -> Integer -> Integer) -- ^ Integer operator.
           -> [Exp Ann] -- ^ Arguments.
           -> EvalM (Exp Ann) -- ^ Result expression.
primIntBin fname op args =
  case args of
    [IntLit ann a, IntLit _ b] ->
      return (IntLit ann (op a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], fname) []) args)

-- | Primitive floating-point binary operator.
primFloatBin :: Text -- ^ Operator name.
             -> (Double -> Double -> Double) -- ^ Floating operator.
             -> [Exp Ann] -- ^ Arguments.
             -> EvalM (Exp Ann) -- ^ Result expression.
primFloatBin fname op args =
  case args of
    [FloatLit ann a, FloatLit _ b] ->
      return (FloatLit ann (op a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], fname) []) args)

-- | Primitive integer division (division by zero yields 0).
primIntDiv :: [Exp Ann] -- ^ Arguments.
           -> EvalM (Exp Ann) -- ^ Result expression.
primIntDiv args =
  case args of
    [IntLit ann a, IntLit _ b] ->
      return (IntLit ann (if b == 0 then 0 else a `div` b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "bölüm") []) args)

-- | Primitive floating-point division (division by zero yields 0.0).
primFloatDiv :: [Exp Ann] -- ^ Arguments.
             -> EvalM (Exp Ann) -- ^ Result expression.
primFloatDiv args =
  case args of
    [FloatLit ann a, FloatLit _ b] ->
      return (FloatLit ann (if b == 0 then 0 else a / b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "bölüm") []) args)

-- | Primitive integer modulo (division by zero yields 0).
primIntMod :: [Exp Ann] -- ^ Arguments.
           -> EvalM (Exp Ann) -- ^ Result expression.
primIntMod args =
  case args of
    [IntLit ann a, IntLit _ b] ->
      return (IntLit ann (if b == 0 then 0 else a `mod` b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "kalan") []) args)

-- | Primitive floating-point modulo (division by zero yields 0.0).
primFloatMod :: [Exp Ann] -- ^ Arguments.
             -> EvalM (Exp Ann) -- ^ Result expression.
primFloatMod args =
  case args of
    [FloatLit ann a, FloatLit _ b] ->
      return (FloatLit ann (if b == 0 then 0 else mod' a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "kalan") []) args)

-- | Primitive integer random in inclusive bounds.
primIntRandom :: [Exp Ann] -- ^ Arguments.
              -> EvalM (Exp Ann) -- ^ Result expression.
primIntRandom args =
  case args of
    [IntLit ann a, IntLit _ b] -> do
      let lo = min a b
          hi = max a b
      st <- get
      case evalRandState st of
        Just seed -> do
          let (nextSeed, n) = randRange seed lo hi
          put st { evalRandState = Just nextSeed }
          return (IntLit ann n)
        Nothing -> do
          mSeed <- liftIO (lookupEnv "KIP_RANDOM_SEED")
          case mSeed >>= readMaybe of
            Just (seedVal :: Integer) -> do
              let seed = fromIntegral seedVal :: Word32
                  (nextSeed, n) = randRange seed lo hi
              put st { evalRandState = Just nextSeed }
              return (IntLit ann n)
            Nothing -> do
              n <- liftIO (randomRIO (lo, hi))
              return (IntLit ann n)
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) (["sayı"], "çek") []) args)
  where
    randRange :: Word32 -> Integer -> Integer -> (Word32, Integer)
    randRange seed lo hi =
      let nextSeed = seed * 1664525 + 1013904223
          range = hi - lo + 1
          val = lo + (toInteger nextSeed `mod` range)
      in (nextSeed, val)

-- | Primitive integer comparison operator.
primIntCmp :: Text -- ^ Operator name.
           -> (Integer -> Integer -> Bool) -- ^ Predicate.
           -> [Exp Ann] -- ^ Arguments.
           -> EvalM (Exp Ann) -- ^ Result expression.
primIntCmp fname op args =
  case args of
    [IntLit _ a, IntLit _ b] ->
      return (boolToExp (op a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], fname) []) args)

-- | Primitive floating-point comparison operator.
primFloatCmp :: Text -- ^ Operator name.
             -> (Double -> Double -> Bool) -- ^ Predicate.
             -> [Exp Ann] -- ^ Arguments.
             -> EvalM (Exp Ann) -- ^ Result expression.
primFloatCmp fname op args =
  case args of
    [FloatLit _ a, FloatLit _ b] ->
      return (boolToExp (op a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], fname) []) args)

-- | Primitive integer to string conversion.
primIntToString :: [Exp Ann] -- ^ Arguments.
                -> EvalM (Exp Ann) -- ^ Result expression.
primIntToString args =
  case args of
    [IntLit ann n] ->
      return (StrLit ann (T.pack (show n)))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) (["dizge"], "hal") []) args)

-- | Primitive floating-point to string conversion.
primFloatToString :: [Exp Ann] -- ^ Arguments.
                  -> EvalM (Exp Ann) -- ^ Result expression.
primFloatToString args =
  case args of
    [FloatLit ann n] ->
      return (StrLit ann (T.pack (show n)))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) (["dizge"], "hal") []) args)

-- | Convert a boolean into a Kip boolean value expression.
boolToExp :: Bool -- ^ Boolean value.
          -> Exp Ann -- ^ Kip boolean expression.
boolToExp b =
  let name = if b then ([], "doğru") else ([], "yanlış")
  in Var (mkAnn Nom NoSpan) name [(name, Nom)]


-- | Run an evaluator action with a starting state.
runEvalM :: EvalM a -- ^ Evaluator computation.
         -> EvalState -- ^ Initial evaluator state.
         -> IO (Either EvalError (a, EvalState)) -- ^ Result or error.
runEvalM m s = runExceptT (runStateT m s)
