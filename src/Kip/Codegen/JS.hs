{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{- |
JavaScript code generator for Kip.

= Design

This backend emits JavaScript text directly from the typed Kip AST. The output is
intended for execution in environments that support top-level @await@ (Node ESM
and the browser playground loader), so generated code can keep Kip's effectful
operations (read/write/random) in a uniform async style.

= Output layout

'codegenProgram' always emits:

1. A primitive prelude ('jsPrimitives').
2. An async runner function containing user code.
3. Overload wrappers ('jsOverloadWrappers') installed after function
   declarations so wrappers can capture library implementations.

= Semantics choices

* Expressions are lowered to /awaitable/ JavaScript expressions.
* Pattern matching is lowered to ordered @if/else if@ chains.
* Kip ADTs become tagged JS objects: @{ tag, args }@.
* Partial-application edge cases are preserved by dedicated lowering helpers.

= Notes

This module prefers explicit textual codegen over an intermediate JS AST to keep
debuggability high and to keep generated code close to the source language model.
-}
module Kip.Codegen.JS
  ( codegenProgram
  , codegenStmts
  , codegenStmt
  , codegenExp
  ) where

import Data.Char (isLetter)
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Kip.AST

data CodegenCtx = MkCodegenCtx
  { sectionableFns :: Set.Set Identifier
  }

emptyCodegenCtx :: CodegenCtx
emptyCodegenCtx = MkCodegenCtx Set.empty

buildCodegenCtx :: [Stmt Ann] -> CodegenCtx
buildCodegenCtx stmts =
  let arityMap = foldl collectArity Map.empty stmts
      sectionable =
        Set.fromList
          [ ident
          | (ident, arities) <- Map.toList arityMap
          , any (> 1) arities
          , 1 `notElem` arities
          ]
  in MkCodegenCtx sectionable
  where
    collectArity acc stmt =
      case stmt of
        Function ident args _ _ _ -> Map.insertWith mergeArities ident [length args] acc
        PrimFunc ident args _ _ -> Map.insertWith mergeArities ident [length args] acc
        _ -> acc
    mergeArities new old = Set.toList (Set.fromList (new ++ old))

-- | Codegen a list of statements into a JS-like program.
-- Order: primitives, then async IIFE containing:
--   - function definitions (hoisted)
--   - overload wrappers (capture hoisted functions)
--   - expression statements (executed)
codegenProgram :: [Stmt Ann] -> Text
codegenProgram stmts =
  let ctx = buildCodegenCtx stmts
      -- Separate function definitions from other statements
      (funcDefs, otherStmts) = partition isFunctionDef stmts
      -- Deduplicate function definitions by JS name, keeping first occurrence
      dedupedFuncDefs = deduplicateByJsName funcDefs
      mergedFuncDefs = mergeCompatibleFunctions dedupedFuncDefs
      -- Function definitions first (they hoist anyway)
      funcCode = T.intercalate "\n\n" (map (codegenStmtWith ctx) mergedFuncDefs)
      -- Expression statements last
      exprCode = T.intercalate "\n\n" (map (codegenStmtWith ctx) otherStmts)
      -- All user code inside async IIFE with wrappers after functions
      wrapped = T.unlines
        [ "const __kip_run = async () => {"
        , funcCode
        , ""
        , jsOverloadWrappers
        , ""
        , exprCode
        , "};"
        , "await __kip_run();"
        , "__kip_close_stdin();"
        ]
  in jsPrimitives <> "\n\n" <> wrapped

-- | Check if a statement is a function definition (including types).
isFunctionDef :: Stmt Ann -> Bool
isFunctionDef stmt =
  case stmt of
    Function {} -> True
    Defn {} -> True
    NewType {} -> True
    PrimFunc {} -> True
    PrimType {} -> True
    Load {} -> True
    ExpStmt {} -> False

-- | Deduplicate simple delegation functions by JS identifier.
--
-- When multiple library files define the same single-wildcard function
-- (e.g. @negatif@ in both @tam-sayı.kip@ and @ondalık-sayı.kip@), only
-- the first definition is kept.  Functions with pattern matching are never
-- deduplicated because they may carry type-specific clauses needed by the
-- overload-wrapper mechanism.
deduplicateByJsName :: [Stmt Ann] -> [Stmt Ann]
deduplicateByJsName = go Set.empty
  where
    go _ [] = []
    go seen (stmt:rest) =
      case stmtJsName stmt of
        Just name
          | name `Set.member` seen -> go seen rest
          | otherwise -> stmt : go (Set.insert name seen) rest
        Nothing -> stmt : go seen rest
    stmtJsName (Function name _ _ [Clause (PWildcard _) _] _) =
      Just (toJsIdent name)
    stmtJsName _ = Nothing

data OverloadKey = OverloadKey
  { odKeyName :: Text
  , odKeyArgs :: [Identifier]
  }
  deriving (Eq, Ord, Show)

-- | Merge function statements that have the same emitted JS name and exactly
-- the same argument identifiers.
--
-- This keeps codegen predictable while allowing multi-definition functions like
-- @filtre@ in dpll to become one JS declaration with all clauses.
mergeCompatibleFunctions :: [Stmt Ann] -> [Stmt Ann]
mergeCompatibleFunctions stmts = snd (foldl step (Map.empty, []) stmts)
  where
    step (seen, acc) stmt =
      case stmt of
        Function name args _ clauses isInf ->
          let key =
                OverloadKey
                  { odKeyName = toJsIdent name
                  , odKeyArgs = map argIdent args
                  }
          in case Map.lookup key seen of
               Nothing ->
                 let idx = length acc
                 in (Map.insert key idx seen, acc ++ [stmt])
               Just idx ->
                 (seen, mergeAt idx clauses isInf acc)
        _ ->
          (seen, acc ++ [stmt])

    mergeAt idx newClauses isInf acc =
      let (prefix, target:suffix) = splitAt idx acc
      in case target of
           Function name oldArgs oldTy oldClauses oldInf ->
             let mergedInf = oldInf || isInf
                 mergedStmt = Function name oldArgs oldTy (oldClauses ++ newClauses) mergedInf
             in prefix ++ (mergedStmt : suffix)
           _ -> acc

-- | JavaScript implementations of Kip primitives.
-- Uses 'var' so user code can override with 'const'.
-- Note: Boolean constructors are defined by the library's doğruluk type,
-- so we use a helper to get the correct constructor format at runtime.
-- Primitives that have library overloads (ters, birleşim, uzunluk, toplam)
-- are stored with __kip_ prefix and wrapped at the end.
-- The code is async-capable to support interactive browser I/O.
-- When KIP_RANDOM_SEED is set, the RNG mirrors the runtime LCG.
jsPrimitives :: Text
jsPrimitives = T.unlines
  [ "// Kip → JavaScript (async/await for interactive browser support)"
  , ""
  , "// Node.js modules for I/O (lazy loaded)"
  , "var __kip_fs = null;"
  , "var __kip_readline = null;"
  , "var __kip_stdin_queue = [];"
  , "var __kip_stdin_waiters = [];"
  , "var __kip_stdin_closed = false;"
  , "var __kip_stdin_mode = null;"
  , "var __kip_is_browser = (typeof window !== 'undefined');"
  , "var __kip_require = null;"
  , "var __kip_random_seed = null;"
  , "if (!__kip_is_browser && typeof process !== 'undefined' && process.versions && process.versions.node) {"
  , "  const { createRequire } = await import('module');"
  , "  __kip_require = createRequire(import.meta.url);"
  , "}"
  , "if (__kip_is_browser) {"
  , "  if (typeof window.__kip_write !== 'function') {"
  , "    window.__kip_write = (x) => console.log(x);"
  , "  }"
  , "  if (typeof window.__kip_read_line !== 'function') {"
  , "    window.__kip_read_line = async () => {"
  , "      var v = prompt('Input:');"
  , "      return v === null ? '' : v;"
  , "    };"
  , "  }"
  , "}"
  , "if (!__kip_is_browser && typeof process !== 'undefined' && process.env && process.env.KIP_RANDOM_SEED) {"
  , "  const seed = Number(process.env.KIP_RANDOM_SEED);"
  , "  if (!Number.isNaN(seed)) {"
  , "    __kip_random_seed = seed >>> 0;"
  , "  }"
  , "}"
  , "if (__kip_is_browser && typeof window.__kip_random_seed === 'number') {"
  , "  __kip_random_seed = window.__kip_random_seed >>> 0;"
  , "}"
  , "var __kip_rand = () => {"
  , "  if (__kip_random_seed === null) return Math.floor(Math.random() * 4294967296);"
  , "  __kip_random_seed = (Math.imul(__kip_random_seed, 1664525) + 1013904223) >>> 0;"
  , "  return __kip_random_seed;"
  , "};"
  , ""
  , "// Initialize stdin buffer for line-by-line reading (Node.js only)"
  , "var __kip_init_stdin = () => {"
  , "  if (typeof process === 'undefined' || !process.stdin) return;"
  , "  if (__kip_stdin_mode !== null) return;"
  , "  if (!__kip_require) return;"
  , "  if (process.stdin.isTTY === false) {"
  , "    __kip_stdin_mode = 'pipe';"
  , "    __kip_fs = __kip_fs || __kip_require('fs');"
  , "    try {"
  , "      __kip_stdin_queue = __kip_fs.readFileSync(0, 'utf8').split('\\n');"
  , "    } catch (e) {"
  , "      __kip_stdin_queue = [];"
  , "    }"
  , "    __kip_stdin_closed = true;"
  , "    return;"
  , "  }"
  , "  __kip_stdin_mode = 'tty';"
  , "  if (__kip_readline === null) {"
  , "    var readline = __kip_require('readline');"
  , "    __kip_readline = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });"
  , "    __kip_readline.on('line', (line) => {"
  , "      if (__kip_stdin_waiters.length > 0) {"
  , "        __kip_stdin_waiters.shift()(line);"
  , "      } else {"
  , "        __kip_stdin_queue.push(line);"
  , "      }"
  , "    });"
  , "    __kip_readline.on('close', () => {"
  , "      __kip_stdin_closed = true;"
  , "      while (__kip_stdin_waiters.length > 0) {"
  , "        __kip_stdin_waiters.shift()('');"
  , "      }"
  , "    });"
  , "  }"
  , "};"
  , "var __kip_close_stdin = () => {"
  , "  if (__kip_readline && __kip_stdin_mode === 'tty') {"
  , "    __kip_readline.close();"
  , "    __kip_readline = null;"
  , "  }"
  , "};"
  , ""
  , "// Helper to create a tagged value (works whether constructor is function or object)"
  , "var __kip_bool = (tag) => typeof window !== 'undefined' && typeof window[tag] === 'function' ? window[tag]() : { tag, args: [] };"
  , "var __kip_true = () => typeof doğru === 'function' ? doğru() : doğru;"
  , "var __kip_false = () => typeof yanlış === 'function' ? yanlış() : yanlış;"
  , "var __kip_some = (x) => typeof varlık === 'function' ? varlık(x) : { tag: 'varlık', args: [x] };"
  , "var __kip_none = () => typeof yokluk === 'function' ? yokluk() : { tag: 'yokluk', args: [] };"
  , "var __kip_float = (x) => ({ __kip_float: true, value: x });"
  , "var __kip_is_float = (x) => typeof x === 'object' && x && x.__kip_float === true;"
  , "var __kip_num = (x) => __kip_is_float(x) ? x.value : x;"
  , ""
  , "// Primitive boolean constructors (will be overridden by library)"
  , "var doğru = { tag: \"doğru\", args: [] };"
  , "var yanlış = { tag: \"yanlış\", args: [] };"
  , ""
  , "// Option type constructors (will be overridden by library if defined)"
  , "var varlık = (...args) => ({ tag: \"varlık\", args });"
  , "var yokluk = (...args) => ({ tag: \"yokluk\", args });"
  , ""
  , "// Unit type (will be overridden by library if defined)"
  , "var bitimlik = (...args) => ({ tag: \"bitimlik\", args });"
  , ""
  , "// Primitive functions for strings/numbers (may be overloaded by library for other types)"
  , "var __kip_prim_ters = (s) => s.split('').reverse().join('');"
  , "var __kip_prim_birleşim = (a, b) => __kip_num(a) + __kip_num(b);"
  , "var __kip_prim_uzunluk = (s) => s.length;"
  , "var __kip_prim_toplam = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) + __kip_num(b)) : (__kip_num(a) + __kip_num(b));"
  , "var __kip_prim_fark = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) - __kip_num(b)) : (__kip_num(a) - __kip_num(b));"
  , ""
  , "// I/O primitives - async to support browser interactivity"
  , "var __kip_prim_oku_stdin = async () => {"
  , "  // Check for browser runtime at call time"
  , "  if (__kip_is_browser && typeof window.__kip_read_line === 'function') {"
  , "    return await window.__kip_read_line();"
  , "  }"
  , "  // Node.js fallback"
  , "  __kip_init_stdin();"
  , "  if (__kip_stdin_queue.length > 0) {"
  , "    return __kip_stdin_queue.shift();"
  , "  }"
  , "  if (__kip_stdin_closed) {"
  , "    return '';"
  , "  }"
  , "  return await new Promise((resolve) => {"
  , "    __kip_stdin_waiters.push(resolve);"
  , "  });"
  , "};"
  , "var __kip_prim_oku_dosya = (path) => {"
  , "  if (!__kip_require) return __kip_none();"
  , "  __kip_fs = __kip_fs || __kip_require('fs');"
  , "  try {"
  , "    return __kip_some(__kip_fs.readFileSync(path, 'utf8'));"
  , "  } catch (e) {"
  , "    return __kip_none();"
  , "  }"
  , "};"
  , "var __kip_prim_yaz_dosya = (path, content) => {"
  , "  if (!__kip_require) return __kip_false();"
  , "  __kip_fs = __kip_fs || __kip_require('fs');"
  , "  try {"
  , "    __kip_fs.writeFileSync(path, content);"
  , "    return __kip_true();"
  , "  } catch (e) {"
  , "    return __kip_false();"
  , "  }"
  , "};"
  , ""
  , "// Primitive functions (can be overridden)"
  , "var yaz = (x) => {"
  , "  var val = __kip_is_float(x) ? x.value : x;"
  , "  var output = __kip_is_float(x) && Number.isInteger(val) ? String(val) + '.0' : val;"
  , "  if (__kip_is_browser && typeof window.__kip_write === 'function') {"
  , "    window.__kip_write(output);"
  , "  } else {"
  , "    console.log(output);"
  , "  }"
  , "  return typeof bitimlik === 'function' ? bitimlik() : bitimlik;"
  , "};"
  , "var çarpım = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) * __kip_num(b)) : (__kip_num(a) * __kip_num(b));"
  , "var fark = __kip_prim_fark;"
  , "var bölüm = (a, b) => {"
  , "  var av = __kip_num(a);"
  , "  var bv = __kip_num(b);"
  , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
  , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av / bv) : Math.trunc(av / bv);"
  , "};"
  , "var kalan = (a, b) => {"
  , "  var av = __kip_num(a);"
  , "  var bv = __kip_num(b);"
  , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
  , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av % bv) : (av % bv);"
  , "};"
  , "var karekök = (a) => __kip_float(Math.sqrt(__kip_num(a)) * 1.0);"
  , "var radyan = (a) => __kip_float(__kip_num(a) * Math.PI / 180);"
  , "var derece = (a) => __kip_float(__kip_num(a) * 180 / Math.PI);"
  , "var pi_sayısı = () => __kip_float(Math.PI);"
  , "var taban = (a) => Math.floor(__kip_num(a));"
  , "var tavan = (a) => Math.ceil(__kip_num(a));"
  , "var tam_sayı_ondalık_sayı_hali = (a) => __kip_float(a * 1.0);"
  , "var sayı_çek = (a, b) => {"
  , "  var lo = Math.min(a, b);"
  , "  var hi = Math.max(a, b);"
  , "  var range = hi - lo + 1;"
  , "  return lo + (__kip_rand() % range);"
  , "};"
  , "var eşitlik = (a, b) => __kip_num(a) === __kip_num(b) ? __kip_true() : __kip_false();"
  , "var küçüklük = (a, b) => __kip_num(a) < __kip_num(b) ? __kip_true() : __kip_false();"
  , "var küçük_eşitlik = (a, b) => __kip_num(a) <= __kip_num(b) ? __kip_true() : __kip_false();"
  , "var büyüklük = (a, b) => __kip_num(a) > __kip_num(b) ? __kip_true() : __kip_false();"
  , "var büyük_eşitlik = (a, b) => __kip_num(a) >= __kip_num(b) ? __kip_true() : __kip_false();"
  , "var dizge_hal = (n) => String(__kip_num(n));"
  , "var tam_sayı_hal = (s) => { const n = parseInt(s, 10); return isNaN(n) ? __kip_none() : __kip_some(n); };"
  , "var ondalık_sayı_hal = (s) => { if (typeof s === 'number') return __kip_float(s * 1.0); const n = parseFloat(s); return isNaN(n) ? __kip_none() : __kip_some(__kip_float(n)); };"
  , "var __kip_call = async (fn, args) => {"
  , "  if (typeof fn !== 'function') {"
  , "    throw new TypeError('Attempted to call a non-function');"
  , "  }"
  , "  if (args.length === 0) return await fn();"
  , "  if (fn.length > 0 && args.length < fn.length) {"
  , "    return (...rest) => __kip_call(fn, args.concat(rest));"
  , "  }"
  , "  if (fn.length > 0 && args.length > fn.length) {"
  , "    const head = args.slice(0, fn.length);"
  , "    const tail = args.slice(fn.length);"
  , "    const out = await fn(...head);"
  , "    return await __kip_call(out, tail);"
  , "  }"
  , "  return await fn(...args);"
  , "};"
  ]

-- | Wrappers for overloaded functions that dispatch based on argument type.
-- These are emitted at the END of the output, after all library code.
-- They capture the library implementations (if any) and create unified functions.
-- I/O functions are async to support interactive browser input.
jsOverloadWrappers :: Text
jsOverloadWrappers = T.unlines
  [ "// Overload wrappers - dispatch to library or primitive based on type"
  , "var __kip_lib_ters = typeof ters === 'function' ? ters : null;"
  , "var __kip_lib_birleşim = typeof birleşim === 'function' ? birleşim : null;"
  , "var __kip_lib_uzunluk = typeof uzunluk === 'function' ? uzunluk : null;"
  , "var __kip_lib_toplam = typeof toplam === 'function' ? toplam : null;"
  , "var __kip_lib_fark = typeof fark === 'function' ? fark : null;"
  , "var __kip_lib_yaz = typeof yaz === 'function' ? yaz : null;"
  , ""
  , "var ters = async (x) => {"
  , "  if (typeof x === 'string') return __kip_prim_ters(x);"
  , "  if (__kip_lib_ters) return await __kip_lib_ters(x);"
  , "  throw new Error('ters: unsupported type');"
  , "};"
  , ""
  , "var birleşim = async (a, b) => {"
  , "  if (typeof a === 'string' || typeof a === 'number') return __kip_prim_birleşim(a, b);"
  , "  if (__kip_lib_birleşim) return await __kip_lib_birleşim(a, b);"
  , "  throw new Error('birleşim: unsupported type');"
  , "};"
  , ""
  , "var uzunluk = async (x) => {"
  , "  if (typeof x === 'string') return __kip_prim_uzunluk(x);"
  , "  if (__kip_lib_uzunluk) return await __kip_lib_uzunluk(x);"
  , "  throw new Error('uzunluk: unsupported type');"
  , "};"
  , ""
  , "var toplam = async (...args) => {"
  , "  if (args.length === 2 && typeof args[0] === 'number') return __kip_prim_toplam(args[0], args[1]);"
  , "  if (args.length === 2 && (__kip_is_float(args[0]) || __kip_is_float(args[1]))) return __kip_prim_toplam(args[0], args[1]);"
  , "  if (args.length === 1 && __kip_lib_toplam) return await __kip_lib_toplam(args[0]);"
  , "  if (__kip_lib_toplam) return await __kip_lib_toplam(...args);"
  , "  return __kip_prim_toplam(...args);"
  , "};"
  , ""
  , "var fark = async (...args) => {"
  , "  if (args.length === 2 && typeof args[0] === 'number') return __kip_prim_fark(args[0], args[1]);"
  , "  if (args.length === 2 && (__kip_is_float(args[0]) || __kip_is_float(args[1]))) return __kip_prim_fark(args[0], args[1]);"
  , "  if (__kip_lib_fark) return await __kip_lib_fark(...args);"
  , "  return __kip_prim_fark(...args);"
  , "};"
  , ""
  , "// I/O wrappers - async for interactive browser support"
  , "var oku = async (...args) => {"
  , "  if (args.length === 0) return await __kip_prim_oku_stdin();"
  , "  if (args.length === 1 && typeof args[0] === 'string') return __kip_prim_oku_dosya(args[0]);"
  , "  throw new Error('oku: unsupported arguments');"
  , "};"
  , ""
  , "var yaz = (...args) => {"
  , "  if (args.length === 1) {"
  , "    var val = __kip_is_float(args[0]) ? args[0].value : args[0];"
  , "    var output = __kip_is_float(args[0]) && Number.isInteger(val) ? String(val) + '.0' : val;"
  , "    if (__kip_is_browser && typeof window.__kip_write === 'function') {"
  , "      window.__kip_write(output);"
  , "    } else {"
  , "      console.log(output);"
  , "    }"
  , "    return typeof bitimlik === 'function' ? bitimlik() : bitimlik;"
  , "  }"
  , "  if (args.length === 2 && typeof args[0] === 'string') {"
  , "    return __kip_prim_yaz_dosya(args[0], args[1]);"
  , "  }"
  , "  if (__kip_lib_yaz) return __kip_lib_yaz(...args);"
  , "  throw new Error('yaz: unsupported arguments');"
  , "};"
  ]

-- | Generate JavaScript for a list of statements without the runtime prelude.
--
-- This is mainly useful for tests or embedding scenarios where the caller
-- manages prelude/wrapper injection manually.
codegenStmts :: [Stmt Ann] -> Text
codegenStmts stmts =
  let ctx = buildCodegenCtx stmts
      merged = mergeCompatibleFunctions stmts
  in T.intercalate "\n\n" (map (codegenStmtWith ctx) merged)

-- | Generate JavaScript for a single top-level statement.
--
-- Function and type declarations are emitted as declarations, while expression
-- statements are emitted in executable statement form.
codegenStmt :: Stmt Ann -> Text
codegenStmt = codegenStmtWith emptyCodegenCtx

codegenStmtWith :: CodegenCtx -> Stmt Ann -> Text
codegenStmtWith ctx stmt =
  case stmt of
    Defn name _ exp' ->
      "const " <> toJsIdent name <> " = " <> codegenExpWith ctx exp' <> ";"
    Function name args _ clauses _ ->
      renderFunction ctx name args clauses
    PrimFunc name args _ _ ->
      "// primitive function " <> identText name <> "(" <> renderArgNames args <> ")"
    Load name ->
      "// load " <> identText name
    NewType name _ ctors ->
      renderNewType name ctors
    PrimType name ->
      "// primitive type " <> identText name
    ExpStmt exp' ->
      codegenExpWith ctx exp' <> ";"

-- | Generate JavaScript for an expression.
--
-- The result is always expression-shaped so it can be embedded in larger
-- contexts. Sequencing and local bindings are lowered through async IIFEs.
codegenExp :: Exp Ann -> Text
codegenExp = codegenExpWith emptyCodegenCtx

codegenExpWith :: CodegenCtx -> Exp Ann -> Text
codegenExpWith ctx exp' =
  case exp' of
    Var {varName, varCandidates} ->
      case varCandidates of
        ((ident, _):_) -> toJsIdent ident
        [] -> toJsIdent varName
    StrLit {lit} ->
      renderString lit
    IntLit {intVal} ->
      T.pack (show intVal)
    FloatLit {floatVal} ->
      "__kip_float(" <> T.pack (show floatVal) <> ")"
    App {fn, args} ->
      renderCall ctx fn args
    Bind {bindName, bindExp}
      | expMentions bindName bindExp ->
          let tmp = "__kip_shadow_" <> toJsIdent bindName
          in renderIife
               [ "const " <> tmp <> " = " <> codegenExpWith ctx bindExp <> ";"
               , "const " <> toJsIdent bindName <> " = " <> tmp <> ";"
               , "return " <> toJsIdent bindName <> ";"
               ]
      | otherwise ->
          renderIife
            [ "const " <> toJsIdent bindName <> " = " <> codegenExpWith ctx bindExp <> ";"
            , "return " <> toJsIdent bindName <> ";"
            ]
    Seq {first = Bind {bindName, bindExp}, second}
      | expMentions bindName bindExp ->
          -- When a Bind shadows a name used in its own initializer, pass the
          -- new value as an IIFE parameter so the initializer sees the outer
          -- binding and the body sees the shadowed one.
          let paramName = toJsIdent bindName
          in "(await (async (" <> paramName <> ") => { return " <> codegenExpWith ctx second <> "; })(" <> codegenExpWith ctx bindExp <> "))"
    Seq {first, second} ->
      renderIife
        (renderExpAsStmt ctx first ++ ["return " <> codegenExpWith ctx second <> ";"])
    Match {scrutinee, clauses} ->
      renderMatch ctx scrutinee clauses
    Let {body} ->
      codegenExpWith ctx body
    Ascribe {ascExp} ->
      codegenExpWith ctx ascExp

-- | Render a Kip function definition as an async JS function.
--
-- Generated functions are always async so all call sites can uniformly use
-- @await@, even for logically pure functions.
--
-- Single wildcard clauses are emitted as direct returns; multi-clause bodies
-- are lowered through the pattern-matching chain renderer.
renderFunction :: CodegenCtx -> Identifier -> [Arg Ann] -> [Clause Ann] -> Text
renderFunction ctx name args clauses =
  renderFunctionNamed ctx (toJsIdent name) args clauses

-- | Render a Kip function using an explicit JS function name.
renderFunctionNamed :: CodegenCtx -> Text -> [Arg Ann] -> [Clause Ann] -> Text
renderFunctionNamed ctx jsName args clauses =
  let argsText = renderArgNames args
      bodyLines =
        case clauses of
          [Clause (PWildcard _) body] ->
            ["return " <> codegenExpWith ctx body <> ";"]
          _ ->
            let arg0 = case args of
                         [] -> "__arg0"
                         (((argName, _), _) : _) -> toJsIdent argName
            in ("const __scrut = " <> arg0 <> ";")
               : renderClauseChain ctx "__scrut" clauses
  in
    T.unlines
      [ "async function " <> jsName <> "(" <> argsText <> ") {"
      , indent 2 (T.unlines bodyLines)
      , "}"
      ]

-- | Clause-chain indirection.
--
-- This alias keeps call sites stable if we later switch the lowering strategy.
renderClauseChain :: CodegenCtx -> Text -> [Clause Ann] -> [Text]
renderClauseChain = renderClauseIfChain

-- | Render an if/else chain for ordered clause matching.
renderClauseIfChain :: CodegenCtx -> Text -> [Clause Ann] -> [Text]
renderClauseIfChain ctx scrutinee =
  go True
  where
    go _ [] =
      ["throw new Error(\"No match\");"]
    go isFirst (Clause pat body : rest) =
      let (cond, binds) = renderPatMatchCond ctx scrutinee pat
          bodyLines = binds ++ ["return " <> codegenExpWith ctx body <> ";"]
          header =
            if cond == ""
              then if isFirst then "{" else "else {"
              else (if isFirst then "if (" else "else if (") <> cond <> ") {"
          block =
            [ header
            , indent 2 (T.unlines bodyLines)
            , "}"
            ]
      in if cond == ""
           then block
           else block ++ go False rest

-- | Render variable bindings implied by a pattern.
--
-- Returns:
--
-- * emitted binding statements
-- * next argument index
--
-- Bindings are right-aligned with constructor arguments to match Kip's pattern
-- semantics for nested constructor/list patterns.
renderPatternBindings :: Text -> [Pat Ann] -> Int -> ([Text], Int)
renderPatternBindings scrutinee pats startIdx =
  -- Note: constructor arguments in Kip patterns are matched from the right.
  -- We keep patLen around so we can index from the end of scrutinee.args,
  -- which mirrors how nested patterns are aligned in the AST/typechecker.
  let patLen = length pats
      (binds, idx, _) = foldl (collect patLen) ([], startIdx, []) pats
  in (binds, idx)
  where
    collect patLen (acc, idx, seen) pat =
      let (binds, nextIdx, seen') = renderPatBinding scrutinee patLen idx seen pat
      in (acc ++ binds, nextIdx, seen')

    -- Bind variables by walking the pattern while keeping alignment consistent
    -- with right-anchored constructor arguments.
    renderPatBinding scrut patLen idx seen pat =
      case pat of
        PWildcard _ -> ([], idx + 1, seen)
        PVar n _ ->
          let name = toJsIdent n
              argAccess = patArgAccess scrut patLen idx
          in if name `elem` seen
               then ([], idx + 1, seen)
               else ([ "const " <> name <> " = " <> argAccess <> ";" ], idx + 1, name : seen)
        PCtor _ subPats ->
          let argAccess = patArgAccess scrut patLen idx
              (subBinds, _, seen') = renderPatternBindingsWithSeen argAccess subPats 0 seen
          in (subBinds, idx + 1, seen')
        PIntLit _ _ -> ([], idx + 1, seen)
        PFloatLit _ _ -> ([], idx + 1, seen)
        PStrLit _ _ -> ([], idx + 1, seen)
        PListLit _ -> ([], idx + 1, seen)

    renderPatternBindingsWithSeen scrut pats idx seen =
      -- Each nested constructor has its own argument list length, so we
      -- recompute patLen for the subpattern list.
      let patLen = length pats
      in foldl (collectWithSeen patLen) ([], idx, seen) pats
      where
        collectWithSeen patLen (acc, ix, seenAcc) p =
          let (binds, nextIx, seen') = renderPatBinding scrut patLen ix seenAcc p
          in (acc ++ binds, nextIx, seen')

-- | Lower a Kip @Match@ expression into an async IIFE expression.
--
-- The scrutinee is evaluated once and captured in @__scrut@ to avoid repeated
-- evaluation and preserve side-effect ordering.
renderMatch :: CodegenCtx -> Exp Ann -> [Clause Ann] -> Text
renderMatch ctx scrutinee clauses =
  renderIife $
    ("const __scrut = " <> codegenExpWith ctx scrutinee <> ";")
      : renderMatchClauses ctx "__scrut" clauses

-- | Render match clauses with the same ordered semantics as function clauses.
renderMatchClauses :: CodegenCtx -> Text -> [Clause Ann] -> [Text]
renderMatchClauses = renderClauseIfChain

-- | Compatibility alias for pattern condition + bindings rendering.
renderPatMatch :: CodegenCtx -> Text -> Pat Ann -> (Text, [Text])
renderPatMatch = renderPatMatchCond

-- | Render both the boolean guard and binding statements for a pattern.
--
-- The guard determines whether the branch matches; bindings are emitted only
-- for variables appearing in that branch.
renderPatMatchCond :: CodegenCtx -> Text -> Pat Ann -> (Text, [Text])
renderPatMatchCond ctx scrutinee pat =
  case pat of
    PWildcard _ -> ("", [])
    PVar n _ -> ("", ["const " <> toJsIdent n <> " = " <> scrutinee <> ";"])
    PCtor (ctor, _) pats ->
      let cond = renderPatCond ctx scrutinee pat
          (binds, _) = renderPatternBindings scrutinee pats 0
      in (cond, binds)
    PIntLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PFloatLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PStrLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PListLit _ -> (renderPatCond ctx scrutinee pat, [])

-- | Render a JavaScript boolean condition for a pattern.
--
-- Constructors are matched by tag and minimum arity; literal patterns are
-- matched by JS equality against the lowered scrutinee.
renderPatCond :: CodegenCtx -> Text -> Pat Ann -> Text
renderPatCond ctx scrutinee pat =
  case pat of
    PWildcard _ -> "true"
    PVar _ _ -> "true"
    PCtor (ctor, _) pats ->
      -- When matching constructor patterns we need to:
      -- 1) check the tag, 2) ensure args are long enough, and
      -- 3) evaluate subpattern guards using right-aligned indexing.
      let patLen = length pats
          headCond = renderCtorTagCond scrutinee ctor
          lenCond =
            if patLen > 0
              then scrutinee <> ".args.length >= " <> T.pack (show patLen)
              else "true"
          argConds =
            [ cond
            | (p, idx) <- zip pats [0 ..]
            , let cond = renderPatCond ctx (patArgAccess scrutinee patLen idx) p
            , cond /= "true"
            ]
      in T.intercalate " && " (headCond : lenCond : argConds)
    PIntLit n _ ->
      scrutinee <> " === " <> T.pack (show n)
    PFloatLit n _ ->
      "__kip_num(" <> scrutinee <> ") === " <> T.pack (show n)
    PStrLit s _ ->
      scrutinee <> " === " <> renderString s
    PListLit pats ->
      renderListPatCond ctx scrutinee pats

-- | Render a boolean condition for a list pattern.
--
-- Kip lists are represented as nested @eki(head, tail)@ constructors ending in
-- @boş@, so this function recursively emits constructor checks for that shape.
renderListPatCond :: CodegenCtx -> Text -> [Pat Ann] -> Text
renderListPatCond _ scrutinee [] =
  -- Empty list pattern matches 'boş'
  scrutinee <> ".tag === \"boş\""
renderListPatCond ctx scrutinee (p:ps) =
  -- Non-empty list pattern matches 'eki' with head and tail
  let headCond = scrutinee <> ".tag === \"eki\""
      lenCond = scrutinee <> ".args.length >= 2"
      headMatch = renderPatCond ctx (scrutinee <> ".args[0]") p
      tailMatch = renderListPatCond ctx (scrutinee <> ".args[1]") ps
      conds = filter (/= "true") [headCond, lenCond, headMatch, tailMatch]
  in if null conds
       then "true"
       else T.intercalate " && " conds

-- | Access a constructor argument using right-aligned pattern indexing.
--
-- If @patLen == 2@ and @idx == 0@, this points to the first element of the
-- last two constructor arguments.
patArgAccess :: Text -> Int -> Int -> Text
patArgAccess scrutinee patLen idx =
  let idxText = T.pack (show idx)
      lenText = T.pack (show patLen)
      -- With right alignment, idx=0 means the *first* pattern argument
      -- matches the *leftmost* of the last patLen elements.
  in if patLen <= 0
       then scrutinee <> ".args[" <> idxText <> "]"
       else scrutinee <> ".args[(" <> scrutinee <> ".args.length - " <> lenText <> " + " <> idxText <> ")]"

-- | Normalize constructor names for runtime tag comparison.
--
-- Some constructor surfaces differ only by Turkish soft-g possessive alternation
-- (for example @varlığı@ vs @varlık@). We accept both spellings to keep
-- pattern matching compatible with primitive option helpers.
renderCtorTagCond :: Text -> Identifier -> Text
renderCtorTagCond scrutinee (mods, name) =
  let exact = toJsIdent (mods, name)
      softened = toJsIdent (mods, stripSoftGPossessive name)
      exactCond = scrutinee <> ".tag === " <> renderString exact
      softCond = scrutinee <> ".tag === " <> renderString softened
  in if exact == softened
       then exactCond
       else "(" <> exactCond <> " || " <> softCond <> ")"

-- | Strip Turkish possessive suffix only when preceded by soft-g.
--
-- Converts final @ğı/ği/ğu/ğü@ back to @k@.
stripSoftGPossessive :: Text -> Text
stripSoftGPossessive txt =
  case T.unsnoc txt of
    Just (pref, c)
      | c `elem` ("ıiuü" :: String) ->
          case T.unsnoc pref of
            Just (pref', 'ğ') -> pref' <> "k"
            _ -> txt
    _ -> txt

-- | Emit JavaScript for a Kip ADT declaration.
--
-- Constructors are emitted as tagged value/factory bindings. For single nullary
-- constructors we additionally alias the type name to the constructor.
renderNewType :: Identifier -> [Ctor Ann] -> Text
renderNewType name ctors =
  let ctorLines =
        [ renderCtor ctorName args
        | ((ctorName, _), args) <- ctors
        ]
      ctorSig =
        T.intercalate " | "
          [ identText ctorName <> "(" <> T.replicate (length args) "_" <> ")"
          | ((ctorName, _), args) <- ctors
          ]
      -- For single-constructor types with no args, also alias the type name
      -- to the constructor (e.g., bitim = bitimlik for unit types)
      typeAlias = case ctors of
        [((ctorName, _), [])] | identText name /= identText ctorName ->
          ["var " <> toJsIdent name <> " = " <> toJsIdent ctorName <> ";"]
        _ -> []
  in
    T.unlines $
      ("/* type " <> identText name <> " = " <> ctorSig <> " */")
        : ctorLines ++ typeAlias

-- | Render a single constructor.
-- Zero-argument constructors are defined as objects (values).
-- Constructors with arguments are defined as functions.
-- Uses toJsIdent for both JS variable name and tag to ensure consistency.
renderCtor :: Identifier -> [a] -> Text
renderCtor ctorName args =
  let jsName = toJsIdent ctorName
  in case args of
    [] -> "var " <> jsName <> " = { tag: "
            <> renderString jsName <> ", args: [] };"
    _ -> "var " <> jsName <> " = (...args) => ({ tag: "
            <> renderString jsName <> ", args });"

-- | Lower function application.
--
-- Most calls become @(await f(...))@. Section-like partial forms are handled
-- by 'partialSectionCall' to preserve Kip semantics.
renderCall :: CodegenCtx -> Exp Ann -> [Exp Ann] -> Text
renderCall ctx fn args =
  let fnText =
        case fn of
          Var {} -> codegenExpWith ctx fn
          _ -> "(" <> codegenExpWith ctx fn <> ")"
      argsCsv = T.intercalate ", " (map (codegenExpWith ctx) args)
  in case partialSectionCall ctx fn fnText args of
       Just sectionFn -> sectionFn
       Nothing -> "(await " <> fnText <> "(" <> argsCsv <> "))"

-- | Render a single-argument partial application that should become a section.
--
-- Section lowering is enabled only for function names proven section-capable in
-- 'buildCodegenCtx' (has an arity > 1 overload and no unary overload).
partialSectionCall :: CodegenCtx -> Exp Ann -> Text -> [Exp Ann] -> Maybe Text
partialSectionCall ctx fn fnText args =
  case (fn, args) of
    (Var {annExp = annFn, varCandidates}, [arg])
      | annCase annFn == Ins && isSectionableCall ctx varCandidates ->
          Just (renderCaseDrivenSection ctx fnText arg)
    _ -> Nothing

-- | Render a generic case-driven section for binary calls.
--
-- Instrumental fixed args are treated as left sections; all other fixed-case
-- args are treated as right sections.
renderCaseDrivenSection :: CodegenCtx -> Text -> Exp Ann -> Text
renderCaseDrivenSection ctx fnText arg =
  if annCase (annExp arg) == Ins
    then "(async (__kip_arg0) => (await __kip_call(" <> fnText <> ", [" <> codegenExpWith ctx arg <> ", __kip_arg0])))"
    else "(async (__kip_arg0) => (await __kip_call(" <> fnText <> ", [__kip_arg0, " <> codegenExpWith ctx arg <> "])))"

isSectionableCall :: CodegenCtx -> [(Identifier, Case)] -> Bool
isSectionableCall ctx =
  any (\(ident, _) -> ident `Set.member` sectionableFns ctx)

-- | Check whether an identifier is mentioned anywhere in an expression.
--
-- This is used to detect self-referencing bindings that would hit JavaScript's
-- temporal dead zone (TDZ) when emitted as @const x = ...x...@.
expMentions :: Identifier -> Exp Ann -> Bool
expMentions name expr =
  let jsName = toJsIdent name
  in expMentionsJs jsName expr

-- | Check whether a JS identifier name appears in a compiled expression.
expMentionsJs :: Text -> Exp Ann -> Bool
expMentionsJs jsName expr =
  case expr of
    Var {varName, varCandidates} ->
      let emitted = case varCandidates of
                      ((ident, _):_) -> toJsIdent ident
                      [] -> toJsIdent varName
      in emitted == jsName
    App {fn, args} -> expMentionsJs jsName fn || any (expMentionsJs jsName) args
    Bind {bindExp} -> expMentionsJs jsName bindExp
    Seq {first, second} -> expMentionsJs jsName first || expMentionsJs jsName second
    Match {scrutinee, clauses} ->
      expMentionsJs jsName scrutinee || any (\(Clause _ body) -> expMentionsJs jsName body) clauses
    Let {body} -> expMentionsJs jsName body
    Ascribe {ascExp} -> expMentionsJs jsName ascExp
    StrLit {} -> False
    IntLit {} -> False
    FloatLit {} -> False

-- | Render an expression in statement position.
--
-- Bindings become declarations; all other expressions become single statements.
-- When a binding's name appears in its own initializer, a temporary variable is
-- used to avoid JavaScript's temporal dead zone.
renderExpAsStmt :: CodegenCtx -> Exp Ann -> [Text]
renderExpAsStmt ctx exp' =
  case exp' of
    Bind {bindName, bindExp} ->
      ["const " <> toJsIdent bindName <> " = " <> codegenExpWith ctx bindExp <> ";"]
    _ ->
      [codegenExpWith ctx exp' <> ";"]

-- | Wrap a list of statements in an async IIFE expression.
renderIife :: [Text] -> Text
renderIife lines' =
  T.unlines
    [ "(await (async () => {"
    , indent 2 (T.unlines lines')
    , "})())"
    ]

-- | Render comma-separated JavaScript argument names.
renderArgNames :: [Arg Ann] -> Text
renderArgNames args =
  T.intercalate ", " (map (toJsIdent . argIdent) args)

-- | Extract the terminal segment of an identifier.
identText :: Identifier -> Text
identText (_, name) = name

-- | Convert a Kip identifier into a JavaScript-safe identifier.
--
-- Namespace pieces are joined with underscores; dashes are rewritten to
-- underscores; apostrophes are removed; reserved words are prefixed.
toJsIdent :: Identifier -> Text
toJsIdent ident =
  let raw = baseIdent ident
      sanitized = T.map replaceDash raw
      cleaned = T.filter (/= '\'') sanitized
      prefixed =
        case T.uncons cleaned of
          Nothing -> "_"
          Just (c, _) ->
            if isIdentStart c then cleaned else "_" <> cleaned
      safe =
        if prefixed `elem` jsReserved
          then "_" <> prefixed
          else prefixed
  in safe
  where
    baseIdent (ns, name) =
      let cleanName = T.filter (/= ' ') name
      in case ns of
           [] -> cleanName
           _  -> T.intercalate "_" (map (T.filter (/= ' ')) ns ++ [cleanName])
    replaceDash c = if c == '-' then '_' else c
    isIdentStart c = isLetter c || c == '_' || c == '$'

-- | JavaScript reserved words blocked from raw identifier emission.
jsReserved :: [Text]
jsReserved =
  [ "break", "case", "catch", "class", "const", "continue", "debugger"
  , "default", "delete", "do", "else", "export", "extends", "false"
  , "finally", "for", "function", "if", "import", "in", "instanceof"
  , "new", "null", "return", "super", "switch", "this", "throw"
  , "true", "try", "typeof", "var", "void", "while", "with", "yield"
  , "let", "enum", "await", "implements", "interface", "package"
  , "private", "protected", "public", "static", "undefined"
  ]

-- | Render a JavaScript string literal with escaping.
renderString :: Text -> Text
renderString txt =
  "\"" <> T.concatMap escapeChar txt <> "\""

-- | Escape one character in JS string context.
escapeChar :: Char -> Text
escapeChar c =
  case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c

-- | Indent each non-empty line by @n@ spaces.
indent :: Int -> Text -> Text
indent n =
  T.unlines . map (T.replicate n " " <>) . filter (not . T.null) . T.lines
