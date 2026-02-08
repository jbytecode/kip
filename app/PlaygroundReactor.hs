{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Main
Description : Reactor entrypoint exported to the browser worker.

This executable is built in WASM reactor mode and exports @kip_run@ so the
worker can call into the same WASM instance multiple times.

ABI contract for @kip_run@:

- arg0: mode (@0 = exec@, @1 = codegen-js@)
- arg1: language (@0 = tr@, @1 = en@)
- return: status (@0 = ok@, non-zero = error)

The source file is always expected at @/main.kip@ inside the WASI filesystem
prepared by the worker.
-}
module Main where

import Control.Exception (SomeException, displayException, try)
import Data.Int (Int32)
import qualified Data.Text.IO as TIO
import Foreign.C.Types (CInt(..))
import System.IO (hPutStrLn, stderr)

import Kip.Playground
import Kip.Runner (Lang(..))

-- | Unused in reactor mode; required so Cabal can compile a Main module.
main :: IO ()
main = return ()

-- | Reactor run mode decoded from host-provided integers.
data ReactorMode
  = ReactorExec
  | ReactorCodegenJs
  deriving (Eq, Show)

-- | Decode host mode integer into 'ReactorMode'.
decodeMode :: CInt -> Either String ReactorMode
decodeMode n =
  case n of
    0 -> Right ReactorExec
    1 -> Right ReactorCodegenJs
    _ -> Left ("Unknown reactor mode: " ++ show n)

-- | Decode host language integer into runner language.
decodeLang :: CInt -> Lang
decodeLang n =
  case n of
    1 -> LangEn
    _ -> LangTr

-- | Build a single isolated playground request for one reactor call.
mkRequest :: ReactorMode -> Lang -> PlaygroundRequest
mkRequest mode lang =
  PlaygroundRequest
    { prMode =
        case mode of
          ReactorExec -> PlaygroundExec
          ReactorCodegenJs -> PlaygroundCodegen "js"
    , prFiles = ["/main.kip"]
    , prIncludeDirs = []
    , prLang = lang
    , prNoPrelude = False
    }

{- |
Execute one call from the host.

Each call delegates to 'runPlaygroundRequest', which guarantees fresh runtime
state and therefore prevents user-definition leakage between consecutive calls.
-}
kipRun :: CInt -> CInt -> IO CInt
kipRun modeRaw langRaw =
  case decodeMode modeRaw of
    Left msg -> do
      hPutStrLn stderr msg
      return 2
    Right mode -> do
      let lang = decodeLang langRaw
      result <- try (runPlaygroundRequest (mkRequest mode lang)) :: IO (Either SomeException PlaygroundOutput)
      case result of
        Left err -> do
          hPutStrLn stderr (displayException err)
          return 1
        Right PlaygroundNoOutput ->
          return 0
        Right (PlaygroundTextOutput txt) -> do
          TIO.putStrLn txt
          return 0

foreign export ccall kip_run :: CInt -> CInt -> IO CInt
kip_run :: CInt -> CInt -> IO CInt
kip_run = kipRun
