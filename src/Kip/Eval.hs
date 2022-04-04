module Kip.Eval where

import Kip.AST

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

data EvalState = 
  MkEvalState
    {
    }

emptyEvalState :: EvalState
emptyEvalState = MkEvalState

data EvalError =
   Unknown
 deriving (Show, Eq)

type EvalM = StateT EvalState (ExceptT EvalError IO)

evalExp :: Exp Case -> EvalM (Exp Case)
evalExp e = return e

evalStmt :: Stmt Case -> EvalM ()
evalStmt stmt = 
  case stmt of
    Defn name ty e -> 
      lift (throwE Unknown)
    Function name args ty body ->
      lift (throwE Unknown)
    NewType name ctors ->
      lift (throwE Unknown)
    ExpStmt e -> do
      e' <- evalExp e
      return ()
    Print e -> do
      e' <- evalExp e
      liftIO (putStrLn (prettyExp e'))

replStmt :: Stmt Case -> EvalM ()
replStmt stmt = 
  case stmt of
    ExpStmt e -> do
      e' <- evalExp e
      liftIO (putStrLn "")
      -- liftIO (putStrLn (prettyExp e'))
    _ -> evalStmt stmt

runEvalM :: EvalM a -> EvalState -> IO (Either EvalError (a, EvalState))
runEvalM m s = runExceptT (runStateT m s) 
