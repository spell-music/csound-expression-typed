module Csound.Typed.Types.GlobalState.Converters(
    execSG, execSG2        
) where

import Control.Monad.Trans.State.Strict

import Csound.Dynamic
import Csound.Typed.Types.GlobalState.GE
import Csound.Typed.Types.GlobalState.SE

---------------------------------------------------------------
-- magic

genExecSG :: Maybe (GE E) -> SE (GE (Dep ())) -> GE (Dep ())
genExecSG s0 (SE x) = do
    res  <- mres
    expr <- case ms of
        Nothing -> return Nothing
        Just y  -> fmap Just y
    return $ Dep $ put $ snd $ runState (unDep res) expr
    where 
        (mres, ms) = runState x s0

execSG :: SE (GE (Dep ())) -> GE (Dep ())
execSG = genExecSG Nothing

execSG2 :: Dep (SE (GE (Dep ()))) -> GE (Dep ())
execSG2 (Dep x) = genExecSG (fmap return s) y
    where (y, s) = runState x Nothing

