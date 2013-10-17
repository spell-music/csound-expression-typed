module Csound.Typed.GlobalState.Converters(
    execSG, execSG2        
) where

import Control.Monad.Trans.State.Strict

import Csound.Dynamic
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE

---------------------------------------------------------------
-- magic

genExecSG :: GE (Maybe E) -> SE (GE (Dep ())) -> GE (Dep ())
genExecSG ms0 (SE x) = do
    s0 <- ms0
    (mres, ms) <- runStateT x (LocalHistory s0 0) 
    res  <- mres
    return $ Dep $ put $ snd $ runState (unDep res) (expDependency ms)

execSG :: SE (GE (Dep ())) -> GE (Dep ())
execSG = execSE -- join $ fmap fst $ runSE x

-- genExecSG (return Nothing)

execSG2 :: Dep (SE (GE (Dep ()))) -> GE (Dep ())
execSG2 (Dep x) = genExecSG (return s) y
    where (y, s) = runState x Nothing

