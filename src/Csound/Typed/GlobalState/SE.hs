module Csound.Typed.GlobalState.SE(
    SE(..), LocalHistory(..), 
    runSE, execSE, evalSE, 
    fromDep, fromDep_, 
    newLocalVar, newLocalVars        
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Default

import Csound.Dynamic
import Csound.Typed.GlobalState.GE

newtype SE a = SE { unSE :: StateT LocalHistory GE a }

data LocalHistory = LocalHistory
    { expDependency :: Maybe E
    , newVarId      :: Int }

instance Default LocalHistory where
    def = LocalHistory def def

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> GE (a, LocalHistory)
runSE a = runStateT (unSE a) def

execSE :: SE a -> GE (Dep ())
execSE = fmap (Dep . put . expDependency . snd) . runSE

fromDep :: GE (Dep a) -> SE (GE a)
fromDep ma = fmap return $ SE $ StateT $ \s -> do
    depA  <- ma
    let (res, dep1) = runState (unDep depA) (expDependency s)
    return (res, s{ expDependency = dep1 })

fromDep_ :: GE (Dep ()) -> SE ()
fromDep_ = fmap (const ()) . fromDep
            
evalSE :: SE a -> GE a
evalSE = fmap fst . runSE

----------------------------------------------------------------------
-- allocation of the local vars

newLocalVars :: [Rate] -> GE [E] -> SE [Var]
newLocalVars rs vs = do
    vars <- mapM newVar rs
    initNewVars vars vs
    return vars

initNewVars :: [Var] -> GE [E] -> SE ()
initNewVars vars vals = fromDep_ $ fmap (zipWithM_ initVar vars) vals

newLocalVar :: Rate -> GE E -> SE Var
newLocalVar rate val = do
    var <- newVar rate
    initNewVar var val
    return var

newVar :: Rate -> SE Var
newVar rate = SE $ do
    s <- get
    let v = Var LocalVar rate (show $ newVarId s)    
    put $ s { newVarId = succ $ newVarId s }
    return v

initNewVar :: Var -> GE E -> SE ()
initNewVar var val = fromDep_ $ fmap (initVar var) val

