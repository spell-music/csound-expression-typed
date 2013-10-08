module Csound.Typed.GlobalState.SE(
    SE(..), runSE, evalSE, fromDep, fromDep_        
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict

import Csound.Dynamic
import Csound.Typed.GlobalState.GE

newtype SE a = SE { unSE :: State (Maybe (GE E)) a }

instance Functor SE where
    fmap f = SE . fmap f . unSE

instance Applicative SE where
    pure = return
    (<*>) = ap

instance Monad SE where
    return = SE . return
    ma >>= mf = SE $ unSE ma >>= unSE . mf

runSE :: SE a -> (a, Maybe (GE E))
runSE a = runState (unSE a) Nothing

fromDep :: GE (Dep a) -> SE (GE a)
fromDep ma = SE $ state $ \s -> 
    case s of
        Nothing -> 
            let pairGE = do
                    a <- ma
                    return $ runState (unDep a) Nothing
            in (fmap fst pairGE, Just $ fmap (maybe emptyE id . snd) pairGE)
        Just mx ->
            let pairGE = do
                    a <- ma
                    x <- mx
                    return $ runState (unDep a) (Just x)
            in (fmap fst pairGE, Just $ fmap (maybe emptyE id . snd) pairGE)

fromDep_ :: GE (Dep ()) -> SE ()
fromDep_ = fmap (const ()) . fromDep
            
evalSE :: SE a -> a
evalSE = fst . runSE


