{-# Language FlexibleInstances #-}
module Csound.Typed.Types.Array(
    Arr(..), 
    newLocalArr, newGlobalArr, 
    readArr, writeArr, modifyArr, mixArr
) where


import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic hiding (writeArr, readArr, newLocalArrVar)

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.GE

import qualified Csound.Dynamic as D

-- | Arrays
newtype Arr ix a = Arr { unArr :: [Var] }

newArrBy :: Tuple a => (Rate -> GE [E] -> SE Var) -> [D] -> [a] -> SE (Arr ix a)
newArrBy mkVar sizes inits = fmap Arr $ mapM (\x -> mkVar x (mapM toGE sizes)) (tupleRates $ proxy inits)
    where 
        proxy :: [a] -> a
        proxy = undefined

newLocalArr :: Tuple a => [D] -> [a] -> SE (Arr ix a)
newLocalArr = newArrBy newLocalArrVar

newGlobalArr :: Tuple a => [D] -> [a] -> SE (Arr ix a)
newGlobalArr = newArrBy newGlobalArrVar


readArr :: (Tuple a, Tuple ix) => Arr ix a -> ix -> SE a
readArr (Arr vars) ixs = fmap (toTuple . return) $ SE $ hideGEinDep $ do
    ixsExp <- fromTuple ixs
    return $ mapM (\v -> read v ixsExp) vars
    where
        read ::  Var -> [E] -> Dep E
        read = D.readArr

writeArr :: (Tuple ix, Tuple a) => Arr ix a -> ix -> a -> SE ()
writeArr (Arr vars) ixs b = SE $ hideGEinDep $ do
    ixsExp <- fromTuple ixs
    bsExp <- fromTuple b
    return $ zipWithM_ (\var value -> write var ixsExp value) vars bsExp
    where
        write ::  Var -> [E] -> E -> Dep ()
        write = D.writeArr

modifyArr :: (Tuple a, Tuple ix) => Arr ix a -> ix -> (a -> a) -> SE ()
modifyArr ref ixs f = do
    value <- readArr ref ixs 
    writeArr ref ixs (f value)

mixArr :: (Tuple ix, Tuple a, Num a) => Arr ix a -> ix -> a -> SE ()
mixArr ref ixs a = modifyArr ref ixs (+ a)
