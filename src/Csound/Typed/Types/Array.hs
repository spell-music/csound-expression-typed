{-# Language FlexibleInstances #-}
module Csound.Typed.Types.Array(
    Arr(..), 
    newLocalArr, newGlobalArr, newLocalCtrlArr, newGlobalCtrlArr,
    readArr, writeArr, modifyArr, mixArr,
    -- * Misc functions to help the type inverence
    Arr1, DArr1, Arr2, DArr2, Arr3, DArr3,
    arr1, darr1, arr2, darr2, arr3, darr3
) where


import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic hiding (writeArr, readArr, newLocalArrVar)

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.GE

import qualified Csound.Dynamic as D

type Arr1 a  = Arr Sig a
type DArr1 a = Arr D   a

type Arr2 a  = Arr (Sig, Sig) a
type DArr2 a = Arr (D, D) a

type Arr3 a  = Arr (Sig, Sig, Sig) a
type DArr3 a = Arr (D, D, D) a

arr1  :: SE (Arr Sig a) -> SE (Arr Sig a)
arr1 = id

darr1 :: SE (Arr D   a) -> SE (Arr D   a)
darr1 = id

arr2  :: SE (Arr (Sig,Sig) a) -> SE (Arr (Sig,Sig) a)
arr2 = id

darr2 :: SE (Arr (D,D)   a)   -> SE (Arr (D,D)     a)
darr2 = id

arr3  :: SE (Arr (Sig,Sig,Sig) a) -> SE (Arr (Sig,Sig,Sig) a)
arr3 = id

darr3 :: SE (Arr (D,D,D)   a)     -> SE (Arr (D,D,D)     a)
darr3 = id

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

newLocalCtrlArr :: Tuple a => [D] -> [a] -> SE (Arr ix a)
newLocalCtrlArr = newArrBy $ newLocalArrVar . toCtrlRate

newGlobalCtrlArr :: Tuple a => [D] -> [a] -> SE (Arr ix a)
newGlobalCtrlArr = newArrBy $ newGlobalArrVar . toCtrlRate

toCtrlRate x = case x of 
    Ar -> Kr
    Kr -> Ir
    _  -> x

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
