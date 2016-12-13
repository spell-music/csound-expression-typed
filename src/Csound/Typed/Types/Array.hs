{-# Language FlexibleInstances #-}
module Csound.Typed.Types.Array(
    Arr(..), 
    newLocalArr, newGlobalArr, newLocalCtrlArr, newGlobalCtrlArr,
    readArr, writeArr, modifyArr, mixArr,
    -- * Misc functions to help the type inverence
    Arr1, DArr1, Arr2, DArr2, Arr3, DArr3,
    arr1, darr1, arr2, darr2, arr3, darr3,

    -- * Array opcodes
    fillLocalArrayNew, fillGlobalArrayNew, fillLocalCtrlArrayNew, fillGlobalCtrlArrayNew,
    maparrayNew, lenarray, copyf2array, copya2ftab, minarray, maxarray, sumarray, 
    scalearray, slicearrayNew,

    fillArrayCopy, maparrayCopy, slicearrayCopy,

    -- * Spectral opcodes
    SpecArr, 

    fftNew, fftinvNew, rfftNew, rifftNew, pvs2tab, tab2pvs, cmplxprodNew, 
    rect2polNew, pol2rectNew, pol2rect2New, windowArrayNew, 
    r2cNew, c2rNew, magsArrayNew, phsArrayNew,

    fftCopy, fftinvCopy, rfftCopy, rifftCopy, cmplxprodCopy, 
    rect2polCopy, pol2rectCopy, pol2rect2Copy, windowArrayCopy, 
    r2cCopy, c2rCopy, magsArrayCopy, phsArrayCopy
) where


import Control.Monad
import Control.Monad.Trans.Class

import Csound.Dynamic hiding (writeArr, readArr, newLocalArrVar, newTmpArrVar)
import qualified Csound.Dynamic as D

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.GE

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

-----------------------------------------------------
-- opcodes with array allocation

mulArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
mulArrayNew = binOp "*"

addArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
addArrayNew = binOp "+"

subArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
subArrayNew = binOp "-"

divArrayNew :: (Tuple b, Num b) => Arr a b -> Arr a b -> SE (Arr a b)
divArrayNew = binOp "/"

fillLocalArrayNew  :: [D] -> SE (Arr Sig Sig)
fillLocalArrayNew = fillArrayBy Ar newLocalArrVar

fillGlobalArrayNew :: [D] -> SE (Arr Sig Sig)
fillGlobalArrayNew = fillArrayBy Ar newGlobalArrVar

fillLocalCtrlArrayNew  :: [D] -> SE (Arr Sig Sig)
fillLocalCtrlArrayNew = fillArrayBy Kr newLocalArrVar

fillGlobalCtrlArrayNew :: [D] -> SE (Arr Sig Sig)
fillGlobalCtrlArrayNew = fillArrayBy Kr newGlobalArrVar

fillArrayBy :: Rate -> (Rate -> GE [E] -> SE Var) -> [D] -> SE (Arr Sig Sig)
fillArrayBy rate mkVar inits = SE $ fmap Arr $ hideGEinDep $ do        
    initExp <- mapM toGE inits
    return $ do
        outVar <- unSE $ newTmpArrVar rate    
        opcsArr isArrayInit outVar "fillarray" [(rate, replicate (length inits) Ir)] initExp
        return [outVar]

lenarray :: Arr a b -> D
lenarray (Arr vs) = fromGE $ return $ f (inlineVar $ head vs)
    where f a = opcs "lenarray" [(Ir, [Xr])] [a]

copyf2array :: Arr Sig Sig -> Tab -> SE ()
copyf2array (Arr vs) t = SE $ hideGEinDep $ do
    tabExp <- toGE t
    return $ depT_ $ opcs "copyf2array" [(Xr, [varRate $ head vs, Ir])] [inlineVar $ head vs, tabExp]

copya2ftab :: Arr Sig Sig -> Tab -> SE ()
copya2ftab (Arr vs) t = SE $ hideGEinDep $ do
    tabExp <- toGE t
    return $ depT_ $ opcs "copya2ftab" [(Xr, [varRate $ head vs, Ir])] [inlineVar $ head vs, tabExp]


maparrayNew :: Arr a b -> Str -> SE (Arr a b)
maparrayNew (Arr vs) str = SE $ fmap Arr $ hideGEinDep $ do
    strExp <- toGE str    
    return $ mapM (\var -> go var strExp) vs    
    where
        go var strExp = do
            outVar <- unSE $ newTmpArrVar (varRate var)
            opcsArr isArrayInit outVar "slicearray" idRate [inlineVar var, strExp]
            return $ outVar

        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

minarray :: (Tuple b, Num b) => Arr a b -> SE b
minarray = extractArray "minarray"

maxarray :: (Tuple b, Num b) => Arr a b -> SE b
maxarray = extractArray "maxarray"

sumarray :: (Tuple b, Num b) => Arr a b -> SE b
sumarray = extractArray "sumarray"

scalearray :: (Tuple b, Num b) => Arr a b -> (b, b) -> SE ()
scalearray (Arr vs) (a, b) = SE $ hideGEinDep $ do
    aExps <- fromTuple a
    bExps <- fromTuple b
    return $ zipWithM_ (\var (aExp, bExp) -> go var (aExp, bExp)) vs (zip aExps bExps)
    where 
        go v (aExp, bExp) = 
            depT_ $ opcs "copyf2array" [(Xr, [varRate $ head vs, Ir])] [inlineVar $ head vs, aExp, bExp]

slicearrayNew :: Arr D a -> (D, D) -> SE (Arr D a)
slicearrayNew (Arr vs) (from, to) = SE $ fmap Arr $ hideGEinDep $ do
    fromExp <- toGE from
    toExp   <- toGE to
    return $ mapM (\var -> go var (fromExp, toExp)) vs    
    where
        go var (from, to) = do
            outVar <- unSE $ newTmpArrVar (varRate var)
            opcsArr isArrayInit outVar "slicearray" idRate [inlineVar var, from, to]
            return $ outVar

        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

-- spectral opcodes

type SpecArr = Arr Sig Sig

fftNew :: SpecArr -> SE SpecArr
fftNew = convert "fft"

fftinvNew :: SpecArr -> SE SpecArr
fftinvNew = convert "fftinvi"

rfftNew :: SpecArr -> SE SpecArr
rfftNew = convert "rfft"

rifftNew :: SpecArr -> SE SpecArr
rifftNew = convert "rifft"

pvs2tab :: SpecArr -> Spec -> SE Sig
pvs2tab = extractWith "pvs2tab" (Kr, [Xr, Fr]) 

tab2pvs :: SpecArr -> SE Spec
tab2pvs = extract1 Fr "tab2pvs"

-- kout[] cmplxprod kin1[], kin2[]
cmplxprodNew :: SpecArr -> SpecArr -> SE SpecArr
cmplxprodNew = convert2 "cmplxprod"

-- kout[] rect2pol kin[]
rect2polNew :: SpecArr -> SE SpecArr
rect2polNew = convert "rect2pol"

-- kout[] pol2rect kin[]
-- kout[] pol2rect kmags[], kphs[]
pol2rectNew :: SpecArr -> SE SpecArr
pol2rectNew = convert "pol2rect"

pol2rect2New :: SpecArr -> SpecArr -> SE SpecArr
pol2rect2New = convert2 "pol2rect2"

-- kout[] window kin[][, koff, itype]
windowArrayNew :: SpecArr -> SE SpecArr
windowArrayNew = convert "window"

-- kout[] r2c kin[]
r2cNew :: SpecArr -> SE SpecArr
r2cNew = convert "r2c"

-- kout[] c2r kin[]
c2rNew :: SpecArr -> SE SpecArr
c2rNew = convert "c2r"

magsArrayNew :: SpecArr -> SE SpecArr
magsArrayNew = convert "mags"

-- kout[] phs kin[]
phsArrayNew :: SpecArr -> SE SpecArr
phsArrayNew = convert "phs"

-----------------------------

isArrayInit = True
noArrayInit = False

binOp :: String -> Arr a b -> Arr a b -> SE (Arr a b)
binOp name (Arr xs) (Arr ys) = fmap Arr $ zipWithM go xs ys
    where
        go x y = SE $ do
            outVar <- unSE $ newTmpArrVar (varRate x)
            infOprArr isArrayInit outVar name (inlineVar x) (inlineVar y)
            return outVar

convert :: String -> Arr a b -> SE (Arr a b)
convert name (Arr vars) = fmap Arr $ mapM go vars
    where
        go v = SE $ do
            outVar <- unSE $ newTmpArrVar (varRate v)
            opcsArr isArrayInit outVar name idRate1 [inlineVar v]
            return outVar

        idRate1 = fmap (\r -> (r, [r])) [Kr, Ar, Ir, Sr, Fr]

convert2 :: String -> Arr a b -> Arr a b -> SE (Arr a b)
convert2 name (Arr xs) (Arr ys) = fmap Arr $ zipWithM go xs ys
    where
        go x y = SE $ do
            outVar <- unSE $ newTmpArrVar (varRate x)
            opcsArr isArrayInit outVar name idRate2 [inlineVar x, inlineVar y]
            return outVar

        idRate2 = fmap (\r -> (r, [r, r])) [Kr, Ar, Ir, Sr, Fr]

extractArray :: (Tuple b) => String -> Arr a b -> SE b
extractArray name (Arr vs) = SE $ fmap (toTuple . return) $ mapM (f . inlineVar) vs
    where f a = depT $ opcs name [(Xr, [Xr])] [a]

extract1 :: (Tuple b, Tuple c) => Rate -> String -> Arr a b -> SE c
extract1 rate name (Arr vs) = SE $ fmap (toTuple . return) $ mapM (f . inlineVar) vs
    where f a = depT $ opcs name [(rate, [Xr])] [a]

extractWith :: (Tuple b, Tuple c, Tuple d) => String -> (Rate, [Rate]) -> Arr a b -> c -> SE d
extractWith name rates (Arr vs) arg = SE $ fmap (toTuple . return) $ hideGEinDep $ do
        argExps <- fromTuple arg        
        return $ zipWithM (\var x -> f (inlineVar var) x) vs argExps
    where f a b = depT $ opcs name [rates] [a, b]

---------------------------------------------------
-- opcodes with copy

fillArrayCopy :: [D] -> Arr Sig Sig -> SE ()
fillArrayCopy inits (Arr outs) = SE $ hideGEinDep $ do
    initExp <- mapM toGE inits
    return $ opcsArr noArrayInit outVar "fillarray" [(varRate outVar, replicate (length inits) Ir)] initExp
    where outVar = head outs

maparrayCopy :: Arr a b -> Str -> Arr a b -> SE ()
maparrayCopy (Arr vs) str (Arr outs) = SE $ hideGEinDep $ do
    strExp <- toGE str    
    return $ zipWithM_ (\var outVar -> go var strExp outVar) vs outs   
    where
        go var strExp outVar = opcsArr noArrayInit outVar "slicearray" idRate [inlineVar var, strExp]
        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

slicearrayCopy :: Arr D a -> (D, D) -> Arr D a -> SE ()
slicearrayCopy (Arr vs) (from, to) (Arr outs) = SE $ hideGEinDep $ do
    fromExp <- toGE from
    toExp   <- toGE to
    return $ zipWithM_ (\var outVar -> go var (fromExp, toExp) outVar) vs outs   
    where
        go var (from, to) outVar = opcsArr noArrayInit outVar "slicearray" idRate [inlineVar var, from, to]         
        idRate = fmap (\rate -> (rate, [rate, Ir, Ir])) [Ir, Kr, Ar]

mulArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
mulArrayCopy = binOpCopy "*"

addArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
addArrayCopy = binOpCopy "+"

subArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
subArrayCopy = binOpCopy "-"

divArrayCopy :: (Tuple b, Num b) => Arr a b -> Arr a b -> Arr a b -> SE ()
divArrayCopy = binOpCopy "/"

-- spectral opcodes

fftCopy :: SpecArr -> SpecArr -> SE ()
fftCopy = convertCopy "fft"

fftinvCopy :: SpecArr -> SpecArr -> SE ()
fftinvCopy = convertCopy "fftinvi"

rfftCopy :: SpecArr -> SpecArr -> SE ()
rfftCopy = convertCopy "rfft"

rifftCopy :: SpecArr -> SpecArr -> SE ()
rifftCopy = convertCopy "rifft"

-- kout[] cmplxprod kin1[], kin2[]
cmplxprodCopy :: SpecArr -> SpecArr -> SpecArr -> SE ()
cmplxprodCopy = convert2Copy "cmplxprod"

-- kout[] rect2pol kin[]
rect2polCopy :: SpecArr -> SpecArr -> SE ()
rect2polCopy = convertCopy "rect2pol"

-- kout[] pol2rect kin[]
-- kout[] pol2rect kmags[], kphs[]
pol2rectCopy :: SpecArr -> SpecArr -> SE ()
pol2rectCopy = convertCopy "pol2rect"

pol2rect2Copy :: SpecArr -> SpecArr -> SpecArr -> SE ()
pol2rect2Copy = convert2Copy "pol2rect2"

-- kout[] window kin[][, koff, itype]
windowArrayCopy :: SpecArr -> SpecArr -> SE ()
windowArrayCopy = convertCopy "window"

-- kout[] r2c kin[]
r2cCopy :: SpecArr -> SpecArr -> SE ()
r2cCopy = convertCopy "r2c"

-- kout[] c2r kin[]
c2rCopy :: SpecArr -> SpecArr -> SE ()
c2rCopy = convertCopy "c2r"

magsArrayCopy :: SpecArr -> SpecArr -> SE ()
magsArrayCopy = convertCopy "mags"

-- kout[] phs kin[]
phsArrayCopy :: SpecArr -> SpecArr -> SE ()
phsArrayCopy = convertCopy "phs"

---------------------------------------------------------------

binOpCopy :: String -> Arr a b -> Arr a b -> Arr a b -> SE ()
binOpCopy name (Arr xs) (Arr ys) (Arr outs) = mapM_ go $ zip3 xs ys outs
    where
        go (x, y, outVar) = SE $ infOprArr noArrayInit outVar name (inlineVar x) (inlineVar y)

convertCopy :: String -> Arr a b -> Arr a b -> SE ()
convertCopy name (Arr vars) (Arr outs) = zipWithM_ go vars outs
    where
        go v outVar = SE $ opcsArr noArrayInit outVar name idRate1 [inlineVar v]          
        idRate1 = fmap (\r -> (r, [r])) [Kr, Ar, Ir, Sr, Fr]

convert2Copy :: String -> Arr a b -> Arr a b -> Arr a b -> SE ()
convert2Copy name (Arr xs) (Arr ys) (Arr outs) = mapM_ go $ zip3 xs ys outs
    where
        go (x, y, outVar) = SE $ opcsArr noArrayInit outVar name idRate2 [inlineVar x, inlineVar y]
        idRate2 = fmap (\r -> (r, [r, r])) [Kr, Ar, Ir, Sr, Fr]
