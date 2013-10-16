module Csound.Typed.Control.SERef where

import Control.Monad
import Csound.Dynamic

import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState

data SERef a = SERef 
    { writeSERef :: a -> SE ()
    , readSERef  :: SE a }

newSERef :: Tuple a => a -> SE (SERef a)
newSERef t = do
    vars <- newLocalVars (tupleRates t) (fromTuple t)
    let wr a = fromDep_ $ fmap (zipWithM_ writeVar vars) (fromTuple a)
        re   = fmap toTuple $ fromDep  $ return $ mapM readVar vars
    return (SERef wr re)

sensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    ref <- newSERef a
    return $ (readSERef ref, writeSERef ref)

