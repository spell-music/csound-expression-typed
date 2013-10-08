module Csound.Typed.Control.SERef where

import Control.Applicative
import Control.Monad

import Csound.Dynamic
import Csound.Dynamic.Control

import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState

data SERef a = SERef 
    { writeSERef :: a -> SE ()
    , readSERef  :: SE a }

newSERef :: Tuple a => a -> SE (SERef a)
newSERef a = return $ SERef writeRef readRef        
    where 
        writeRef x = fromDep_ $ liftA2 (zipWithM_ writeVar) vars (fromTuple x)
        readRef    = fmap toTuple $ fromDep $ fmap (mapM readVar) vars

        vars = do
            initVals <- fromTuple a
            onLocals $ zipWithM newLocalVar (tupleRates a) initVals

sensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    ref <- newSERef a
    return $ (readSERef ref, writeSERef ref)

