module Csound.Typed.Control.SERef where

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic hiding (newLocalVars)

import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState

-- | It describes a reference to mutable values.
data SERef a = SERef 
    { writeSERef :: a -> SE ()
    , readSERef  :: SE a }

-- | Allocates a new local (it is visible within the instrument) mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
newSERef :: Tuple a => a -> SE (SERef a)
newSERef t = do
    vars <- newLocalVars (tupleRates t) (fromTuple t)
    let wr a = fromDep_ $ (zipWithM_ writeVar vars) =<< lift (fromTuple a)
        re   = fmap toTuple $ fromDep $ mapM readVar vars
    return (SERef wr re)

-- | An alias for the function @newSERef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
sensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    ref <- newSERef a
    return $ (readSERef ref, writeSERef ref)

-- | Allocates a new global mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
newGlobalSERef :: Tuple a => a -> SE (SERef a)
newGlobalSERef t = do
    vars <- newGlobalVars (tupleRates t) (fromTuple t)
    let wr a = fromDep_ $ (zipWithM_ writeVar vars) =<< lift (fromTuple a)
        re   = fmap toTuple $ fromDep $ mapM readVar vars
    return (SERef wr re)

-- | An alias for the function @newSERef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
globalSensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
globalSensorsSE a = do
    ref <- newSERef a
    return $ (readSERef ref, writeSERef ref)
