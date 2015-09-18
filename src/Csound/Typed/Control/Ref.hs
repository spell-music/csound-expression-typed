module Csound.Typed.Control.Ref where

import Control.DeepSeq(deepseq)

import Control.Monad
import Csound.Dynamic hiding (newLocalVars)

import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState.SE

-- | It describes a reference to mutable values.
newtype Ref a = Ref [Var]
{-
    { writeRef :: a -> SE ()
    , readRef  :: SE a }
-}

writeRef :: Tuple a => Ref a -> a -> SE ()
writeRef (Ref vars) a = fromDep_ $ hideGEinDep $ do
    vals <- fromTuple a
    return $ zipWithM_ writeVar vars vals

--    (zipWithM_ writeVar vars) =<< lift (fromTuple a)
--writeVar :: Var -> E -> Dep ()
--[Var] (GE [E])

readRef  :: Tuple a => Ref a -> SE a
readRef (Ref vars) = SE $ fmap (toTuple . return) $ mapM readVar vars

-- | Allocates a new local (it is visible within the instrument) mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
newRef :: Tuple a => a -> SE (Ref a)
newRef t = fmap Ref $ newLocalVars (tupleRates t) (fromTuple t)    
   
-- | Adds the given signal to the value that is contained in the
-- reference.
mixRef :: (Num a, Tuple a) => Ref a -> a -> SE ()
mixRef ref asig = modifyRef ref (+ asig)

-- | Modifies the Ref value with given function.
modifyRef :: Tuple a => Ref a -> (a -> a) -> SE ()
modifyRef ref f = do
    v <- readRef ref      
    writeRef ref (f v)

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
sensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
sensorsSE a = do
    ref <- newRef a
    return $ (readRef ref, writeRef ref)

-- | Allocates a new global mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
newGlobalRef :: Tuple a => a -> SE (Ref a)
newGlobalRef t = fmap Ref $ newGlobalVars (tupleRates t) (fromTuple t)    

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
globalSensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
globalSensorsSE a = do
    ref <- newRef a
    return $ (readRef ref, writeRef ref)
