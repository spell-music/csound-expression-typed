module Csound.Typed.Control.Ref(
    Ref(..), writeRef, readRef, newRef, mixRef, modifyRef, sensorsSE, newGlobalRef,
    newCtrlRef, newGlobalCtrlRef,
    globalSensorsSE, newClearableGlobalRef, newTab, newGlobalTab,
    -- conditionals
    whileSE
) where

import Data.Boolean
import Control.DeepSeq(deepseq)

import Control.Monad
import Control.Monad.Trans.Class
import Csound.Dynamic hiding (when1, newLocalVars)

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.GE

import qualified Csound.Dynamic as D

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
   
-- | Allocates a new local (it is visible within the instrument) mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
-- It contains control signals (k-rate) and constants for numbers (i-rates).
newCtrlRef :: Tuple a => a -> SE (Ref a)
newCtrlRef t = fmap Ref $ newLocalVars (fmap toCtrlRate $ tupleRates t) (fromTuple t) 
        
toCtrlRate x = case x of 
    Ar -> Kr
    Kr -> Ir
    _  -> x

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

-- | Allocates a new global mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
-- It contains control signals (k-rate) and constants for numbers (i-rates).
newGlobalCtrlRef :: Tuple a => a -> SE (Ref a)
newGlobalCtrlRef t = fmap Ref $ newGlobalVars (fmap toCtrlRate $ tupleRates t) (fromTuple t)   

-- | An alias for the function @newRef@. It returns not the reference
-- to mutable value but a pair of reader and writer functions.
globalSensorsSE :: Tuple a => a -> SE (SE a, a -> SE ())
globalSensorsSE a = do
    ref <- newRef a
    return $ (readRef ref, writeRef ref)

-- | Allocates a new clearable global mutable value and initializes it with value. 
-- A reference can contain a tuple of variables.
-- The variable is set to zero at the end of every iteration.
-- It's useful for accumulation of audio values from several instruments.
newClearableGlobalRef :: Tuple a => a -> SE (Ref a)
newClearableGlobalRef t = fmap Ref $ newClearableGlobalVars (tupleRates t) (fromTuple t) 

-------------------------------------------------------------------------------
-- writable tables

-- | Creates a new table. The Tab could be used while the instrument
-- is playing. When the instrument is retriggered the new tab is allocated.
--
-- > newTab size
newTab :: D -> SE Tab
newTab size = ftgentmp 0 0 size 7 0 [size, 0]

-- | Creates a new global table. 
-- It's generated only once. It's persisted between instrument calls.
--
-- > newGlobalTab identifier size
newGlobalTab :: Int -> SE Tab
newGlobalTab size = do 
    ref <- newGlobalCtrlRef ((fromGE $ saveWriteTab size) :: D)
    fmap (fromGE . toGE) $ readRef ref

{-
    identifier <- geToSe $ getNextGlobalGenId
    ref <- newGlobalRef (0 :: D)        
    tabId <- ftgenonce 0 (Csound.Typed.Types.Prim.int identifier) size 7 0 [size, 0]
    writeRef ref (fromGE $ toGE tabId)
    fmap (fromGE . toGE) $ readRef ref
-}

-----------------------------------------------------------------------
-- some opcodes that I have to define upfront


-- | 
-- Generate a function table from within an instrument definition, without duplication of data.
--
-- Enables the creation of function tables entirely inside 
--       instrument definitions, without any duplication of data.
--
-- > ifno  ftgenonce  ip1, ip2dummy, isize, igen, iarga, iargb, ...
--
-- csound doc: <http://www.csounds.com/manual/html/ftgenonce.html>
ftgenonce ::  D -> D -> D -> D -> D -> [D] -> SE Tab
ftgenonce b1 b2 b3 b4 b5 b6 = fmap ( Tab . return) $ SE $ (depT =<<) $ lift $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> mapM unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "ftgenonce" [(Ir,(repeat Ir))] ([a1,a2,a3,a4,a5] ++ a6)

-- | 
-- Generate a score function table from within the orchestra, which is deleted at the end of the note.
--
-- Generate a score function table from within the orchestra,
--     which is optionally deleted at the end of the note.
--
-- > ifno  ftgentmp  ip1, ip2dummy, isize, igen, iarga, iargb, ...
--
-- csound doc: <http://www.csounds.com/manual/html/ftgentmp.html>
ftgentmp ::  D -> D -> D -> D -> D -> [D] -> SE Tab
ftgentmp b1 b2 b3 b4 b5 b6 = fmap ( Tab . return) $ SE $ (depT =<<) $ lift $ f <$> unD b1 <*> unD b2 <*> unD b3 <*> unD b4 <*> unD b5 <*> mapM unD b6
    where f a1 a2 a3 a4 a5 a6 = opcs "ftgentmp" [(Ir,(repeat Ir))] ([a1,a2,a3,a4,a5] ++ a6)

------------------------------------------------

whileSE :: SE BoolSig -> SE () -> SE ()
whileSE mcond body = do        
    ref <- newCtrlRef (0::Sig)
    writeCond ref
    untilRefBegin ref
    body
    writeCond ref
    untilRefEnd
    where
        writeCond ref = do
            cond <- mcond
            when1 cond        $ writeRef ref 1
            when1 (notB cond) $ writeRef ref 0

-- ifBegin :: BoolSig -> SE ()
-- ifBegin a = fromDep_ $ D.ifBegin Kr =<< lift (toGE a)

untilRefBegin :: Ref Sig -> SE ()
untilRefBegin (Ref [var]) = fromDep_ $ D.untilBegin ((D.prim $ D.PrimVar D.Kr var) ==* 1)

untilRefEnd :: SE ()
untilRefEnd = fromDep_ D.untilEnd
