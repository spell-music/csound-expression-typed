module Csound.Typed.Control where

import Control.Applicative
import Control.Monad

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import Csound.Dynamic.Control

-- global side effects
newtype GE a = GE { unGE :: ReaderT Options (StateT History IO) a }

instance Functor GE where
    fmap f = GE . fmap f . unGE

instance Applicative GE where
    pure = return
    (<*>) = ap

instance Monad GE where
    return = GE . return
    ma >>= mf = GE $ unGE ma >>= unGE . mf

data Options = Options 
    { setSampleRate :: Int
    , setBlockSize  :: Int
    , setSeed       :: Double }
    
data History = History
    { genMap    :: GenMap
    , stringMap :: StringMap
    , globals   :: Globals
    , locals    :: Locals
    , instrs    :: Instrs }
   
unsafePerformGE :: GE a -> a
unsafePerformGE = undefined
