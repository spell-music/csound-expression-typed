module Csound.Typed.Control (
    -- * SE
    module Csound.Typed.GlobalState.SE,
    -- ** SE reference
    module Csound.Typed.Control.SERef,
    -- * Global settings
    instr0,
    -- * Score
    module Csound.Typed.Control.Mix,
    -- * Midi
    module Csound.Typed.Control.Midi,
    -- * Events
    module Csound.Typed.Control.Evt
) where
    
import Csound.Typed.GlobalState.SE

import Csound.Typed.Control.SERef

import Csound.Typed.Control.Evt
import Csound.Typed.Control.Mix
import Csound.Typed.Control.Midi

import Csound.Typed.Types
import Csound.Typed.GlobalState

instr0 :: Tuple a => SE a -> SE a
instr0 a = return $ toTuple $ saveIns0 ins0Arity (tupleRates $ proxy a) ins0Exp
    where
        ins0Exp = fmap fromTuple a

        ins0Arity = tupleArity $ proxy a

        proxy :: Tuple a => SE a -> a
        proxy = const (toTuple $ return $ repeat undefined)

