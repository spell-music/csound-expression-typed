module Csound.Typed.Control (
    -- * SE
    module Csound.Typed.Types.GlobalState.SE,
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
    
import Csound.Typed.Types.GlobalState.SE

import Csound.Typed.Control.SERef

import Csound.Typed.Control.Evt
import Csound.Typed.Control.Mix
import Csound.Typed.Control.Midi

import Csound.Dynamic
import Csound.Typed.Types
import Csound.Typed.Types.GlobalState

instr0 :: Tuple a => SE a -> SE a
instr0 a = return $ toTuple $ saveIns0 (ins0Arity a) (ins0Exp a)
    where
        ins0Exp = fmap fromTuple

        ins0Arity = tupleArity . proxy

        proxy :: Tuple a => SE a -> a
        proxy = undefined

saveIns0 :: Int -> SE (GE [E]) -> GE [E]
saveIns0 = undefined

