-- | Band-limited oscillators
module Csound.Typed.Control.Vco(
    saw, isaw, pulse, tri, sqr, blosc        
) where

import Csound.Dynamic.Control

import Csound.Typed.GlobalState
import Csound.Typed.Types

-- | A sawtooth.
saw :: Sig -> Sig
saw = wave Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isaw :: Sig -> Sig
isaw = wave IntegratedSaw

-- | A triangle wave.
tri :: Sig -> Sig
tri = wave Triangle

-- | Pulse (not normalized).
pulse :: Sig -> Sig 
pulse = wave Pulse

-- | A square wave.
sqr :: Sig -> Sig
sqr = wave Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
blosc :: Tab -> Sig -> Sig
blosc tab cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ wave (UserGen gen) cps

wave :: BandLimited -> Sig -> Sig
wave waveType cps = fromGE $ do
    expr <- toGE cps
    waveId <- saveBandLimitedWave waveType
    return $ readBandLimited waveId expr
    
