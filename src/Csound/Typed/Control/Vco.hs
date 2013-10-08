-- | Band-limited oscillators
module Csound.Typed.Control.Vco(
    saw, isaw, pulse, tri, sqr, blosc        
) where

import Csound.Dynamic.Control

import Csound.Typed.GlobalState
import Csound.Typed.Types

saw :: Sig -> Sig
saw = wave Saw

isaw :: Sig -> Sig
isaw = wave IntegratedSaw

tri :: Sig -> Sig
tri = wave Triangle

pulse :: Sig -> Sig 
pulse = wave Pulse

sqr :: Sig -> Sig
sqr = wave Square

blosc :: Tab -> Sig -> Sig
blosc tab cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ wave (UserGen gen) cps

wave :: BandLimited -> Sig -> Sig
wave waveType cps = fromGE $ do
    expr <- toGE cps
    waveId <- saveBandLimitedWave waveType
    return $ readBandLimited waveId expr
    
