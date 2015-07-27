-- | Band-limited oscillators
module Csound.Typed.Control.Vco(
    saw, isaw, pulse, tri, sqr, blosc,
    saw', isaw', pulse', tri', sqr', blosc'
) where

import Csound.Typed.GlobalState
import Csound.Typed.Types

--------------------------------------------------------------
-- no phase

-- | A sawtooth.
saw :: Sig -> Sig
saw = noPhaseWave Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isaw :: Sig -> Sig
isaw = noPhaseWave IntegratedSaw

-- | A triangle wave.
tri :: Sig -> Sig
tri = noPhaseWave Triangle

-- | Pulse (not normalized).
pulse :: Sig -> Sig 
pulse = noPhaseWave Pulse

-- | A square wave.
sqr :: Sig -> Sig
sqr = noPhaseWave Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
blosc :: Tab -> Sig -> Sig
blosc tab cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ noPhaseWave (UserGen gen) cps

--------------------------------------------------------------
-- with phase

-- | A sawtooth.
saw' :: D -> Sig -> Sig
saw' = withPhaseWave Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isaw' :: D -> Sig -> Sig
isaw' = withPhaseWave IntegratedSaw

-- | A triangle wave.
tri' :: D -> Sig -> Sig
tri' = withPhaseWave Triangle

-- | Pulse (not normalized).
pulse' :: D -> Sig -> Sig 
pulse' = withPhaseWave Pulse

-- | A square wave.
sqr' :: D -> Sig -> Sig
sqr' = withPhaseWave Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
blosc' :: Tab -> D -> Sig -> Sig
blosc' tab phs cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ withPhaseWave (UserGen gen) phs cps

--------------------------------------------------------------

noPhaseWave :: BandLimited -> Sig -> Sig
noPhaseWave waveType cps = fromGE $ do
    expr <- toGE cps
    waveId <- saveBandLimitedWave waveType
    return $ readBandLimited Nothing waveId expr

withPhaseWave :: BandLimited -> D -> Sig -> Sig
withPhaseWave waveType phs cps = fromGE $ do
    expr <- toGE cps
    phsExpr <- toGE phs
    waveId <- saveBandLimitedWave waveType
    return $ readBandLimited (Just phsExpr) waveId expr
