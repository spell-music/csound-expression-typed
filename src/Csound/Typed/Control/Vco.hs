-- | Band-limited oscillators
module Csound.Typed.Control.Vco(
    saw, isaw, pulse, tri, sqr, blosc,
    saw', isaw', pulse', tri', sqr', blosc',

    -- | Hard sync 
    SyncSmooth(..),
    sawSync, isawSync, pulseSync, triSync, sqrSync, bloscSync,
    sawSync', isawSync', pulseSync', triSync', sqrSync', bloscSync'
) where

import Csound.Dynamic(Gen(..), GenId(..))
import Csound.Typed.GlobalState
import Csound.Typed.Types

import Csound.Typed.GlobalState

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

--------------------------------------------------------------
-- no phase

-- | A sawtooth.
sawSync :: SyncSmooth -> Sig -> Sig -> Sig
sawSync = noPhaseWaveHardSync Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isawSync :: SyncSmooth -> Sig -> Sig -> Sig
isawSync = noPhaseWaveHardSync IntegratedSaw

-- | A triangle wave.
triSync :: SyncSmooth -> Sig -> Sig -> Sig
triSync = noPhaseWaveHardSync Triangle

-- | Pulse (not normalized).
pulseSync :: SyncSmooth -> Sig -> Sig -> Sig 
pulseSync = noPhaseWaveHardSync Pulse

-- | A square wave.
sqrSync :: SyncSmooth -> Sig -> Sig -> Sig
sqrSync = noPhaseWaveHardSync Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
bloscSync :: SyncSmooth -> Tab -> Sig -> Sig -> Sig
bloscSync smoothType tab ratioCps cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ noPhaseWaveHardSync (UserGen gen) smoothType ratioCps cps

--------------------------------------------------------------
-- with phase

-- | A sawtooth.
sawSync' :: SyncSmooth -> D -> Sig -> Sig -> Sig
sawSync' = withPhaseWaveHardSync Saw

-- | Integrated sawtooth: 4 * x * (1 - x).
isawSync' :: SyncSmooth -> D -> Sig -> Sig -> Sig
isawSync' = withPhaseWaveHardSync IntegratedSaw

-- | A triangle wave.
triSync' :: SyncSmooth -> D -> Sig -> Sig -> Sig
triSync' = withPhaseWaveHardSync Triangle

-- | Pulse (not normalized).
pulseSync' :: SyncSmooth -> D -> Sig -> Sig -> Sig 
pulseSync' = withPhaseWaveHardSync Pulse

-- | A square wave.
sqrSync' :: SyncSmooth -> D -> Sig -> Sig -> Sig
sqrSync' = withPhaseWaveHardSync Square

-- | A band-limited oscillator with user defined waveform (it's stored in the table).
bloscSync' :: SyncSmooth -> Tab -> D -> Sig -> Sig -> Sig
bloscSync' smoothType tab phs ratioCps cps = hideGE $ do
    gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
    return $ withPhaseWaveHardSync (UserGen gen) smoothType phs ratioCps cps

-----------------------------------------------

-- | Type of smooth shape to make smooth transitions on retrigger.
-- Available types are: 
--
-- * No smooth: @RawSync@
--
-- * Ramp smooth: @SawSync@
--
-- * Triangular smooth: @TriSync@
--
-- * User defined shape: @UserSync@
data SyncSmooth = RawSync | SawSync | TriSync | TrapSync | UserSync Tab

getSyncShape :: SyncSmooth -> GE (Maybe BandLimited)
getSyncShape x = case x of
    RawSync -> return $ Nothing
    SawSync -> gen7 4097 [1, 4097, 0]
    TriSync -> gen7 4097 [0, 2048, 1, 2049, 0]
    TrapSync -> gen7 4097 [1, 2048, 1, 2049, 0]
    UserSync tab -> do
        gen <- fromPreTab $ getPreTabUnsafe "blosc: tab should be primitive, not an expression." tab
        return $ Just $ UserGen gen
    where
        gen7 size args = return $ Just $ UserGen $ Gen { genSize = size, genId = IntGenId 7, genArgs = args, genFile = Nothing }

noPhaseWaveHardSync :: BandLimited -> SyncSmooth -> Sig -> Sig -> Sig
noPhaseWaveHardSync waveType smoothWaveType syncRatio cps = fromGE $ do
    smoothWave <- getSyncShape smoothWaveType
    exprRatio <- toGE syncRatio
    exprCps <- toGE cps
    waveId <- saveBandLimitedWave waveType
    smoothWaveId <- case smoothWave of
        Nothing -> return Nothing
        Just wave -> fmap Just $ saveBandLimitedWave wave
    return $ readHardSyncBandLimited smoothWaveId Nothing waveId exprRatio exprCps

withPhaseWaveHardSync :: BandLimited -> SyncSmooth -> D -> Sig -> Sig -> Sig
withPhaseWaveHardSync waveType smoothWaveType phs syncRatio cps = fromGE $ do
    smoothWave <- getSyncShape smoothWaveType
    phsExpr <- toGE phs
    exprRatio <- toGE syncRatio
    exprCps <- toGE cps
    waveId <- saveBandLimitedWave waveType
    smoothWaveId <- case smoothWave of
        Nothing -> return Nothing
        Just wave -> fmap Just $ saveBandLimitedWave wave
    return $ readHardSyncBandLimited smoothWaveId (Just phsExpr) waveId exprRatio exprCps
