module Csound.Typed.GlobalState.Instr where

import Control.Monad
import Data.Map

import Csound.Dynamic
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types.MixSco
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.Options
import Csound.Typed.GlobalState.Cache
import Csound.Typed.GlobalState.Opcodes(turnoff2, exitnow, servantUpdateChnAlive, servantUpdateChnRetrig)
import Csound.Typed.GlobalState.Elements(getInstrIds)

data Arity = Arity
    { arityIns      :: Int
    , arityOuts     :: Int }

type InsExp = SE [E]
type EffExp = [E] -> SE [E]
type UnitExp = SE ()

saveInstr :: SE () -> GE InstrId
saveInstr a = onInstr . C.saveInstr =<< execSE a

saveCachedInstr :: C.CacheName -> SE () -> GE InstrId
saveCachedInstr cacheName a = onInstr . C.saveCachedInstr cacheName =<< execSE a

livenessWatch :: Arity -> SE ()
livenessWatch arity = fromDep_ $ servantUpdateChnAlive (C.chnPargId $ arityIns arity)

retrigWatch :: Arity -> SE ()
retrigWatch arity = fromDep_ $ servantUpdateChnRetrig (C.chnPargId $ arityIns arity)

saveSourceInstrCachedWithLivenessWatch :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCachedWithLivenessWatch cacheName arity instr = saveCachedInstr cacheName $ do
    toOut =<< instr
    livenessWatch arity 
    where toOut = SE . C.sendChn (arityIns arity) (arityOuts arity)

saveSourceInstrCachedWithLivenessWatchAndRetrig :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCachedWithLivenessWatchAndRetrig cacheName arity instr = saveCachedInstr cacheName $ do
    toOut =<< instr
    livenessWatch arity
    retrigWatch arity
    where toOut = SE . C.sendChn (arityIns arity) (arityOuts arity)

saveSourceInstrCachedWithLivenessWatchAndRetrigAndEvtLoop :: C.CacheName -> Arity -> InsExp -> UnitExp -> GE (InstrId, InstrId)
saveSourceInstrCachedWithLivenessWatchAndRetrigAndEvtLoop cacheName arity instr evtInstr = do 
    instrId <- saveSourceInstrCachedWithLivenessWatchAndRetrig cacheName arity instr
    evtInstrId <- saveInstr evtInstr
    return (instrId, evtInstrId)

saveSourceInstrCached :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCached cacheName arity instr = saveCachedInstr cacheName $ toOut =<< instr
    where toOut = SE . C.sendChn (arityIns arity) (arityOuts arity)

saveSourceInstrCached_ :: C.CacheName -> UnitExp -> GE InstrId
saveSourceInstrCached_ cacheName instr = saveCachedInstr cacheName instr

saveSourceInstrCachedWithLivenessWatch_ :: C.CacheName -> Arity -> UnitExp -> GE InstrId
saveSourceInstrCachedWithLivenessWatch_ cacheName arity instr = saveCachedInstr cacheName $ 
    instr >> livenessWatch arity

saveEffectInstr :: Arity -> EffExp -> GE InstrId
saveEffectInstr arity eff = saveInstr $ setOuts =<< eff =<< getIns
    where 
        setOuts = SE . C.writeChn (C.chnRefFromParg 5 (arityOuts arity))
        getIns  = SE $ C.readChn  $ C.chnRefFromParg 4 (arityIns  arity)

saveMixInstr :: Int -> CsdEventList M -> GE InstrId
saveMixInstr arity a = do
    setDuration $ csdEventListDur a
    saveInstr $ SE $ C.sendOut arity =<< renderMixSco arity a

saveMixInstr_ :: CsdEventList M -> GE (DepT GE ())
saveMixInstr_ a = do
    setDuration $ csdEventListDur a
    return $ renderMixSco_ a

saveMasterInstr :: Arity -> InsExp -> GE ()
saveMasterInstr arity sigs = do
    gainLevel <- fmap defGain getOptions 
    saveAlwaysOnInstr =<< (saveInstr $ (SE . C.sendOut (arityOuts arity) . C.safeOut gainLevel) =<< sigs)

saveMidiInstr :: C.MidiType -> C.Channel -> Arity -> InsExp -> GE [E]
saveMidiInstr midiType channel arity instr = do
    setDurationToInfinite
    vars <- onGlobals $ sequence $ replicate (arityOuts arity) (C.newClearableGlobalVar Ar 0)
    let expr = (SE . zipWithM_ (appendVarBy (+)) vars) =<< instr
    instrId <- saveInstr expr
    saveMidi $ MidiAssign midiType channel instrId
    return $ fmap readOnlyVar vars 

saveMidiMap :: GE ()
saveMidiMap = do
    m <- fmap midiMap getHistory
    mapM_ (\(C.MidiKey midiType channel, instrExpr) -> saveMidiInstr_ midiType channel (SE instrExpr)) $ toList m

saveMidiInstr_ :: C.MidiType -> C.Channel -> UnitExp -> GE ()
saveMidiInstr_ midiType channel instr = do
    instrId <- onInstr . C.saveInstr =<< execSE instr
    saveMidi $ MidiAssign midiType channel instrId   

saveIns0 :: Int -> [Rate] -> SE [E] -> GE [E]
saveIns0 arity rates as = do
    vars <- onGlobals $ zipWithM C.newPersistentGlobalVar rates (replicate arity 0)
    saveUserInstr0 $ unSE $ (SE . zipWithM_ writeVar vars) =<< as 
    return $ fmap readOnlyVar vars

terminatorInstr :: GE (SE ())
terminatorInstr = do
    ids <- fmap (getInstrIds . instrs) getHistory
    return $ fromDep_ $ (mapM_ turnoff2 $ fmap instrIdE ids) >> exitnow

