module Csound.Typed.GlobalState.Instr where

import Control.Monad

import Csound.Dynamic
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types.MixSco
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.Options
import Csound.Typed.GlobalState.Cache

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

saveSourceInstrCached :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCached cacheName arity instr = saveCachedInstr cacheName $ toOut =<< instr
    where toOut = SE . C.sendChn (arityIns arity) (arityOuts arity)

saveSourceInstrCached_ :: C.CacheName -> UnitExp -> GE InstrId
saveSourceInstrCached_ cacheName instr = saveCachedInstr cacheName instr

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
    saveAlwaysOnInstr =<< 
        (saveInstr $ (SE . C.sendOut (arityOuts arity) . C.safeOut gainLevel) =<< sigs)
    expr2 <- getSysExpr 
    saveAlwaysOnInstr =<< saveInstr (SE expr2)
    expr3 <- guiInstrExp 
    saveAlwaysOnInstr =<< saveInstr (SE expr3)

saveMidiInstr :: MidiType -> Channel -> Arity -> InsExp -> GE [E]
saveMidiInstr midiType channel arity instr = do
    setDurationToInfinite
    vars <- onGlobals $ sequence $ replicate (arityOuts arity) (C.newClearableGlobalVar Ar 0)
    let expr = (SE . zipWithM_ (appendVarBy (+)) vars) =<< instr
    instrId <- saveInstr expr
    saveMidi $ MidiAssign midiType channel instrId
    return $ fmap readOnlyVar vars 

saveMidiInstr_ :: MidiType -> Channel -> UnitExp -> GE (Dep ())
saveMidiInstr_ midiType channel instr = do
    instrId <- onInstr . C.saveInstr =<< execSE instr
    saveMidi $ MidiAssign midiType channel instrId
    return $ return ()

saveIns0 :: Int -> [Rate] -> SE [E] -> GE [E]
saveIns0 arity rates as = do
    vars <- onGlobals $ zipWithM C.newPersistentGlobalVar rates (replicate arity 0)
    saveUserInstr0 $ unSE $ (SE . zipWithM_ writeVar vars) =<< as 
    return $ fmap readOnlyVar vars

