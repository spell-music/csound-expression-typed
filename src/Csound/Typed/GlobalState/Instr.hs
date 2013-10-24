module Csound.Typed.GlobalState.Instr where

import Control.Monad

import Csound.Dynamic
import qualified Csound.Dynamic.Control as C

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
type UnitExp = Dep ()

writeOut :: ([E] -> SE ()) -> InsExp -> Dep ()
writeOut f expr = execSE $ f =<< expr

saveSourceInstrCached :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCached cacheName arity instr = onInstr . C.saveCachedInstr cacheName $ writeOut toOut instr
    where toOut = SE . C.sendChn (arityIns arity) (arityOuts arity)

saveSourceInstrCached_ :: C.CacheName -> UnitExp -> GE InstrId
saveSourceInstrCached_ cacheName instr = onInstr $ C.saveCachedInstr cacheName instr

saveEffectInstr :: Arity -> EffExp -> GE InstrId
saveEffectInstr arity eff = onInstr . C.saveInstr $ (execSE $ setOuts =<< eff =<< getIns)
    where 
        setOuts = SE . C.writeChn (C.chnRefFromParg 5 (arityOuts arity))
        getIns  = SE $ C.readChn  $ C.chnRefFromParg 4 (arityIns  arity)

saveMixInstr :: Int -> CsdEventList M -> GE InstrId
saveMixInstr arity a = do
    setDuration $ csdEventListDur a
    onInstr $ C.saveInstr $ C.sendOut arity =<< renderMixSco arity a

saveMixInstr_ :: CsdEventList M -> GE (DepT GE ())
saveMixInstr_ a = do
    setDuration $ csdEventListDur a
    return $ renderMixSco_ a

saveMasterInstr :: Arity -> InsExp -> GE ()
saveMasterInstr arity sigs = do
    gainLevel <- fmap defGain getOptions 
    let expr1 = writeOut (SE . C.sendOut (arityOuts arity) . C.safeOut gainLevel) sigs
    expr2 <- getSysExpr 
    instrId <- onInstr $ C.saveInstr (expr1 >> expr2)
    setMasterInstrId instrId

saveMidiInstr :: MidiType -> Channel -> Arity -> InsExp -> GE [E]
saveMidiInstr midiType channel arity instr = do
    setDurationToInfinite
    vars <- onGlobals $ sequence $ replicate (arityOuts arity) (C.newClearableGlobalVar Ar 0)
    let expr = writeOut (SE . zipWithM_ (appendVarBy (+)) vars) instr
    instrId <- onInstr $ C.saveInstr expr
    saveMidi $ MidiAssign midiType channel instrId
    return $ fmap readOnlyVar vars 

saveMidiInstr_ :: MidiType -> Channel -> UnitExp -> GE (Dep ())
saveMidiInstr_ midiType channel instr = do
    instrId <- onInstr . C.saveInstr $ instr
    saveMidi $ MidiAssign midiType channel instrId
    return $ return ()

saveIns0 :: Int -> [Rate] -> SE [E] -> GE [E]
saveIns0 arity rates as = do
    vars <- onGlobals $ zipWithM C.newGlobalVar rates (replicate arity 0)
    saveUserInstr0 $ writeOut (SE . zipWithM_ writeVar vars) as 
    return $ fmap readOnlyVar vars

