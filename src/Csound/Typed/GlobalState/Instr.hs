module Csound.Typed.GlobalState.Instr where

import Control.Monad

import Csound.Dynamic
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types.MixSco
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.Converters
import Csound.Typed.GlobalState.Options
import Csound.Typed.GlobalState.Cache

import Debug.Trace



data Arity = Arity
    { arityIns      :: Int
    , arityOuts     :: Int }

type InsExp = SE (GE [E])
type EffExp = [E] -> SE (GE [E])
type UnitExp = GE (Dep ())

writeOut :: ([E] -> Dep ()) -> InsExp -> GE C.InstrBody
writeOut f expr = execSE $ join $ fmap (fromDep_ . fmap f) expr

saveSourceInstrCached :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCached cacheName arity instr = onInstr . C.saveCachedInstr cacheName =<< writeOut toOut instr
    where toOut = C.sendChn (arityIns arity) (arityOuts arity)

saveSourceInstrCached_ :: C.CacheName -> UnitExp -> GE InstrId
saveSourceInstrCached_ cacheName instr = onInstr . C.saveCachedInstr cacheName =<< instr

saveEffectInstr :: Arity -> EffExp -> GE InstrId
saveEffectInstr arity eff = onInstr . C.saveInstr =<< (execSG2 $ fmap (fmap (fmap setOuts) . eff) getIns)
    where 
        setOuts = C.writeChn $ C.chnRefFromParg 5 (arityOuts arity)
        getIns  = C.readChn  $ C.chnRefFromParg 4 (arityIns  arity)

saveMixInstr :: Int -> [E] -> CsdEventList M -> GE [E]
saveMixInstr arity args a = traceShow ("mix") $ do
    setDuration $ csdEventListDur a
    instrId <- onInstr $ C.saveInstr $ C.sendOut arity =<< renderMixSco arity a
    return $ C.subinstr arity instrId args

saveMixInstr_ :: CsdEventList M -> GE (Dep ())
saveMixInstr_ a = traceShow ("mix_") $ do
    setDuration $ csdEventListDur a
    return $ renderMixSco_ a

saveMasterInstr :: Arity -> InsExp -> GE ()
saveMasterInstr arity sigs = traceShow ("master") $ do
    gainLevel <- fmap setGain getOptions 
    expr1 <- writeOut (C.sendOut (arityOuts arity) . C.safeOut gainLevel) sigs
    expr2 <- getSysExpr 
    instrId <- onInstr $ C.saveInstr (expr1 >> expr2)
    setMasterInstrId instrId

saveMidiInstr :: MidiKey -> Arity -> InsExp -> GE [E]
saveMidiInstr key@(MidiKey midiType channel _) arity instr = do
    midiInstr <- getMidiInstrFromCache key
    case midiInstr of 
        Just res    -> return res
        Nothing     -> do
            vars <- onGlobals $ sequence $ replicate (arityOuts arity) (C.newClearableGlobalVar Ar 0)
            expr <- writeOut (zipWithM_ (appendVarBy (+)) vars) instr
            instrId <- onInstr $ C.saveInstr expr
            saveMidi $ MidiAssign midiType channel instrId
            let res = fmap readOnlyVar vars 
            saveMidiInstrToCache key res
            return res

saveMidiInstr_ :: MidiType -> Channel -> UnitExp -> GE (Dep ())
saveMidiInstr_ midiType channel instr = do
    instrId <- onInstr . C.saveInstr =<< instr
    saveMidi $ MidiAssign midiType channel instrId
    return $ return ()

saveIns0 :: Int -> [Rate] -> SE (GE [E]) -> GE [E]
saveIns0 arity rates as = do
    vars <- onGlobals $ zipWithM C.newGlobalVar rates (replicate arity 0)
    saveUserInstr0 =<< writeOut (zipWithM_ writeVar vars) as 
    return $ fmap readOnlyVar vars

