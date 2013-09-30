module Csound.Typed.Types.GlobalState.Instr where

import Control.Monad

import Csound.Dynamic
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types.MixSco
import Csound.Typed.Types.GlobalState.GE
import Csound.Typed.Types.GlobalState.SE
import Csound.Typed.Types.GlobalState.Converters

data Arity = Arity
    { arityIns      :: Int
    , arityOuts     :: Int }

type InsExp = SE (GE [E])
type EffExp = [E] -> SE (GE [E])

writeOut :: ([E] -> Dep ()) -> InsExp -> GE C.InstrBody
writeOut f expr = execSG $ fmap (fmap f) expr

saveSourceInstrCached :: C.CacheName -> Arity -> InsExp -> GE InstrId
saveSourceInstrCached cacheName arity instr = onInstr . C.saveCachedInstr cacheName =<< writeOut toOut instr
    where toOut = C.sendChn (arityIns arity) (arityOuts arity)

saveEffectInstr :: Arity -> EffExp -> GE InstrId
saveEffectInstr arity eff = onInstr . C.saveInstr =<< (execSG2 $ fmap (fmap (fmap setOuts) . eff) getIns)
    where 
        setOuts = C.writeChn $ C.chnRefFromParg 5 (arityOuts arity)
        getIns  = C.readChn  $ C.chnRefFromParg 4 (arityIns  arity)

saveMixInstr :: Int -> CsdEventList M -> GE [E]
saveMixInstr arity a = do
    setDuration $ csdEventListDur a
    instrId <- onInstr $ C.saveInstr $ C.sendOut arity =<< renderMixSco arity a
    return $ C.subinstr arity instrId []

saveMasterInstr :: Arity -> InsExp -> GE ()
saveMasterInstr arity sigs = do
    expr1 <- writeOut (C.sendOut (arityOuts arity) . C.safeOut) sigs
    expr2 <- getSysExpr 
    instrId <- onInstr $ C.saveInstr (expr1 >> expr2)
    setMasterInstrId instrId

saveMidiInstr :: MidiType -> Channel -> Arity -> InsExp -> GE [E]
saveMidiInstr midiType channel arity instr = do
    vars <- onGlobals $ sequence $ replicate (arityOuts arity) (C.newClearableGlobalVar Ar 0)
    expr <- writeOut (zipWithM_ (appendVarBy (+)) vars) instr
    instrId <- onInstr $ C.saveInstr expr
    saveMidi $ MidiAssign midiType channel instrId
    return $ fmap readOnlyVar vars 

saveIns0 :: Int -> [Rate] -> SE (GE [E]) -> GE [E]
saveIns0 arity rates as = do
    vars <- onGlobals $ zipWithM C.newGlobalVar rates (replicate arity 0)
    saveUserInstr0 =<< writeOut (zipWithM_ writeVar vars) as 
    return $ fmap readOnlyVar vars

