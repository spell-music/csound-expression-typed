module Csound.Typed.Types.GlobalState.Instr where

import Control.Monad

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

import qualified System.Mem.StableName.Dynamic as DM

import Csound.Dynamic
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types
import Csound.Typed.Types.MixSco
import Csound.Typed.Types.GlobalState

funProxy :: (a -> b) -> (a, b)
funProxy = const (msg, msg)
    where msg = error "I'm a Csound.Typed.Types.GlobalState.Instr.funProxy"

saveSourceInstrCached :: (Arg a, Out b) => (a -> b) -> GE InstrId
saveSourceInstrCached instr = do
    expr <- writeOut toExpr $ instr toArg
    onInstrIO $ do
        cacheName <- liftIO $ DM.makeDynamicStableName instr
        st <- get
        let (instrId, st1) = runState (C.saveCachedInstr cacheName expr) st
        put st1
        return instrId
    where toExpr = uncurry C.sendChn (insArity instr)

saveEffectInstr :: (Out a, Out b) => (a -> b) -> GE InstrId
saveEffectInstr eff = do
    expr <- execSG2 $ fmap (fmap (fmap setOuts) . outGE . (eff $ )) getIns
    onInstr $ C.saveInstr expr
    where 
        (arityIns, arityOuts) = effArity eff
        setOuts = C.writeChn $ C.chnRefFromParg 5 arityOuts

        getIns :: Out a => Dep a
        getIns  = fmap (fromOut . return . map fromE) $ C.readChn $ C.chnRefFromParg 4 arityIns

insArity :: (Arg a, Out b) => (a -> b) -> (Int, Int)
insArity instr = (arity a, outArity b)
    where (a, b) = funProxy instr

effArity :: (Out a, Out b) => (a -> b) -> (Int, Int)
effArity instr = (outArity a, outArity b)
    where (a, b) = funProxy instr

saveMixInstr :: Int -> CsdEventList M -> GE [Sig]
saveMixInstr arity a = do
    setDuration $ csdEventListDur a
    instrId <- onInstr $ C.saveInstr $ C.sendOut arity =<< renderMixSco arity a
    return $ map fromE $ C.subinstr arity instrId []

saveMasterInstr :: Out a => a -> GE ()
saveMasterInstr sigs = do
    expr1 <- writeOut (C.sendOut (outArity sigs) . C.safeOut) sigs
    expr2 <- getSysExpr 
    instrId <- onInstr $ C.saveInstr (expr1 >> expr2)
    setMasterInstrId instrId
    setInstr0 (outArity sigs)

saveMidiInstr :: Out a => MidiType -> Channel -> (Msg -> a) -> GE [Sig]
saveMidiInstr midiType channel instr = do
    let sigs = instr Msg
    vars <- onGlobals $ sequence $ replicate (outArity sigs) (C.newClearableGlobalVar Ar 0)
    expr <- writeOut (zipWithM_ (appendVarBy (+)) vars) sigs
    instrId <- onInstr $ C.saveInstr expr
    saveMidi $ MidiAssign midiType channel instrId
    return $ fmap (fromE . readOnlyVar) vars 

writeOut :: Out a => ([E] -> Dep ()) -> a -> GE C.InstrBody
writeOut f sigs = execSG $ fmap (fmap f . mapM toGE) $ toOut sigs

