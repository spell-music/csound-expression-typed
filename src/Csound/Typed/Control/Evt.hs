{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Evt where

import Control.Applicative
import Control.Monad(join)

import qualified Csound.Dynamic as C
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types
import Csound.Typed.Types.GlobalState
import Csound.Typed.Types.GlobalState.Instr

-------------------------------------------------
-- triggereing the events

trig :: (Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt (D, D, a) -> NoSE b
trig instr evts = fromOut $ do
    instrId <- saveSourceInstrCached instr
    saveEvtInstr arityOuts instrId evts
    where (_, arityOuts) = insArity instr
     
saveEvtInstr :: Arg a => Int -> C.InstrId -> Evt (D, D, a) -> GE [Sig]
saveEvtInstr arityOuts instrId evts = do
    evtMixInstrId <- onInstr . C.saveInstr =<< evtMixInstr
    return $ fmap fromE $ C.subinstr arityOuts evtMixInstrId []
    where
        evtMixInstr :: GE C.InstrBody
        evtMixInstr = execSG $ fmap (return . return) $ do
            chnId <- fromDep $ return $ C.chnRefAlloc arityOuts
            go chnId evts
            fromDep_ $ fmap (\chn -> C.sendOut arityOuts =<< C.readChn chn) chnId 

        go :: Arg a => GE C.ChnRef -> Evt (D, D, a) -> SE ()
        go mchnId es = 
            runEvt es $ \(start, dur, args) -> fromDep_ $ do
                chnId <- mchnId
                e <- C.Event instrId <$> toGE start <*> toGE dur <*> (fmap (++ [C.chnRefId chnId]) $ toNote args) 
                return $ C.event e 


sched :: (Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt (D, a) -> NoSE b
sched instr evts = trig instr (fmap phi evts)
    where phi (a, b) = (0, a, b)

schedHarp :: (Arg a, Out b, Out (NoSE b)) => (a -> b) -> Evt a -> NoSE b
schedHarp instr evts = fromOut $ do
    instrId <- saveSourceInstrCached (autoOff 2 . instr)
    saveEvtInstr arityOuts instrId (fmap phi evts)
    where 
        (_, arityOuts) = insArity instr
        phi a = (0, -1, a)

autoOff :: Out a => D -> a -> SE a
autoOff dt sigs = fmap outFromGE $ magic $ fmap (phi =<< ) $ outGE sigs
    where 
        phi x = do
            dtE <- toGE dt
            return $ C.autoOff dtE x

        magic :: SE (GE (C.Dep a)) -> SE (GE a)
        magic = join . fmap fromDep

