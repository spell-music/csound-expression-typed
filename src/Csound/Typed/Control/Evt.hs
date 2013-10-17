{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Evt(
    trig, sched, schedHarp, autoOff,
    trig_, sched_,
    trigBy, schedBy, schedHarpBy
) where

import Control.Applicative
import Control.Monad.IO.Class

import qualified Csound.Dynamic as C
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

-------------------------------------------------
-- triggereing the events

trig :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
trig instr evts = trigBy instr (const $ evts) (Unit $ return ())
    
trigBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, D, a)) -> (c -> b)
trigBy instr evts args = toTuple $ do
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
    justArgs <- fromTuple args
    saveEvtInstr (arityOuts $ funArity instr) instrId justArgs (evts toArg)
     
saveEvtInstr :: Arg a => Int -> C.InstrId -> [E] -> Evt (D, D, a) -> GE [E]
saveEvtInstr arity instrId evtArgs evts = do
    evtMixInstrId <- onInstr . C.saveInstr =<< evtMixInstr
    return $ C.subinstr arity evtMixInstrId evtArgs
    where
        evtMixInstr :: GE C.InstrBody
        evtMixInstr = execSG $ fmap (return . return) $ do
            chnId <- fromDep $ return $ C.chnRefAlloc arity
            go chnId evts
            fromDep_ $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 

        go :: Arg a => GE C.ChnRef -> Evt (D, D, a) -> SE ()
        go mchnId es = 
            runEvt es $ \(start, dur, args) -> fromDep_ $ do
                chnId <- mchnId
                e <- C.Event instrId <$> toGE start <*> toGE dur <*> (fmap (++ [C.chnRefId chnId]) $ toNote args) 
                return $ C.event e 

sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
sched instr evts = trig instr (fmap phi evts)
    where phi (a, b) = (0, a, b)

schedBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, a)) -> (c -> b)
schedBy instr evts = trigBy instr (fmap (fmap phi) evts)
    where phi (a, b) = (0, a, b)

schedHarp :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> b
schedHarp instr evts = schedHarpBy instr (const evts) (Unit $ return ())

schedHarpBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt a) -> (c -> b)
schedHarpBy instr evts args = toTuple $ do
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp $ (autoOff 2 =<< ) . instr)
    justArgs <- fromTuple args
    saveEvtInstr (arityOuts $ funArity instr) instrId justArgs (fmap phi $ evts toArg)
    where phi a = (0, -1, a)

autoOff :: Sigs a => D -> a -> SE a
autoOff dt sigs = fmap toTuple $ fromDep $ phi =<< fromTuple sigs
    where 
        phi x = do
            dtE <- toGE dt
            return $ C.autoOff dtE x

-----------------------------------------------------------------------
--

trig_ :: (Arg a) => (a -> SE ()) -> Evt (D, D, a) -> SE ()
trig_ instr evts = fromDep_ $ do
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached_ cacheName (unitExp $ unit $ instr toArg)
    saveEvtInstr_ instrId evts

saveEvtInstr_ :: Arg a => C.InstrId -> Evt (D, D, a) -> GE (C.Dep ())
saveEvtInstr_ instrId evts = execSE $ runEvt evts $ \(start, dur, args) -> fromDep_ $ 
    fmap C.event $ C.Event instrId <$> toGE start <*> toGE dur <*> toNote args

sched_ :: (Arg a) => (a -> SE ()) -> Evt (D, a) -> SE ()
sched_ instr evts = trig_ instr (fmap phi evts)
    where phi (a, b) = (0, a, b)

