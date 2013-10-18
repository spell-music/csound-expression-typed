{-# Language TypeFamilies, FlexibleContexts #-}
module Csound.Typed.Control.Evt(
    trig, sched, schedHarp, trigBy, schedBy, schedHarpBy,
    trig', sched', schedHarp', autoOff,
    trig_, sched_,
    trigBy', schedBy', schedHarpBy'
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad.IO.Class

import qualified Csound.Dynamic as C
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr
import Csound.Typed.Control.Overload

-------------------------------------------------
-- triggereing the events

trig :: (Arg a, Sigs b, Instr f, a ~ InstrIn f, b ~ InstrOut f) => f -> Evt (D, D, a) -> InstrOut f
trig instr evts = genTrig (toInstr instr) evts instr

trig' :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
trig' instr evts = genTrig instr evts instr
    
genTrig :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> cacheInstr -> b
genTrig instr evts cacheInstr = apInstr0 $ do
    key <- evtKey evts cacheInstr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName cacheInstr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId evts

trigBy :: (Arg a, Arg c, Instr f, InstrIn f ~ a, Sigs (InstrOut f)) => f -> (c -> Evt (D, D, a)) -> (c -> InstrOut f)
trigBy instr evts = genTrigBy (toInstr instr) evts instr 

trigBy' :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, D, a)) -> (c -> b)
trigBy' instr evts = genTrigBy instr evts instr

genTrigBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, D, a)) -> cacheInstr -> (c -> b)
genTrigBy instr evts cacheInstr args = flip apInstr args $ do
    key <- evtKey evts cacheInstr
    withCache getEvtKey saveEvtKey key $ do        
        cacheName <- liftIO $ C.makeCacheName cacheInstr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (evts toArg)  

saveEvtInstr :: Arg a => Int -> C.InstrId -> Evt (D, D, a) -> GE C.InstrId
saveEvtInstr arity instrId evts = onInstr . C.saveInstr =<< evtMixInstr
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

sched :: (Arg a, Instr f, a ~ InstrIn f, Sigs (InstrOut f)) => f -> Evt (D, a) -> InstrOut f
sched instr evts = genSched (toInstr instr) evts instr

sched' :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
sched' instr evts = genSched instr evts instr

genSched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> cacheInstr -> b
genSched instr evts cacheInstr = apInstr0 $ do
    key <- evtKey evts cacheInstr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName cacheInstr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi evts)  
    where phi (a, b) = (0, a, b)
        
schedBy :: (Arg a, Instr f, Sigs (InstrOut f), a ~ InstrIn f, Arg c) => f -> (c -> Evt (D, a)) -> (c -> InstrOut f)
schedBy instr evts = genSchedBy (toInstr instr) evts instr

schedBy' :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, a)) -> (c -> b)
schedBy' instr evts = genSchedBy instr evts instr

genSchedBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, a)) -> cacheName -> (c -> b)
genSchedBy instr evts cacheInstr args = flip apInstr args $ do
    key <- evtKey evts cacheInstr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName cacheInstr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi $ evts toArg)  
    where phi (a, b) = (0, a, b)

schedHarp :: (Arg a, Instr f, a ~ InstrIn f, Sigs (InstrOut f)) => f -> Evt a -> InstrOut f
schedHarp instr evts = genSchedHarp (toInstr instr) evts instr

schedHarp' :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> b
schedHarp' instr evts = genSchedHarp instr evts instr

genSchedHarp :: (Arg a, Sigs b) => (a -> SE b) -> Evt a -> cacheInstr -> b
genSchedHarp instr evts cacheInstr = apInstr0 $ do
    key <- evtKey evts cacheInstr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName cacheInstr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp $ (autoOff 2 =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi evts)
    where phi a = (0, -1, a)

schedHarpBy :: (Arg a, Instr f, a ~ InstrIn f, Sigs (InstrOut f), Arg c) => f -> (c -> Evt a) -> (c -> InstrOut f)
schedHarpBy instr evts = genSchedHarpBy (toInstr instr) evts instr

schedHarpBy' :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt a) -> (c -> b)
schedHarpBy' instr evts = genSchedHarpBy instr evts instr

genSchedHarpBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt a) -> cacheInstr -> (c -> b)
genSchedHarpBy instr evts cacheInstr args = flip apInstr args $ do
    key <- evtKey evts cacheInstr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName cacheInstr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp $ (autoOff 2 =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi $ evts toArg)
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
    key <- evtKey evts instr
    withCache getEvtProcKey saveEvtProcKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached_ cacheName (unitExp $ unit $ instr toArg)
        saveEvtInstr_ instrId evts

sched_ :: (Arg a) => (a -> SE ()) -> Evt (D, a) -> SE ()
sched_ instr evts = fromDep_ $ do
    key <- evtKey evts instr
    withCache getEvtProcKey saveEvtProcKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached_ cacheName (unitExp $ unit $ instr toArg)
        saveEvtInstr_ instrId $ fmap phi evts
    where phi (a, b) = (0, a, b)

saveEvtInstr_ :: Arg a => C.InstrId -> Evt (D, D, a) -> GE (C.Dep ())
saveEvtInstr_ instrId evts = execSE $ runEvt evts $ \(start, dur, args) -> fromDep_ $ 
    fmap C.event $ C.Event instrId <$> toGE start <*> toGE dur <*> toNote args

-------------------------------------------------------------------

evtKey :: a -> b -> GE EvtKey
evtKey a b = liftIO $ EvtKey <$> hash a <*> hash b
    where hash x = hashStableName <$> makeStableName x

