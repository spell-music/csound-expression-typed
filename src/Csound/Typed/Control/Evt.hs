{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Evt(
    trig, sched, schedHarp, 
    trigBy, schedBy, schedHarpBy,
    trig_, sched_,
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad.IO.Class

import qualified Csound.Dynamic as C
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

-------------------------------------------------
-- triggereing the events

-- | Triggers an instrument with an event stream. The event stream
-- contains triples:
--
-- > (delay_after_event_is_fired, duration_of_the_event, argument_for_the_instrument)
trig :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, D, a) -> b
trig instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId evts

-- | A closure to trigger an instrument inside the body of another instrument.
trigBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, D, a)) -> (c -> b)
trigBy instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache getEvtKey saveEvtKey key $ do        
        cacheName <- liftIO $ C.makeCacheName instr
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

-- | It's like the function @trig@, but delay is set to zero.
sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (D, a) -> b
sched instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi evts)  
    where phi (a, b) = (0, a, b)
     
-- | A closure to trigger an instrument inside the body of another instrument.
schedBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (D, a)) -> (c -> b)
schedBy instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi $ evts toArg)  
    where phi (a, b) = (0, a, b)

-- | An instrument is triggered with event stream and delay time is set to zero 
-- (event fires immediately) and duration is set to inifinite time. The note is 
-- held while the instrument is producing something. If the instrument is silent
-- for some seconds (specified in the first argument) then it's turned off.
schedHarp :: (Arg a, Sigs b) => D -> (a -> SE b) -> Evt a -> b
schedHarp turnOffTime instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi evts)
    where phi a = (0, -1, a)

-- | A closure to trigger an instrument inside the body of another instrument.
schedHarpBy :: (Arg a, Sigs b, Arg c) => D -> (a -> SE b) -> (c -> Evt a) -> (c -> b)
schedHarpBy turnOffTime instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap phi $ evts toArg)
    where phi a = (0, -1, a)

autoOff :: Sigs a => D -> a -> SE a
autoOff dt sigs = fmap toTuple $ fromDep $ phi =<< fromTuple sigs
    where 
        phi x = do
            dtE <- toGE dt
            return $ C.autoOff dtE x

-----------------------------------------------------------------------

-- | Triggers a procedure on the event stream.
trig_ :: (Arg a) => (a -> SE ()) -> Evt (D, D, a) -> SE ()
trig_ instr evts = fromDep_ $ do
    key <- evtKey evts instr
    withCache getEvtProcKey saveEvtProcKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached_ cacheName (unitExp $ fmap (const unit) $ instr toArg)
        saveEvtInstr_ instrId evts

-- | Triggers a procedure on the event stream. A delay time is set to zero.
sched_ :: (Arg a) => (a -> SE ()) -> Evt (D, a) -> SE ()
sched_ instr evts = fromDep_ $ do
    key <- evtKey evts instr
    withCache getEvtProcKey saveEvtProcKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached_ cacheName (unitExp $ fmap (const unit) $ instr toArg)
        saveEvtInstr_ instrId $ fmap phi evts
    where phi (a, b) = (0, a, b)

saveEvtInstr_ :: Arg a => C.InstrId -> Evt (D, D, a) -> GE (C.Dep ())
saveEvtInstr_ instrId evts = execSE $ runEvt evts $ \(start, dur, args) -> fromDep_ $ 
    fmap C.event $ C.Event instrId <$> toGE start <*> toGE dur <*> toNote args

-------------------------------------------------------------------

evtKey :: a -> b -> GE EvtKey
evtKey a b = liftIO $ EvtKey <$> hash a <*> hash b
    where hash x = hashStableName <$> makeStableName x

