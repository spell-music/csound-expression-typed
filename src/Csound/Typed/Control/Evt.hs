{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Evt(
    trigs, scheds, schedHarps, 
    trigsBy, schedsBy, schedHarpsBy,
    trigs_, scheds_,
) where

import System.Mem.StableName

import Control.Applicative
import Control.Monad.IO.Class

import qualified Csound.Dynamic as C
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

import Csound.Typed.Control.SERef

-------------------------------------------------
-- triggereing the events

-- | Triggers an instrument with an event stream. The event stream
-- contains triples:
--
-- > (delay_after_event_is_fired, duration_of_the_event, argument_for_the_instrument)
trigs :: (Arg a, Sigs b) => (a -> SE b) -> Evt [(D, D, a)] -> b
trigs instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId evts

-- | A closure to trigger an instrument inside the body of another instrument.
trigsBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt [(D, D, a)]) -> (c -> b)
trigsBy instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do        
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (evts toArg)  

saveEvtInstr :: Arg a => Int -> C.InstrId -> Evt [(D, D, a)] -> GE C.InstrId
saveEvtInstr arity instrId evts = saveInstr $ do
    aliveCountRef <- newSERef (10 :: D)
    evtMixInstr aliveCountRef
    where
        evtMixInstr :: SERef D -> SE ()
        evtMixInstr aliveCountRef = do
            chnId <- fromDep $ C.chnRefAlloc arity
            go aliveCountRef chnId evts
            fromDep_ $ hideGEinDep $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 
            aliveCount <- readSERef aliveCountRef
            fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnAlive chnId $ toGE aliveCount 

        go :: Arg a => SERef D -> GE C.ChnRef -> Evt [(D, D, a)] -> SE ()
        go aliveCountRef mchnId events = 
            runEvt events $ \es -> do
                writeSERef aliveCountRef $ int $ 2 * length es
                chnId <- geToSe mchnId
                fromDep_ $ mapM_ (event chnId) es
    
        event :: Arg a => C.ChnRef -> (D, D, a) -> Dep ()
        event chnId (start, dur, args) = hideGEinDep $ fmap C.event $ 
            C.Event instrId <$> toGE start <*> toGE dur <*> (fmap (++ [C.chnRefId chnId]) $ toNote args) 
            

-- | It's like the function @trigs@, but delay is set to zero.
scheds :: (Arg a, Sigs b) => (a -> SE b) -> Evt [(D, a)] -> b
scheds instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) evts)  
    where phi (a, b) = (0, a, b)
     
-- | A closure to trigger an instrument inside the body of another instrument.
schedsBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt [(D, a)]) -> (c -> b)
schedsBy instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) $ evts toArg)  
    where phi (a, b) = (0, a, b)

-- | An instrument is triggered with event stream and delay time is set to zero 
-- (event fires immediately) and duration is set to inifinite time. The note is 
-- held while the instrument is producing something. If the instrument is silent
-- for some seconds (specified in the first argument) then it's turned off.
schedHarps :: (Arg a, Sigs b) => D -> (a -> SE b) -> Evt [a] -> b
schedHarps turnOffTime instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) evts)
    where phi a = (0, -1, a)

-- | A closure to trigger an instrument inside the body of another instrument.
schedHarpsBy :: (Arg a, Sigs b, Arg c) => D -> (a -> SE b) -> (c -> Evt [a]) -> (c -> b)
schedHarpsBy turnOffTime instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) $ evts toArg)
    where phi a = (0, -1, a)

autoOff :: Sigs a => D -> a -> SE a
autoOff dt sigs = fmap toTuple $ fromDep $ hideGEinDep $ phi =<< fromTuple sigs
    where 
        phi x = do
            dtE <- toGE dt
            return $ C.autoOff dtE x

-----------------------------------------------------------------------

-- | Triggers a procedure on the event stream.
trigs_ :: (Arg a) => (a -> SE ()) -> Evt [(D, D, a)] -> SE ()
trigs_ instr evts = fromDep_ $ hideGEinDep $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtProcKey saveEvtProcKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached_ cacheName (unitExp $ fmap (const unit) $ instr toArg)
        return $ saveEvtInstr_ instrId evts

-- | Triggers a procedure on the event stream. A delay time is set to zero.
scheds_ :: (Arg a) => (a -> SE ()) -> Evt [(D, a)] -> SE ()
scheds_ instr evts = fromDep_ $ hideGEinDep $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtProcKey saveEvtProcKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCached_ cacheName (unitExp $ fmap (const unit) $ instr toArg)
        return $ saveEvtInstr_ instrId $ fmap (fmap phi) evts
    where phi (a, b) = (0, a, b)

saveEvtInstr_ :: Arg a => C.InstrId -> Evt [(D, D, a)] -> Dep ()
saveEvtInstr_ instrId evts = unSE $ runEvt evts $ \es -> fromDep_ $ mapM_ event es
    where event (start, dur, args) = hideGEinDep $ fmap C.event $ C.Event instrId <$> toGE start <*> toGE dur <*> toNote args

-------------------------------------------------------------------

evtKey :: a -> b -> GE EvtKey
evtKey a b = liftIO $ EvtKey <$> hash a <*> hash b
    where hash x = hashStableName <$> makeStableName x

