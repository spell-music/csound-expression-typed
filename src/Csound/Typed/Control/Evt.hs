{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Evt(
    trigs, scheds, schedHarps, retrigs, evtLoop,
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
import Csound.Typed.Constants(infiniteDur)

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
            
retrigs :: (Arg a, Sigs b) => (a -> SE b) -> Evt [a] -> b
retrigs instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatchAndRetrig cacheName (funArity instr) (insExp instr)
        saveRetrigEvtInstr (arityOuts $ funArity instr) instrId evts
   
saveRetrigEvtInstr :: Arg a => Int -> C.InstrId -> Evt [a] -> GE C.InstrId
saveRetrigEvtInstr arity instrId evts = saveInstr $ do
    aliveCountRef  <- newSERef (10 :: D)
    retrigWatchRef <- newSERef (0  :: D)
    evtMixInstr aliveCountRef retrigWatchRef
    where
        evtMixInstr :: SERef D -> SERef D -> SE ()
        evtMixInstr aliveCountRef retrigWatchRef = do
            chnId <- fromDep $ C.chnRefAlloc arity
            go aliveCountRef retrigWatchRef chnId evts
            fromDep_ $ hideGEinDep $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 
            aliveCount <- readSERef aliveCountRef
            fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnAlive chnId $ toGE aliveCount                         

        go :: Arg a => SERef D -> SERef D -> GE C.ChnRef -> Evt [a] -> SE ()
        go aliveCountRef retrigWatchRef mchnId events = 
            runEvt events $ \es -> do
                writeSERef aliveCountRef $ int $ 2 * length es                
                modifySERef retrigWatchRef (+ 1)
                chnId <- geToSe mchnId
                currentRetrig <- readSERef retrigWatchRef
                fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnRetrig mchnId $ toGE currentRetrig                                
                fromDep_ $ mapM_ (event chnId currentRetrig) es
    
        event :: Arg a => C.ChnRef -> D -> a -> Dep ()
        event chnId currentRetrig args = hideGEinDep $ fmap C.event $ do
            currentRetrigExp <- toGE currentRetrig
            C.Event instrId 0 infiniteDur <$> (fmap (++ [C.chnRefId chnId, currentRetrigExp]) $ toNote args) 

evtLoop :: Sigs a => SE a -> Evt Unit -> a
evtLoop instr evts = apInstr0 $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr        
        (instrId, evtInstrId) <- saveSourceInstrCachedWithLivenessWatchAndRetrigAndEvtLoop cacheName (constArity instr) (insExp $ toInstrExp instr) (evtLoopInstr evts)
        saveEvtLoopInstr (arityOuts $ constArity instr) instrId evtInstrId
    where 
        toInstrExp :: a -> (Unit -> a)
        toInstrExp = const

evtLoopInstr :: Evt Unit -> SE ()
evtLoopInstr evts = do
    runEvt evts $ const $ fromDep_ $ servantUpdateChnEvtLoop (C.chnPargId $ 0)

saveEvtLoopInstr :: Int -> C.InstrId -> C.InstrId -> GE C.InstrId
saveEvtLoopInstr arity instrId evtInstrId = saveInstr $ do
    aliveCountRef  <- newSERef (10 :: D)
    retrigWatchRef <- newSERef (0  :: D)        
    evtMixInstr aliveCountRef retrigWatchRef
    where
        evtMixInstr :: SERef D -> SERef D -> SE ()
        evtMixInstr aliveCountRef retrigWatchRef = do
            chnId <- fromDep $ C.chnRefAlloc arity
            initStartInstrs chnId
            masterEvt <- fmap (sigToEvt . fromGE . fmap C.changed . toGE) $ readServantEvt chnId
            go aliveCountRef retrigWatchRef chnId masterEvt
            fromDep_ $ hideGEinDep $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 
            aliveCount <- readSERef aliveCountRef
            fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnAlive chnId $ toGE aliveCount           

        go :: SERef D -> SERef D -> GE C.ChnRef -> Evt Unit -> SE ()
        go aliveCountRef retrigWatchRef mchnId events = 
            runEvt events $ \es -> do
                writeSERef aliveCountRef 2                
                modifySERef retrigWatchRef (+ 1)
                chnId <- geToSe mchnId
                currentRetrig <- readSERef retrigWatchRef
                fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnRetrig mchnId $ toGE currentRetrig                                
                fromDep_ $ event chnId currentRetrig
                fromDep_ $ startEvtInstr chnId
    
        event :: C.ChnRef -> D -> Dep ()
        event chnId currentRetrig = hideGEinDep $ fmap C.event $ do
            currentRetrigExp <- toGE currentRetrig
            return $ eventForAudioInstr chnId currentRetrigExp           

        startEvtInstr chnId = C.event $ C.Event evtInstrId 0 infiniteDur [C.chnRefId chnId]

        initStartInstrs mchnId = fromDep_ $ hideGEinDep $ do
            chnId <- mchnId
            return $ initStartEvtInstr   chnId >> initStartAudioInstr chnId

        initStartEvtInstr   chnId = C.event_i $ eventForEvtInstr chnId
        initStartAudioInstr chnId = C.event_i $ eventForAudioInstr chnId 0

        eventForEvtInstr chnId = C.Event evtInstrId 0 infiniteDur [C.chnRefId chnId]
        eventForAudioInstr chnId currentRetrig = 
            C.Event instrId 0 infiniteDur [C.chnRefId chnId, currentRetrig] 

        readServantEvt :: GE C.ChnRef -> SE Sig
        readServantEvt chnId = SE $ fmap fromE $ hideGEinDep $ fmap readChnEvtLoop chnId

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
    where phi a = (0, infiniteDur, a)

-- | A closure to trigger an instrument inside the body of another instrument.
schedHarpsBy :: (Arg a, Sigs b, Arg c) => D -> (a -> SE b) -> (c -> Evt [a]) -> (c -> b)
schedHarpsBy turnOffTime instr evts args = flip apInstr args $ do
    key <- evtKey evts instr
    withCache InfiniteDur getEvtKey saveEvtKey key $ do
        cacheName <- liftIO $ C.makeCacheName instr
        instrId <- saveSourceInstrCachedWithLivenessWatch cacheName (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
        saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) $ evts toArg)
    where phi a = (0, infiniteDur, a)

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


-------------------------------------------------------------------
-- sample level triggering

samNext :: (Sigs a) => Evt Unit -> a -> a -> a
samNext = undefined

samLoop :: (Sigs a) => Evt Unit -> a -> a
samLoop = undefined

