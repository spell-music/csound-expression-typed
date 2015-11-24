{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Evt(
    sched, sched_, schedBy, schedHarp, schedHarpBy,
    retrigs, evtLoop, evtLoopOnce    
) where

import System.Mem.StableName

import Data.Boolean

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Temporal.Media as T(render, Event(..))

import qualified Csound.Dynamic as C
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Opcodes(primInstrId)
import Csound.Typed.Control.Instr
import Csound.Typed.Control.Mix(Sco)

import Csound.Typed.Control.Ref
import Csound.Typed.Constants(infiniteDur)

renderEvts :: Evt (Sco a) -> Evt [(D, D, a)]
renderEvts = fmap (fmap unEvt . T.render)
    where unEvt e = (T.eventStart e, T.eventDur e, T.eventContent e)

sched :: (Arg a, Sigs b) => (a -> SE b) -> Evt (Sco a) -> b
sched instr evts = apInstr0 $ do    
    instrId <- saveSourceInstrCachedWithLivenessWatch (funArity instr) (insExp instr)
    saveEvtInstr (arityOuts $ funArity instr) instrId (renderEvts evts)
    where unEvt e = (T.eventStart e, T.eventDur e, T.eventContent e)

-- | Triggers a procedure on the event stream.
sched_ :: (Arg a) => (a -> SE ()) -> Evt (Sco a) -> SE ()
sched_ instr evts = fromDep_ $ hideGEinDep $ do
    instrId <- saveSourceInstrCached_ (unitExp $ fmap (const unit) $ instr toArg)
    return $ saveEvtInstr_ instrId (renderEvts evts)

-- | A closure to trigger an instrument inside the body of another instrument.
schedBy :: (Arg a, Sigs b, Arg c) => (a -> SE b) -> (c -> Evt (Sco a)) -> (c -> b)
schedBy instr evts args = flip apInstr args $ do
    instrId <- saveSourceInstrCachedWithLivenessWatch (funArity instr) (insExp instr)
    saveEvtInstr (arityOuts $ funArity instr) instrId (renderEvts $ evts toArg)  

-------------------------------------------------
-- triggereing the events

saveEvtInstr :: Arg a => Int -> C.InstrId -> Evt [(D, D, a)] -> GE C.InstrId
saveEvtInstr arity instrId evts = saveInstr $ do
    aliveCountRef <- newRef (10 :: D)
    evtMixInstr aliveCountRef
    where
        evtMixInstr :: Ref D -> SE ()
        evtMixInstr aliveCountRef = do
            chnId <- fromDep $ C.chnRefAlloc arity
            go aliveCountRef chnId evts
            fromDep_ $ hideGEinDep $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 
            aliveCount <- readRef aliveCountRef
            fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnAlive chnId $ toGE aliveCount 

        go :: Arg a => Ref D -> GE C.ChnRef -> Evt [(D, D, a)] -> SE ()
        go aliveCountRef mchnId events = 
            runEvt events $ \es -> do
                writeRef aliveCountRef $ int $ 2 * length es
                chnId <- geToSe mchnId
                fromDep_ $ mapM_ (event chnId) es
    
        event :: Arg a => C.ChnRef -> (D, D, a) -> Dep ()
        event chnId (start, dur, args) = hideGEinDep $ fmap C.event $ 
            C.Event (primInstrId instrId) <$> toGE start <*> toGE dur <*> (fmap (++ [C.chnRefId chnId]) $ toNote args) 
            
-- | Retriggers an instrument every time an event happens. The note
-- is held until the next event happens.
retrigs :: (Arg a, Sigs b) => (a -> SE b) -> Evt [a] -> b
retrigs instr evts = apInstr0 $ do
    instrId <- saveSourceInstrCachedWithLivenessWatchAndRetrig (funArity instr) (insExp instr)
    saveRetrigEvtInstr (arityOuts $ funArity instr) instrId evts
   
saveRetrigEvtInstr :: Arg a => Int -> C.InstrId -> Evt [a] -> GE C.InstrId
saveRetrigEvtInstr arity instrId evts = saveInstr $ do
    aliveCountRef  <- newRef (10 :: D)
    retrigWatchRef <- newRef (0  :: D)
    evtMixInstr aliveCountRef retrigWatchRef
    where
        evtMixInstr :: Ref D -> Ref D -> SE ()
        evtMixInstr aliveCountRef retrigWatchRef = do
            chnId <- fromDep $ C.chnRefAlloc arity
            go aliveCountRef retrigWatchRef chnId evts
            fromDep_ $ hideGEinDep $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 
            aliveCount <- readRef aliveCountRef
            fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnAlive chnId $ toGE aliveCount                         

        go :: Arg a => Ref D -> Ref D -> GE C.ChnRef -> Evt [a] -> SE ()
        go aliveCountRef retrigWatchRef mchnId events = 
            runEvt events $ \es -> do
                writeRef aliveCountRef $ int $ 2 * length es                
                modifyRef retrigWatchRef (+ 1)
                chnId <- geToSe mchnId
                currentRetrig <- readRef retrigWatchRef
                fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnRetrig mchnId $ toGE currentRetrig                                
                fromDep_ $ mapM_ (event chnId currentRetrig) es
    
        event :: Arg a => C.ChnRef -> D -> a -> Dep ()
        event chnId currentRetrig args = hideGEinDep $ fmap C.event $ do
            currentRetrigExp <- toGE currentRetrig
            C.Event (primInstrId instrId) 0 infiniteDur <$> (fmap (++ [C.chnRefId chnId, currentRetrigExp]) $ toNote args) 

evtLoop :: (Num a, Tuple a, Sigs a) => Maybe (Evt Unit) -> [SE a] -> [Evt Unit] -> a
evtLoop = evtLoopGen True

evtLoopOnce :: (Num a, Tuple a, Sigs a) => Maybe (Evt Unit) -> [SE a] -> [Evt Unit] -> a
evtLoopOnce = evtLoopGen False

evtLoopGen :: (Num a, Tuple a, Sigs a) => Bool -> Maybe (Evt Unit) -> [SE a] -> [Evt Unit] -> a
evtLoopGen mustLoop maybeOffEvt instrs evts = apInstr0 $ do
    (instrId, evtInstrId) <- saveSourceInstrCachedWithLivenessWatchAndRetrigAndEvtLoop (constArity instr) (insExp $ toInstrExp instr) (toSingleEvt evts)
    saveEvtLoopInstr mustLoop loopLength maybeOffEvt (arityOuts $ constArity instr) instrId evtInstrId
    where         
        loopLength = int $ lcm (length instrs) (length evts)
        instr = toSingleInstr instrs

        toInstrExp :: a -> (Unit -> a)
        toInstrExp = const

        toSingleInstr :: (Num a, Tuple a) => [SE a] -> SE a
        toSingleInstr as = do
            let n = mod' (fromE $ getRetrigVal 4) (sig $ int $ length as)
            ref <- newRef 0
            zipWithM_ (f ref n) (fmap (sig . int) [0 .. ]) as
            readRef ref
            where 
                f :: Tuple a => Ref a -> Sig -> Sig -> SE a -> SE ()
                f ref n ix a = when1 (n ==* ix) $ writeRef ref =<< a

        toSingleEvt :: [Evt Unit] -> SE ()
        toSingleEvt evts = do
            let n = mod' (fromE $ getRetrigVal 4) (sig $ int $ length evts)
            zipWithM_ (f n) (fmap (sig . int) [0 .. ]) evts
            where 
                f :: Sig -> Sig -> Evt Unit -> SE ()
                f n ix evt = when1 (n ==* ix) $ evtLoopInstr evt

evtLoopInstr :: Evt Unit -> SE ()
evtLoopInstr evts = do
    runEvt evts $ const $ fromDep_ $ servantUpdateChnEvtLoop (C.chnPargId $ 0)

saveEvtLoopInstr :: Bool -> D -> Maybe (Evt Unit) -> Int -> C.InstrId -> C.InstrId -> GE C.InstrId
saveEvtLoopInstr mustLoop loopLength maybeOffEvt arity instrId evtInstrId = saveInstr $ do
    aliveCountRef  <- newRef (10 :: D)
    retrigWatchRef <- newRef (0  :: D)        
    evtMixInstr aliveCountRef retrigWatchRef
    where
        evtMixInstr :: Ref D -> Ref D -> SE ()
        evtMixInstr aliveCountRef retrigWatchRef = do
            chnId <- fromDep $ C.chnRefAlloc arity
            initStartInstrs chnId
            isOn <- fmap sig $ case maybeOffEvt of
                Nothing     -> return 1
                Just offEvt -> do
                    isOn <- newRef (1 :: D)                    
                    runEvt offEvt $ const $ do
                        writeRef isOn 0
                        modifyRef retrigWatchRef (+ 1)
                        currentRetrig <- readRef retrigWatchRef
                        fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnRetrig chnId $ toGE currentRetrig                        
                    readRef isOn         

            masterEvt <- fmap (sigToEvt . (* isOn) . fromGE . fmap C.changed . toGE) $ readServantEvt chnId
            go aliveCountRef retrigWatchRef chnId masterEvt
            fromDep_ $ hideGEinDep $ fmap (\chn -> C.sendOut arity =<< C.readChn chn) chnId 
            aliveCount <- readRef aliveCountRef
            fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnAlive chnId $ toGE aliveCount    

        go = goBy (+ 1)   

        goBy :: (D -> D) -> Ref D -> Ref D -> GE C.ChnRef -> Evt Unit -> SE ()
        goBy updateRetrig aliveCountRef retrigWatchRef mchnId events = 
            runEvt events $ \es -> do                
                modifyRef retrigWatchRef updateRetrig
                chnId <- geToSe mchnId
                currentRetrig <- readRef retrigWatchRef
                if not mustLoop 
                    then do
                        when1 (sig currentRetrig >=* (sig loopLength)) $ do
                            fromDep_ turnoff
                    else return ()
                fromDep_ $ hideGEinDep $ liftA2 masterUpdateChnRetrig mchnId $ toGE currentRetrig                                
                audioEvent chnId currentRetrig
                evtEvent chnId currentRetrig  



        fireEventFor :: (C.ChnRef -> E -> C.Event) -> C.ChnRef -> D -> SE ()
        fireEventFor f chnId currentRetrig = fromDep_ $ hideGEinDep $ fmap C.event $ do
            currentRetrigExp <- toGE currentRetrig
            return $ f chnId currentRetrigExp           

        audioEvent = fireEventFor eventForAudioInstr
        evtEvent   = fireEventFor eventForEvtInstr

        startEvtInstr chnId currentRetrig = C.event $ eventForEvtInstr chnId currentRetrig

        initStartInstrs mchnId = fromDep_ $ hideGEinDep $ do
            chnId <- mchnId
            return $ initStartEvtInstr   chnId >> initStartAudioInstr chnId

        initStartEvtInstr   chnId = C.event_i $ eventForEvtInstr chnId 0 
        initStartAudioInstr chnId = C.event_i $ eventForAudioInstr chnId 0

        eventForEvtInstr   = eventFor evtInstrId
        eventForAudioInstr = eventFor instrId

        eventFor idx chnId currentRetrig = 
            C.Event (primInstrId idx) 0 infiniteDur [C.chnRefId chnId, currentRetrig] 

        readServantEvt :: GE C.ChnRef -> SE Sig
        readServantEvt chnId = SE $ fmap fromE $ hideGEinDep $ fmap readChnEvtLoop chnId


-- | An instrument is triggered with event stream and delay time is set to zero 
-- (event fires immediately) and duration is set to inifinite time. The note is 
-- held while the instrument is producing something. If the instrument is silent
-- for some seconds (specified in the first argument) then it's turned off.
schedHarp :: (Arg a, Sigs b) => D -> (a -> SE b) -> Evt [a] -> b
schedHarp turnOffTime instr evts = apInstr0 $ do
    instrId <- saveSourceInstrCachedWithLivenessWatch (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
    saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) evts)
    where phi a = (0, infiniteDur, a)

-- | A closure to trigger an instrument inside the body of another instrument.
schedHarpBy :: (Arg a, Sigs b, Arg c) => D -> (a -> SE b) -> (c -> Evt [a]) -> (c -> b)
schedHarpBy turnOffTime instr evts args = flip apInstr args $ do
    instrId <- saveSourceInstrCachedWithLivenessWatch (funArity instr) (insExp $ (autoOff turnOffTime =<< ) . instr)
    saveEvtInstr (arityOuts $ funArity instr) instrId (fmap (fmap phi) $ evts toArg)
    where phi a = (0, infiniteDur, a)

autoOff :: Sigs a => D -> a -> SE a
autoOff dt sigs = fmap toTuple $ fromDep $ hideGEinDep $ phi =<< fromTuple sigs
    where 
        phi x = do
            dtE <- toGE dt
            return $ C.autoOff dtE x


saveEvtInstr_ :: Arg a => C.InstrId -> Evt [(D, D, a)] -> Dep ()
saveEvtInstr_ instrId evts = unSE $ runEvt evts $ \es -> fromDep_ $ mapM_ event es
    where event (start, dur, args) = hideGEinDep $ fmap C.event $ C.Event (primInstrId instrId) <$> toGE start <*> toGE dur <*> toNote args

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

