module Csound.Typed.Types.MixSco(
    M(..), CsdEventList, csdEventListDur, csdEventListNotes,
    delayAndRescaleCsdEventListM, renderMixSco, renderMixSco_
) where

import Control.Applicative
import Control.Monad

import Csound.Dynamic hiding (int)
import Csound.Typed.GlobalState.Elements
import Csound.Typed.GlobalState.Opcodes
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.Control.Ref
import Csound.Typed.Types.Prim

import qualified Temporal.Media as T

type CsdEventList a = T.Track D a

csdEventListNotes :: CsdEventList a -> [(D, D, a)]
csdEventListNotes a = fmap (\(T.Event start dur content) -> (start, dur, content)) $ T.render a

csdEventListDur :: CsdEventList a -> D
csdEventListDur = T.dur

rescaleCsdEventList :: D -> CsdEventList a -> CsdEventList a
rescaleCsdEventList = T.str

delayCsdEventList :: D -> CsdEventList a -> CsdEventList a
delayCsdEventList = T.del

data M 
    = Snd InstrId (CsdEventList [E])
    | Eff InstrId (CsdEventList M) Int   

delayAndRescaleCsdEventListM :: CsdEventList M -> CsdEventList M
delayAndRescaleCsdEventListM = delayCsdEventListM . rescaleCsdEventListM

delayCsdEventListM :: CsdEventList M -> CsdEventList M
delayCsdEventListM = T.mapEvents delayCsdEventM    

delayCsdEventM :: T.Event D M -> T.Event D M
delayCsdEventM (T.Event start dur evt) = T.Event start dur (phi evt)
    where phi x = case x of
            Snd n evts          -> Snd n $ delayCsdEventList start evts
            Eff n evts arityIn  -> Eff n (delayCsdEventListM $ delayCsdEventList start evts) arityIn        

rescaleCsdEventListM :: CsdEventList M -> CsdEventList M
rescaleCsdEventListM = T.mapEvents rescaleCsdEventM    

rescaleCsdEventM :: T.Event D M -> T.Event D M
rescaleCsdEventM (T.Event start dur evt) = T.Event start dur (phi evt)
    where phi x = case x of
            Snd n evts          -> Snd n $ rescaleCsdEventList (dur/localDur) evts
            Eff n evts arityIn  -> Eff n (rescaleCsdEventListM $ rescaleCsdEventList (dur/localDur) evts) arityIn
            where localDur = case x of
                    Snd _ evts   -> csdEventListDur evts
                    Eff _ evts _ -> csdEventListDur evts

renderMixSco :: Int -> CsdEventList M -> Dep [E]
renderMixSco arity evts = do
    chnId <- chnRefAlloc arity  
    aliveCountRef <- unSE $ newRef (10 :: D)  
    go aliveCountRef chnId evts    
    readChn chnId
    where 
        go :: Ref D -> ChnRef -> CsdEventList M -> Dep ()
        go aliveCountRef outId xs = do
            mapM_ (onEvent aliveCountRef outId) notes
            unSE $ writeRef aliveCountRef $ int $ 2 * length notes
            aliveCount <- unSE $ readRef aliveCountRef
            hideGEinDep $ liftA2 masterUpdateChnAlive (return chnId) $ toGE aliveCount 
            where 
                notes = csdEventListNotes xs
                chnId = outId

        onEvent :: Ref D -> ChnRef -> (D, D, M) -> Dep ()
        onEvent aliveCountRef outId (start, dur, x) = case x of
            Snd instrId es          -> onSnd aliveCountRef instrId outId es
            Eff instrId es arityIn  -> onEff aliveCountRef instrId start dur outId es arityIn

        onSnd _ instrId outId es = forM_ (csdEventListNotes es) $ \(start, dur, args) -> 
            mkEvent instrId start dur (args ++ [chnRefId outId])

        onEff aliveCountRef instrId start dur outId es arityIn = do
            inId <- chnRefAlloc arityIn
            mkEvent instrId start dur [chnRefId inId, chnRefId outId]            
            go aliveCountRef inId es

renderMixSco_ :: CsdEventList M -> Dep ()
renderMixSco_ evts = mapM_ onEvent $ csdEventListNotes evts
    where
        onEvent :: (D, D, M) -> Dep ()
        onEvent (start, dur, x) = case x of
            Snd instrId es      -> onSnd instrId es
            Eff instrId es _    -> onEff instrId start dur es

        onSnd instrId es = forM_ (csdEventListNotes es) $ \(start, dur, args) -> 
            mkEvent instrId start dur args

        onEff instrId start dur es = do
            mkEvent instrId start dur [] 
            renderMixSco_ es


mkEvent :: InstrId -> D -> D -> [E] -> Dep ()
mkEvent instrId startD durD args =  hideGEinDep $ do
        start <- toGE startD
        dur   <- toGE durD
        return $ event_i $ Event instrId start dur args
