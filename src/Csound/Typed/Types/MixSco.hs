module Csound.Typed.Types.MixSco(
    M(..), delayAndRescaleCsdEventListM, renderMixSco, renderMixSco_
) where

import Control.Monad

import Csound.Dynamic
import Csound.Dynamic.Control

data M 
    = Snd InstrId (CsdEventList [E])
    | Eff InstrId (CsdEventList M) Int   

delayAndRescaleCsdEventListM :: CsdEventList M -> CsdEventList M
delayAndRescaleCsdEventListM = delayCsdEventListM . rescaleCsdEventListM

delayCsdEventListM :: CsdEventList M -> CsdEventList M
delayCsdEventListM es = 
    es { csdEventListNotes = fmap delayCsdEventM $ csdEventListNotes es }

delayCsdEventM :: CsdEvent M -> CsdEvent M
delayCsdEventM (start, dur, evt) = (start, dur, phi evt)
    where phi x = case x of
            Snd n evts          -> Snd n $ delayCsdEventList start evts
            Eff n evts arityIn  -> Eff n (delayCsdEventListM $ delayCsdEventList start evts) arityIn        

rescaleCsdEventListM :: CsdEventList M -> CsdEventList M
rescaleCsdEventListM es = 
    es { csdEventListNotes = fmap rescaleCsdEventM $ csdEventListNotes es }

rescaleCsdEventM :: CsdEvent M -> CsdEvent M
rescaleCsdEventM (start, dur, evt) = (start, dur, phi evt)
    where phi x = case x of
            Snd n evts          -> Snd n $ rescaleCsdEventList (dur/localDur) evts
            Eff n evts arityIn  -> Eff n (rescaleCsdEventListM $ rescaleCsdEventList (dur/localDur) evts) arityIn
            where localDur = case x of
                    Snd _ evts   -> csdEventListDur evts
                    Eff _ evts _ -> csdEventListDur evts

renderMixSco :: Monad m => Int -> CsdEventList M -> DepT m [E]
renderMixSco arity evts = do
    chnId <- chnRefAlloc arity
    go chnId evts
    readChn chnId
    where 
        go :: Monad m => ChnRef -> CsdEventList M -> DepT m ()
        go outId xs = mapM_ (onEvent outId) $ csdEventListNotes xs

        onEvent :: Monad m => ChnRef -> CsdEvent M -> DepT m ()
        onEvent outId (start, dur, x) = case x of
            Snd instrId es          -> onSnd instrId outId es
            Eff instrId es arityIn  -> onEff instrId start dur outId es arityIn

        onSnd instrId outId es = forM_ (csdEventListNotes es) $ \(start, dur, args) ->
            event_i $ Event instrId (double start) (double dur) (args ++ [chnRefId outId])

        onEff instrId start dur outId es arityIn = do
            inId <- chnRefAlloc arityIn
            event_i $ Event instrId (double start) (double dur) [chnRefId inId, chnRefId outId]
            go inId es

renderMixSco_ :: Monad m => CsdEventList M -> DepT m ()
renderMixSco_ evts = mapM_ onEvent $ csdEventListNotes evts
    where
        onEvent :: Monad m => CsdEvent M -> DepT m ()
        onEvent (start, dur, x) = case x of
            Snd instrId es      -> onSnd instrId es
            Eff instrId es _    -> onEff instrId start dur es

        onSnd instrId es = forM_ (csdEventListNotes es) $ \(start, dur, args) ->
            event_i $ Event instrId (double start) (double dur) args

        onEff instrId start dur es = do
            event_i $ Event instrId (double start) (double dur) []
            renderMixSco_ es

