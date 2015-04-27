module Csound.Typed.Types.MixSco(
    M(..), CsdEventList, csdEventListDur, csdEventListNotes,
    delayAndRescaleCsdEventListM, renderMixSco, renderMixSco_
) where

import Control.Monad

import Csound.Dynamic
import Csound.Typed.GlobalState.Elements
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
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
    go chnId evts
    readChn chnId
    where 
        go :: ChnRef -> CsdEventList M -> Dep ()
        go outId xs = mapM_ (onEvent outId) $ csdEventListNotes xs

        onEvent :: ChnRef -> (D, D, M) -> Dep ()
        onEvent outId (start, dur, x) = case x of
            Snd instrId es          -> onSnd instrId outId es
            Eff instrId es arityIn  -> onEff instrId start dur outId es arityIn

        onSnd instrId outId es = forM_ (csdEventListNotes es) $ \(start, dur, args) -> 
            mkEvent instrId start dur (args ++ [chnRefId outId])

        onEff instrId start dur outId es arityIn = do
            inId <- chnRefAlloc arityIn
            mkEvent instrId start dur [chnRefId inId, chnRefId outId]            
            go inId es

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
