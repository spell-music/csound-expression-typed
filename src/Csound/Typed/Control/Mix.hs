{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Mix(
    Mix, sco, sco_, eff, mix, mix_, CsdSco(..), CsdEventList(..), CsdEvent
) where

import Control.Monad.IO.Class
import Data.Traversable

import Csound.Dynamic
import qualified Csound.Dynamic.Control as C

import Csound.Typed.Types
import Csound.Typed.Types.MixSco
import Csound.Typed.GlobalState
import Csound.Typed.Control.Instr

-- | Special type that represents a scores of sound signals.
-- If an instrument is triggered with the scores the result is wrapped
-- in the value of this type. 
newtype Mix a = Mix { unMix :: GE M } 

wrapSco :: (CsdSco f) => f a -> (CsdEventList a -> GE M) -> f (Mix b)
wrapSco notes getContent = singleCsdEvent (0, csdEventListDur evts, Mix $ getContent evts)
    where evts = toCsdEventList notes

-- | Play a bunch of notes with the given instrument.
--
-- > res = sco instrument scores 
--
-- * @instrument@ is a function that takes notes and produces a 
--   tuple of signals (maybe with some side effect)
--  
-- * @scores@ are some notes (see the type class 'Csound.Base.CsdSco')
--
-- Let's try to understand the type of the output. It's @CsdSco f => f (Mix (NoSE a))@. 
-- What does it mean? Let's look at the different parts of this type:
--
-- * @CsdSco f => f a@ - you can think of it as a container of some values of 
--   type @a@ (every value of type @a@ starts at some time and lasts 
--   for some time in seconds)
--
-- * @Mix a@ - is an output of Csound instrument it can be one or several 
--   signals ('Csound.Base.Sig' or 'Csound.Base.CsdTuple'). 
--
-- *NoSE a* - it's a tricky part of the output. 'NoSE' means literaly 'no SE'. 
-- It tells to the type checker that it can skip the 'Csound.Base.SE' wrapper
-- from the type 'a' so that @SE a@ becomes just @a@ or @SE (a, SE b, c)@ 
-- becomes @(a, b, c)@. Why should it be? We need 'SE' to deduce the order of the
-- opcodes that have side effects. We need it within one instrument. But when 
-- instrument is rendered we no longer need 'SE' type. So 'NoSE' lets me drop it
-- from the output type. 
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)
sco instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
    return $ Snd instrId events'

sco_ :: (CsdSco f, Arg a) => (a -> SE ()) -> f a -> f (Mix Unit)
sco_ instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached_ cacheName (unitExp $ unit $ instr toArg)
    return $ Snd instrId events'

-- | Applies an effect to the sound. Effect is applied to the sound on the give track. 
--
-- > res = eff effect sco 
--
-- * @effect@ - a function that takes a tuple of signals and produces 
--   a tuple of signals.
--
-- * @sco@ - something that is constructed with 'Csound.Base.sco' or 
--   'Csound.Base.eff'. 
--
-- With the function 'Csound.Base.eff' you can apply a reverb or adjust the 
-- level of the signal. It functions like a mixing board but unlike mixing 
-- board it produces the value that you can arrange with functions from your
-- favorite Score-generation library. You can delay it or mix with some other track and 
-- apply some another effect on top of it!
eff :: (CsdSco f, Sigs a, Sigs b) => (a -> SE b) -> f (Mix a) -> f (Mix b)
eff ef sigs = wrapSco sigs $ \events -> do
    notes <- traverse unMix events
    instrId <- saveEffectInstr (funArity ef) (effExp ef)
    return $ Eff instrId notes (arityIns $ funArity ef)

-- | Renders a scores to global variable that contains a resulting sound signals.
mix :: (Sigs a, CsdSco f) => f (Mix a) -> a
mix a = toTuple $ saveMixInstr (mixArity a) =<< 
    (fmap rescaleCsdEventListM $ traverse unMix $ toCsdEventList a)
    where 
        mixArity :: Sigs a => f (Mix a) -> Int
        mixArity = tupleArity . proxy
            where
                proxy :: f (Mix a) -> a
                proxy = const undefined

mix_ :: (CsdSco f) => f (Mix Unit) -> SE ()
mix_ a = fromDep_ $ saveMixInstr_ 
    =<< (fmap rescaleCsdEventListM $ traverse unMix $ toCsdEventList a)

