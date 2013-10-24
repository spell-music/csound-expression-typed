{-# Language FlexibleContexts #-}
module Csound.Typed.Control.Mix(
    Mix, 
    sco, eff, mix, mixBy,
    sco_, mix_, mixBy_,
    CsdSco(..), CsdEventList(..), CsdEvent
) where

import Control.Monad.IO.Class
import Data.Traversable
import System.Mem.StableName

import Csound.Dynamic hiding (Instr)
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

-- | Plays a bunch of notes with the given instrument.
--
-- > res = sco instrument scores 
sco :: (CsdSco f, Arg a, Sigs b) => (a -> SE b) -> f a -> f (Mix b)
sco instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached cacheName (funArity instr) (insExp instr)
    return $ Snd instrId events'

-- | Invokes a procedure for the given bunch of events.
sco_ :: (CsdSco f, Arg a) => (a -> SE ()) -> f a -> f (Mix Unit)
sco_ instr notes = wrapSco notes $ \events -> do
    events' <- traverse toNote events
    cacheName <- liftIO $ C.makeCacheName instr
    instrId <- saveSourceInstrCached_ cacheName (unitExp $ fmap (const unit) $ instr toArg)
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

-- | Renders a scores to the sound signals. we can use it inside the other instruments.
-- Warning: if we use a score that lasts for an hour in the note that lasts for 5 seconds
-- all the events would be generated, though we will hear only first five seconds.
-- So the semantics is good but implementation is inefficient for such a cases 
-- (consider event streams for such cases). 
mix :: (Sigs a, CsdSco f) => f (Mix a) -> a
mix a = flip apInstr unit $ do
    key <- mixKey a
    withCache getMixKey saveMixKey key $ 
        saveMixInstr (mixArity a) =<< toEventList a

-- | Imitates a closure for a bunch of notes to be played within another instrument. 
mixBy :: (Arg a, Sigs b, CsdSco f) => (a -> f (Mix b)) -> (a -> b)
mixBy evts args = flip apInstr args $ do
    key <- mixKey evts
    withCache getMixKey saveMixKey key $ 
        saveMixInstr (mixArityFun evts) =<< (toEventList $ evts toArg)

-- | Converts a bunch of procedures scheduled with scores to a single procedure.
mix_ :: (CsdSco f) => f (Mix Unit) -> SE ()
mix_ a = fromDep_ $ hideGEinDep $ do
    key <- mixKey a
    withCache getMixProcKey saveMixProcKey key $
        saveMixInstr_ =<< toEventList a

-- | Imitates a closure for a bunch of procedures to be played within another instrument. 
mixBy_ :: (Arg a, CsdSco f) => (a -> f (Mix Unit)) -> (a -> SE ())
mixBy_ evts args = mix_ $ evts args

----------------------------------------------------------

mixKey :: a -> GE MixKey
mixKey = liftIO . fmap (MixKey . hashStableName) . makeStableName

toEventList :: (CsdSco f) => f (Mix a) -> GE (CsdEventList M)
toEventList evts = fmap delayAndRescaleCsdEventListM $ traverse unMix $ toCsdEventList $ evts

mixArity :: Sigs b => f (Mix b) -> Int
mixArity = tupleArity . proxy
    where
        proxy :: f (Mix b) -> b
        proxy = const undefined

mixArityFun :: Sigs b => (a -> f (Mix b)) -> Int
mixArityFun = tupleArity . proxy
    where
        proxy :: (a -> f (Mix b)) -> b
        proxy = const undefined
