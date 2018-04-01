module Csound.Typed.Plugins.TapeEcho(
    tapeRead
  , tapeWrite
) where

import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(tapeEchoPlugin)

--  Function to read from tape.
--
--  tapeRead aIn, kDelay, kRandomSpread
--
-- The function is used in the same manner as deltapi
--  first init the delay buffer and the use tapeRead.
--
--  aIn - input signal
--  kDelay - delay time
--  kRandomSpread - [0, Inf] - the random spread of reading from the tape
--     the higher the worser the quality of the tape.
-- opcode tapeRead, a, akk
tapeRead :: Sig -> Sig -> Sig -> SE Sig
tapeRead ain kdel kRandomSpread = fmap (Sig . return) $ SE $ (depT =<<) $ lift $ do
  addUdoPlugin E.tapeEchoPlugin
  f <$> toGE ain <*> toGE kdel <*> toGE kRandomSpread
  where f ain kdel krand = opcs "tapeRead" [(Ar, [Ar, Kr, Kr])] [ain, kdel, krand]


-- function to write to tape
--
-- tapeWrite aIn, aOut, kFbGain
--
-- It should be though of as delayw for magnetic tape.
--
-- aIn - input signal
-- aOut - output signal
-- kFbGain - gain of feedback [0, 2]
tapeWrite :: Sig -> Sig -> Sig -> SE ()
tapeWrite ain aout kFeedback = SE $ (depT_ =<<) $ lift $ do
  f <$> toGE ain <*> toGE aout <*> toGE kFeedback
  where f ain aout kfb = opcs "tapeWrite" [(Xr, [Ar, Ar, Kr])] [ain, aout, kfb]

{-
addUdoPlugin E.pitchShifterDelayPlugin
    f <$> toGE ain <*> toGE ktrans <*> toGE kdlt <*> toGE kFB1 <*> toGE kFB2 <*> toGE imaxdlt
    where f ain ktrans kdlt kFB1 kFB2 imaxdlt = opcs "PitchShifterDelay" [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir])] [ain, ktrans, kdlt, kFB1, kFB2, imaxdlt]
-}