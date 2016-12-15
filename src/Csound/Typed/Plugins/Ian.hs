module Csound.Typed.Plugins.Ian(  
    pitchShifterDelay
) where

import Data.Boolean
import Control.Monad.Trans.Class

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(pitchShifterDelayPlugin)

pitchShifterDelay :: D -> (Sig, Sig) -> Sig -> Sig -> Sig -> Sig
pitchShifterDelay imaxdlt (fb1, fb2) kdel ktrans ain = csdPitchShifterDelay ain ktrans kdel fb1 fb2 imaxdlt

-- | PitchShifterDelay
-- ; ----------------
-- ; A pitch shifter effect that employs delay lines
-- ;
-- ; aout  PitchShifterDelay  ain,ktrans,kdlt,kFB1,kFB2,imaxdlt
--;
--; Initialisation
--; --------------
--; imaxdlt --  maximum delay time (kdlt should not exceed this value)
--;
--; Performance
--; -----------
--; ain     --  input audio to be pitch shifted
--; ktrans  --  pitch transposition (in semitones)
--; kdlt    --  delay time employed by the pitch shifter effect (should be within the range ksmps/sr and imaxdlt) 
--; kFB1    --  feedback using method 1 (output from delay taps are fed back directly into their own buffers before enveloping and mixing)
--; kFB2    --  feedback using method 2 (enveloped and mixed output from both taps is fed back into both buffers)
-- 
-- opcode  PitchShifterDelay,a,akkkki
csdPitchShifterDelay :: Sig -> Sig -> Sig -> Sig -> Sig -> D -> Sig
csdPitchShifterDelay ain ktrans kdlt kFB1 kFB2 imaxdlt = fromGE $ do
    addUdoPlugin E.pitchShifterDelayPlugin
    f <$> toGE ain <*> toGE ktrans <*> toGE kdlt <*> toGE kFB1 <*> toGE kFB2 <*> toGE imaxdlt
    where f ain ktrans kdlt kFB1 kFB2 imaxdlt = opcs "PitchShifterDelay" [(Ar, [Ar, Kr, Kr, Kr, Kr, Ir])] [ain, ktrans, kdlt, kFB1, kFB2, imaxdlt]
