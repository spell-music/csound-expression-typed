module Csound.Typed.Plugins.Audaciouseq(    
) where

import Data.Boolean
import Control.Monad.Trans.Class

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(audaciouseqPlugin)

-------------------------------------------------------------------------------

-- opcode audaciouseq, a, akkkkkkkkkk
--
-- ain, kgain1, kgain2, kgain3, kgain4, kgain5, 
--     kgain6, kgain7, kgain8, kgain9, kgain10 xin
audaciouseq :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
audaciouseq ain kgain1 kgain2 kgain3 kgain4 kgain5 kgain6 kgain7 kgain8 kgain9 kgain10 = fromGE $ do
    addUdoPlugin E.audaciouseqPlugin
    f <$> toGE ain <*> toGE kgain1 <*> toGE kgain2 <*> toGE kgain3 <*> toGE kgain4 <*> toGE kgain5 <*> toGE kgain6 <*> toGE kgain7 <*> toGE kgain8 <*> toGE kgain9 <*> toGE kgain10
    where f ain kgain1 kgain2 kgain3 kgain4 kgain5 kgain6 kgain7 kgain8 kgain9 kgain10 = opcs "audaciouseq" [(Ar, [Ar, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr, Kr])] [ain, kgain1, kgain2, kgain3, kgain4, kgain5, kgain6, kgain7, kgain8, kgain9, kgain10]
