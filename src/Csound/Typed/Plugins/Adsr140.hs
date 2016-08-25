module Csound.Typed.Plugins.Adsr140(    
) where

import Data.Boolean
import Control.Monad.Trans.Class

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(zdfPlugin)

-------------------------------------------------------------------------------

-- /* Gated, Re-triggerable ADSR modeled after the Doepfer A-140 */
-- opcode adsr140, a, aakkkk
--
-- agate, aretrig, kattack, kdecay, ksustain, krelease xin
adsr140 :: Sig -> Sig -> Sig -> Sig -> Sig -> Sig
adsr140 aretrig kattack kdecay ksustain krelease = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE aretrig <*> toGE kattack <*> toGE kdecay <*> toGE ksustain <*> toGE krelease
    where f aretrig kattack kdecay ksustain krelease = opcs "adsr140" [(Ar, [Ar, Ar, Kr, Kr, Kr, Kr])] [agate, aretrig, kattack, kdecay, ksustain, krelease]
