module Csound.Typed.Plugins.Zdf(    
) where

import Data.Boolean
import Control.Monad.Trans.Class

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(zdfPlugin)

-------------------------------------------------------------------------------

-- 1-pole (6dB) lowpass/highpass filter
-- takes in a a-rate signal and cutoff value in frequency
--
-- xout alp, ahp
zdf_1pole :: Sig -> Sig -> (Sig, Sig)
zdf_1pole asig cfq = toTuple $ fmap ($ 2) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq
    where f asig cfq = mopcs "zdf_1pole" ([Ar, Ar], [Ar, Xr]) [asig, cfq]

-- 1-pole allpass filter
-- takes in an a-rate signal and corner frequency where input
-- phase is shifted -90 degrees
zdf_allpass_1pole :: Sig -> Sig -> Sig
zdf_allpass_1pole asig cfq = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq
    where f asig cfq = opcs "zdf_allpass_1pole" [(Ar, [Ar, Xr])] [asig, cfq]

-- 2-pole (12dB) lowpass/highpass/bandpass filter
-- takes in a a-rate signal, cutoff value in frequency, and
-- Q factor for resonance
--
-- xout alp, abp, ahp
zdf_2pole :: Sig -> Sig -> Sig -> (Sig, Sig, Sig)
zdf_2pole asig cfq q = toTuple $ fmap ($ 3) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE q
    where f asig cfq q = mopcs "zdf_2pole" ([Ar, Ar, Ar], [Ar, Xr, Xr]) [asig, cfq, q]

-- 2-pole (12dB) lowpass/highpass/bandpass/notch filter
-- takes in a a-rate signal, cutoff value in frequency, and
-- Q factor for resonance
--
-- xout alp, abp, ahp, anotch
zdf_2pole_notch :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig)
zdf_2pole_notch asig cfq q = toTuple $ fmap ($ 4) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE q
    where f asig cfq q = mopcs "zdf_2pole_notch" ([Ar, Ar, Ar, Ar], [Ar, Xr, Xr]) [asig, cfq, q]

-- moog ladder
--
-- opcode zdf_ladder, a, akk
--
-- ain, kcf, kres   xin
zdf_ladder :: Sig -> Sig -> Sig -> Sig
zdf_ladder asig cfq res = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = opcs "zdf_ladder" [(Ar, [Ar, Xr, Xr])] [asig, cfq, res]

-- 4-pole
--
-- opcode zdf_4pole, aaaaaa, akk
--   ain, kcf, kres xin
--
-- xout alp2, abp2, ahp2, alp4, abl4, abp4
zdf_4pole :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig, Sig, Sig)
zdf_4pole asig cfq res = toTuple $ fmap ($ 6) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = mopcs "zdf_1pole" ([Ar, Ar, Ar, Ar, Ar, Ar], [Ar, Xr, Xr]) [asig, cfq, res]

-- 4-pole
--
-- opcode zdf_4pole_hp, aaaaaa, akk
--   ain, kcf, kres xin
--
-- xout alp2, abp2, ahp2, alp4, abl4, abp4
zdf_4pole_hp :: Sig -> Sig -> Sig -> (Sig, Sig, Sig, Sig, Sig, Sig)
zdf_4pole_hp asig cfq res = toTuple $ fmap ($ 6) $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = mopcs "zdf_4pole_hp" ([Ar, Ar, Ar, Ar, Ar, Ar], [Ar, Xr, Xr]) [asig, cfq, res]

-- ;; TODO - implement
-- opcode zdf_peak_eq, a, akkk
-- ain, kcf, kres, kdB xin
zdf_peak_eq :: Sig -> Sig -> Sig
zdf_peak_eq ain kcf kres kdB = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE ain <*> toGE kcf <*> toGE kres <*> toGE kdB
    where f ain kcf kres kdB = opcs "zdf_peak_eq" [(Ar, [Ar, Kr, Kr, Kr])] [ain, kcf, kres, kdB]

-- opcode zdf_high_shelf_eq, a, akk
--  ain, kcf, kdB xin
zdf_high_shelf_eq :: Sig -> Sig -> Sig -> Sig
zdf_high_shelf_eq asig cfq res = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = opcs "zdf_high_shelf_eq" [(Ar, [Ar, Kr, Kr])] [asig, cfq, res]

-- opcode zdf_low_shelf_eq, a, akk
--  ain, kcf, kdB xin
zdf_low_shelf_eq :: Sig -> Sig -> Sig -> Sig
zdf_low_shelf_eq asig cfq res = fromGE $ do
    addUdoPlugin E.zdfPlugin
    f <$> toGE asig <*> toGE cfq <*> toGE res
    where f asig cfq res = opcs "zdf_low_shelf_eq" [(Ar, [Ar, Kr, Kr])] [asig, cfq, res]

{-
-- |
-- > tabQueue2_append table (pch, vol)
tabQueue2_append :: Tab -> (D, D) -> SE ()
tabQueue2_append tab (pch, vol) = SE $ (depT_ =<<) $ lift $ do    
    addUdoPlugin E.tabQueue2Plugin
    f <$> toGE tab <*> toGE pch <*> toGE vol
    where f tab pch vol = opcs "TabQueue2_Append" [(Xr, [Ir, Ir, Ir])] [tab, pch, vol]

-- | Delete by pitch
--
-- > tabQueue2_delete table pch
tabQueue2_delete :: Tab -> D -> SE ()
tabQueue2_delete tab pch = SE $ (depT_ =<<) $ lift $ do    
    addUdoPlugin E.tabQueue2Plugin
    f <$> toGE tab <*> toGE pch
    where f tab pch = opcs "TabQueue2_Delete" [(Xr, [Ir, Ir])] [tab, pch]

-- | Queue is not empty
tabQueue2_hasElements :: Tab -> BoolSig
tabQueue2_hasElements = (==* 1) . tabQueue2_hasElements'

tabQueue2_hasElements' :: Tab -> Sig
tabQueue2_hasElements' tab = fromGE $ do
    addUdoPlugin E.tabQueue2Plugin
    f <$> toGE tab
    where f tab = opcs "TabQueue2_HasElements" [(Kr, [Ir])] [tab]

tabQueue2_readLastElement :: Tab -> (Sig, Sig)
tabQueue2_readLastElement tab = toTuple $ fmap ($ 2) $ do
    addUdoPlugin E.tabQueue2Plugin    
    f <$> toGE tab
    where f tab = mopcs "TabQueue2_ReadLastElement" ([Kr, Kr], [Ir]) [tab]
-}