{-# Language TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Csound.Typed.Control.Overload.SpecInstr(
    AmpInstr(..), CpsInstr(..)      
) where

import Csound.Typed.Types
import Csound.Typed.GlobalState
 
import Control.Arrow(first, second)
------------------------------------------------------------------------

class AmpInstr a where
    type AmpInstrOut a :: *
    ampInstr :: a -> D -> SE (AmpInstrOut a)

instance AmpInstr (D -> SE Sig) where
    type AmpInstrOut (D -> SE Sig) = Sig
    ampInstr = id

instance AmpInstr (D -> SE (Sig, Sig)) where
    type AmpInstrOut (D -> SE (Sig, Sig)) = (Sig, Sig)
    ampInstr = id

instance AmpInstr (D -> Sig) where
    type AmpInstrOut (D -> Sig) = Sig
    ampInstr f = return . f

instance AmpInstr (D -> (Sig, Sig)) where
    type AmpInstrOut (D -> (Sig, Sig)) = (Sig, Sig)
    ampInstr f = return . f

instance AmpInstr (Sig -> SE Sig) where
    type AmpInstrOut (Sig -> SE Sig) = Sig
    ampInstr f = f . sig

instance AmpInstr (Sig -> SE (Sig, Sig)) where
    type AmpInstrOut (Sig -> SE (Sig, Sig)) = (Sig, Sig)
    ampInstr f = f . sig

instance AmpInstr (Sig -> Sig) where
    type AmpInstrOut (Sig -> Sig) = Sig
    ampInstr f = return . f . sig

instance AmpInstr (Sig -> (Sig, Sig)) where
    type AmpInstrOut (Sig -> (Sig, Sig)) = (Sig, Sig)
    ampInstr f = return . f . sig

instance AmpInstr (SE Sig) where
    type AmpInstrOut (SE Sig) = Sig
    ampInstr a amp = fmap (sig amp * ) a

instance AmpInstr (SE (Sig, Sig)) where
    type AmpInstrOut (SE (Sig, Sig)) = (Sig, Sig)
    ampInstr a amp = fmap (\(a1, a2) -> (sig amp * a1, sig amp * a2)) a 

instance AmpInstr Sig where
    type AmpInstrOut Sig = Sig
    ampInstr a amp = return $ a * sig amp

instance AmpInstr (Sig, Sig) where
    type AmpInstrOut (Sig, Sig) = (Sig, Sig)
    ampInstr (a1, a2) amp = return (a1 * sig amp, a2 * sig amp)

------------------------------------------------------------------------

class CpsInstr a where
    type CpsInstrOut a :: *
    cpsInstr :: a -> (D, D) -> SE (CpsInstrOut a)

instance CpsInstr ((D, D) -> SE Sig) where
    type CpsInstrOut ((D, D) -> SE Sig) = Sig
    cpsInstr = id

instance CpsInstr ((D, D) -> SE (Sig, Sig)) where
    type CpsInstrOut ((D, D) -> SE (Sig, Sig)) = (Sig, Sig)
    cpsInstr = id

instance CpsInstr ((D, D) -> Sig) where
    type CpsInstrOut ((D, D) -> Sig) = Sig
    cpsInstr f = return . f

instance CpsInstr ((D, D) -> (Sig, Sig)) where
    type CpsInstrOut ((D, D) -> (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = return . f

instance CpsInstr ((D, Sig) -> SE Sig) where
    type CpsInstrOut ((D, Sig) -> SE Sig) = Sig
    cpsInstr f = f . second sig

instance CpsInstr ((D, Sig) -> SE (Sig, Sig)) where
    type CpsInstrOut ((D, Sig) -> SE (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = f . second sig

instance CpsInstr ((D, Sig) -> Sig) where
    type CpsInstrOut ((D, Sig) -> Sig) = Sig
    cpsInstr f = return . f . second sig

instance CpsInstr ((D, Sig) -> (Sig, Sig)) where
    type CpsInstrOut ((D, Sig) -> (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = return . f . second sig

instance CpsInstr ((Sig, D) -> SE Sig) where
    type CpsInstrOut ((Sig, D) -> SE Sig) = Sig
    cpsInstr f = f . first sig

instance CpsInstr ((Sig, D) -> SE (Sig, Sig)) where
    type CpsInstrOut ((Sig, D) -> SE (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = f . first sig

instance CpsInstr ((Sig, D) -> Sig) where
    type CpsInstrOut ((Sig, D) -> Sig) = Sig
    cpsInstr f = return . f . first sig

instance CpsInstr ((Sig, D) -> (Sig, Sig)) where
    type CpsInstrOut ((Sig, D) -> (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = return . f . first sig

instance CpsInstr ((Sig, Sig) -> SE Sig) where
    type CpsInstrOut ((Sig, Sig) -> SE Sig) = Sig
    cpsInstr f = f . first sig . second sig

instance CpsInstr ((Sig, Sig) -> SE (Sig, Sig)) where
    type CpsInstrOut ((Sig, Sig) -> SE (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = f . first sig . second sig

instance CpsInstr ((Sig, Sig) -> Sig) where
    type CpsInstrOut ((Sig, Sig) -> Sig) = Sig
    cpsInstr f = return . f . first sig . second sig

instance CpsInstr ((Sig, Sig) -> (Sig, Sig)) where
    type CpsInstrOut ((Sig, Sig) -> (Sig, Sig)) = (Sig, Sig)
    cpsInstr f = return . f . first sig . second sig

instance CpsInstr (D -> SE Sig) where
    type CpsInstrOut (D -> SE Sig) = Sig
    cpsInstr f (amp, cps) = fmap (* sig amp) $ f cps

instance CpsInstr (D -> SE (Sig, Sig)) where
    type CpsInstrOut (D -> SE (Sig, Sig)) = (Sig, Sig)
    cpsInstr f (amp, cps) = fmap (first (* sig amp) . second (* sig amp)) $ f cps

instance CpsInstr (D -> Sig) where
    type CpsInstrOut (D -> Sig) = Sig
    cpsInstr f (amp, cps) = return $ sig amp * f cps

instance CpsInstr (D -> (Sig, Sig)) where
    type CpsInstrOut (D -> (Sig, Sig)) = (Sig, Sig)
    cpsInstr f (amp, cps) = return $ first (* sig amp) $ second (* sig amp) $ f cps

instance CpsInstr (Sig -> SE Sig) where
    type CpsInstrOut (Sig -> SE Sig) = Sig
    cpsInstr f (amp, cps) = fmap (* sig amp) $ f $ sig cps

instance CpsInstr (Sig -> SE (Sig, Sig)) where
    type CpsInstrOut (Sig -> SE (Sig, Sig)) = (Sig, Sig)
    cpsInstr f (amp, cps) = fmap (first (* sig amp) . second (* sig amp)) $ f $ sig cps

instance CpsInstr (Sig -> Sig) where
    type CpsInstrOut (Sig -> Sig) = Sig
    cpsInstr f (amp, cps) = return $ sig amp * f (sig cps)

instance CpsInstr (Sig -> (Sig, Sig)) where
    type CpsInstrOut (Sig -> (Sig, Sig)) = (Sig, Sig)
    cpsInstr f (amp, cps) = return $ first (* sig amp) $ second (* sig amp) $ f $ sig cps



