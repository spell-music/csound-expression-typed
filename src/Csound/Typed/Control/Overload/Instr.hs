{-# Language TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Csound.Typed.Control.Overload.Instr(
    Instr(..)
) where

import Csound.Typed.Types
import Csound.Typed.GlobalState
 
class Instr a where
    type InstrIn  a :: *
    type InstrOut a :: *

    toInstr :: a -> InstrIn a -> SE (InstrOut a)

instance Instr (a -> Sig) where
    type InstrIn  (a -> Sig) = a
    type InstrOut (a -> Sig) = Sig

    toInstr f = return . f

instance Instr (a -> (Sig, Sig)) where
    type InstrIn  (a -> (Sig, Sig)) = a
    type InstrOut (a -> (Sig, Sig))  = (Sig, Sig)

    toInstr f = return . f

instance Instr (a -> (Sig, Sig, Sig)) where
    type InstrIn  (a -> (Sig, Sig, Sig)) = a
    type InstrOut (a -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toInstr f = return . f

instance Instr (a -> (Sig, Sig, Sig, Sig)) where
    type InstrIn  (a -> (Sig, Sig, Sig, Sig)) = a
    type InstrOut (a -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toInstr f = return . f

instance Instr (a -> (Sig, Sig, Sig, Sig, Sig)) where
    type InstrIn  (a -> (Sig, Sig, Sig, Sig, Sig)) = a
    type InstrOut (a -> (Sig, Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig, Sig)

    toInstr f = return . f

instance Instr (a -> SE Sig) where
    type InstrIn  (a -> SE Sig) = a
    type InstrOut (a -> SE Sig) = Sig

    toInstr f = f

instance Instr (a -> SE (Sig, Sig)) where
    type InstrIn  (a -> SE (Sig, Sig)) = a
    type InstrOut (a -> SE (Sig, Sig)) = (Sig, Sig)

    toInstr f = f

instance Instr (a -> SE (Sig, Sig, Sig)) where
    type InstrIn  (a -> SE (Sig, Sig, Sig)) = a
    type InstrOut (a -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toInstr f = f

instance Instr (a -> SE (Sig, Sig, Sig, Sig)) where
    type InstrIn  (a -> SE (Sig, Sig, Sig, Sig)) = a
    type InstrOut (a -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toInstr f = f

instance Instr (a -> SE (Sig, Sig, Sig, Sig, Sig)) where
    type InstrIn  (a -> SE (Sig, Sig, Sig, Sig, Sig)) = a
    type InstrOut (a -> SE (Sig, Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig, Sig)

    toInstr f = f

