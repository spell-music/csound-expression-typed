{-# Language TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Csound.Typed.Control.Overload where

import Csound.Dynamic hiding (Instr)
import Csound.Typed
 
ampCps :: Msg -> (D, D)
ampCps _ = (ampmidi, cpsmidi)

ampmidi :: D
ampmidi = fromE $ opcs "ampmidi" [(Ir, [Ir])] [1]

cpsmidi :: D
cpsmidi = fromE $ opcs "cpsmidi" [(Ir, [])] []

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

-------------------------------------------------------------------------------

class MidiInstr a where
    type MidiInstrOut a :: *

    toMidiInstr :: a -> Msg -> SE (MidiInstrOut a)

-- just sig

instance MidiInstr (Msg -> Sig) where
    type MidiInstrOut (Msg -> Sig) = Sig

    toMidiInstr f = return . f

instance MidiInstr (Msg -> (Sig, Sig)) where
    type MidiInstrOut (Msg -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = return . f

instance MidiInstr (Msg -> (Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = return . f

instance MidiInstr (Msg -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = return . f

-- se sig

instance MidiInstr (Msg -> SE Sig) where
    type MidiInstrOut (Msg -> SE Sig) = Sig

    toMidiInstr f = f

instance MidiInstr (Msg -> SE (Sig, Sig)) where
    type MidiInstrOut (Msg -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = f

instance MidiInstr (Msg -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = f

instance MidiInstr (Msg -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = f

-- by (Sig, Sig)

sig2 :: Msg -> (Sig, Sig)
sig2 msg = (sig amp, sig cps)
    where (amp, cps) = ampCps msg

instance MidiInstr ((Sig, Sig) -> Sig) where
    type MidiInstrOut ((Sig, Sig) -> Sig) = Sig

    toMidiInstr f = return . f . sig2

instance MidiInstr ((Sig, Sig) -> (Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = return . f . sig2

instance MidiInstr ((Sig, Sig) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = return . f . sig2

instance MidiInstr ((Sig, Sig) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = return . f . sig2

-- se sig

instance MidiInstr ((Sig, Sig) -> SE Sig) where
    type MidiInstrOut ((Sig, Sig) -> SE Sig) = Sig

    toMidiInstr f = f . sig2

instance MidiInstr ((Sig, Sig) -> SE (Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = f . sig2

instance MidiInstr ((Sig, Sig) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = f . sig2

instance MidiInstr ((Sig, Sig) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = f . sig2

-- by Sig / D

dsig :: Msg -> (D, Sig)
dsig msg = (amp, sig cps)
    where (amp, cps) = ampCps msg

instance MidiInstr ((D, Sig) -> Sig) where
    type MidiInstrOut ((D, Sig) -> Sig) = Sig

    toMidiInstr f = return . f . dsig

instance MidiInstr ((D, Sig) -> (Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = return . f . dsig

instance MidiInstr ((D, Sig) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = return . f . dsig

instance MidiInstr ((D, Sig) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = return . f . dsig

-- se sig

instance MidiInstr ((D, Sig) -> SE Sig) where
    type MidiInstrOut ((D, Sig) -> SE Sig) = Sig

    toMidiInstr f = f . dsig

instance MidiInstr ((D, Sig) -> SE (Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = f . dsig

instance MidiInstr ((D, Sig) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = f . dsig

instance MidiInstr ((D, Sig) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = f . dsig

-- by Sig / D

sigd :: Msg -> (Sig, D)
sigd msg = (sig amp, cps)
    where (amp, cps) = ampCps msg

instance MidiInstr ((Sig, D) -> Sig) where
    type MidiInstrOut ((Sig, D) -> Sig) = Sig

    toMidiInstr f = return . f . sigd

instance MidiInstr ((Sig, D) -> (Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = return . f . sigd

instance MidiInstr ((Sig, D) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = return . f . sigd

instance MidiInstr ((Sig, D) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = return . f . sigd

-- se sig

instance MidiInstr ((Sig, D) -> SE Sig) where
    type MidiInstrOut ((Sig, D) -> SE Sig) = Sig

    toMidiInstr f = f . sigd

instance MidiInstr ((Sig, D) -> SE (Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = f . sigd

instance MidiInstr ((Sig, D) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = f . sigd

instance MidiInstr ((Sig, D) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = f . sigd

-- d2

d2 :: Msg -> (D, D)
d2 = ampCps

instance MidiInstr ((D, D) -> Sig) where
    type MidiInstrOut ((D, D) -> Sig) = Sig

    toMidiInstr f = return . f . d2 

instance MidiInstr ((D, D) -> (Sig, Sig)) where
    type MidiInstrOut ((D, D) -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = return . f . d2

instance MidiInstr ((D, D) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = return . f . d2

instance MidiInstr ((D, D) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = return . f . d2

-- se sig

instance MidiInstr ((D, D) -> SE Sig) where
    type MidiInstrOut ((D, D) -> SE Sig) = Sig

    toMidiInstr f = f . d2

instance MidiInstr ((D, D) -> SE (Sig, Sig)) where
    type MidiInstrOut ((D, D) -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f = f . d2

instance MidiInstr ((D, D) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f = f . d2

instance MidiInstr ((D, D) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f = f . d2

-- sig

instance MidiInstr (Sig -> Sig) where
    type MidiInstrOut (Sig -> Sig) = Sig

    toMidiInstr f _ = return $ sig ampmidi * f (sig cpsmidi)
    
instance MidiInstr (Sig -> (Sig, Sig)) where
    type MidiInstrOut (Sig -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f _ = return $ (sig ampmidi * a1, sig ampmidi * a2)
        where (a1, a2) = f (sig cpsmidi)

instance MidiInstr (Sig -> (Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f _ = return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3)
        where (a1, a2, a3) = f (sig cpsmidi)

instance MidiInstr (Sig -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f _ = return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3, sig ampmidi * a4)
        where (a1, a2, a3, a4) = f (sig cpsmidi)

    
instance MidiInstr (Sig -> SE Sig) where
    type MidiInstrOut (Sig -> SE Sig) = Sig

    toMidiInstr f _ = do
        a1 <- f (sig cpsmidi)
        return $ sig ampmidi * a1
    
instance MidiInstr (Sig -> SE (Sig, Sig)) where
    type MidiInstrOut (Sig -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f _ = do
        (a1, a2) <- f (sig cpsmidi)
        return $ (sig ampmidi * a1, sig ampmidi * a2)

instance MidiInstr (Sig -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f _ = do
        (a1, a2, a3) <- f (sig cpsmidi)
        return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3)

instance MidiInstr (Sig -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f _ = do
        (a1, a2, a3, a4) <- f (sig cpsmidi)
        return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3, sig ampmidi * a4)

-- d

instance MidiInstr (D -> Sig) where
    type MidiInstrOut (D -> Sig) = Sig

    toMidiInstr f _ = return $ sig ampmidi * f cpsmidi
    
instance MidiInstr (D -> (Sig, Sig)) where
    type MidiInstrOut (D -> (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f _ = return $ (sig ampmidi * a1, sig ampmidi * a2)
        where (a1, a2) = f cpsmidi

instance MidiInstr (D -> (Sig, Sig, Sig)) where
    type MidiInstrOut (D -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f _ = return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3)
        where (a1, a2, a3) = f cpsmidi

instance MidiInstr (D -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (D -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f _ = return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3, sig ampmidi * a4)
        where (a1, a2, a3, a4) = f cpsmidi

instance MidiInstr (D -> SE Sig) where
    type MidiInstrOut (D -> SE Sig) = Sig

    toMidiInstr f _ = do
        a1 <- f (cpsmidi)
        return $ sig ampmidi * a1
    
instance MidiInstr (D -> SE (Sig, Sig)) where
    type MidiInstrOut (D -> SE (Sig, Sig)) = (Sig, Sig)

    toMidiInstr f _ = do
        (a1, a2) <- f (cpsmidi)
        return $ (sig ampmidi * a1, sig ampmidi * a2)

instance MidiInstr (D -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut (D -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    toMidiInstr f _ = do
        (a1, a2, a3) <- f (cpsmidi)
        return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3)

instance MidiInstr (D -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (D -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    toMidiInstr f _ = do
        (a1, a2, a3, a4) <- f (cpsmidi)
        return $ (sig ampmidi * a1, sig ampmidi * a2, sig ampmidi * a3, sig ampmidi * a4)


