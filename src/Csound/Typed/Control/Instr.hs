-- | Converts to low-level instruments
module Csound.Typed.Control.Instr(
    Arity(..), InsExp, EffExp,
    insArity, effArity, masterArity, midiArity,
    insExp, effExp, masterExp, midiExp,
    -- * out from expression
    outGE, outFromE, outFromGE
) where

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.Types.GlobalState

funProxy :: (a -> b) -> (a, b)
funProxy = const (msg, msg)
    where msg = error "I'm a Csound.Typed.Types.Tuple.funProxy"

insArity :: (Arg a, Out b) => (a -> b) -> Arity
insArity instr = Arity (argArity a) (outArity b)
    where (a, b) = funProxy instr

effArity :: (Out a, Out b) => (a -> b) -> Arity
effArity instr = Arity (outArity a) (outArity b)
    where (a, b) = funProxy instr

masterArity :: (Out a) => a -> Arity
masterArity a = Arity 0 (outArity a)

midiArity :: (Out a) => (Msg -> a) -> Arity
midiArity a = Arity 0 (outArity $ snd $ funProxy a)
   
insExp :: (Arg a, Out b) => (a -> b) -> InsExp 
insExp instr = outGE $ instr toArg

effExp :: (Out a, Out b) => (a -> b) -> EffExp
effExp instr = outGE . instr . outFromE

masterExp :: (Out a) => a -> InsExp
masterExp = outGE

midiExp :: (Out a) => (Msg -> a) -> InsExp
midiExp instr = outGE $ instr Msg

outGE :: Out a => a -> SE (GE [E])
outGE = fmap (mapM toGE) . toOut

outFromE :: Out a => [E] -> a
outFromE = fromOut . return . fmap fromE

outFromGE :: Out a => GE [E] -> a
outFromGE = fromOut . fmap (fmap fromE)

