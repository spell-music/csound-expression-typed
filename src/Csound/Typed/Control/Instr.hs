-- | Converts to low-level instruments
module Csound.Typed.Control.Instr(
    Arity(..), InsExp, EffExp,
    funArity, constArity, 
    insExp, effExp, masterExp, midiExp, unitExp
) where

import Csound.Typed.Types
import Csound.Typed.GlobalState

funProxy :: (a -> f b) -> (a, b)
funProxy = const (msg, msg)
    where msg = error "I'm a Csound.Typed.Control.Instr.funProxy"

funArity :: (Tuple a, Tuple b) => (a -> SE b) -> Arity
funArity instr = Arity (tupleArity a) (tupleArity b)
    where (a, b) = funProxy instr

constArity :: (Tuple a) => SE a -> Arity
constArity a = Arity 0 (outArity a)
   
insExp :: (Arg a, Tuple b) => (a -> SE b) -> InsExp 
insExp instr = fmap fromTuple $ instr toArg

effExp :: (Tuple a, Tuple b) => (a -> SE b) -> EffExp
effExp instr = fmap fromTuple . instr . toTuple . return 

masterExp :: (Tuple a) => SE a -> InsExp
masterExp = fmap fromTuple

midiExp :: (Tuple a) => (Msg -> SE a) -> InsExp
midiExp instr = fmap fromTuple $ instr Msg

unitExp :: SE Unit -> UnitExp
unitExp = execSE . execGEinSE . fmap unUnit

