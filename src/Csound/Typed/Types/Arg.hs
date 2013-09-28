module Csound.Typed.Types.Arg(
    Arg(..), makeArgMethods, arg, toNote, argArity, toArg
) where

import Control.Applicative 

import Csound.Dynamic

import Csound.Typed.Types.GlobalState
import Csound.Typed.Types.Prim
import Csound.Typed.Types.TupleHelpers

-- | Describes all Csound values that can be used in the score section. 
-- Instruments are triggered with the values from this type class.
-- Actual methods are hidden, but you can easily make instances for your own types
-- with function 'makeArgMethods'. You need to describe the new instance in  terms 
-- of some existing one. For example:
--
-- > data Note = Note 
-- >     { noteAmplitude    :: D
-- >     , notePitch        :: D
-- >     , noteVibrato      :: D
-- >     , noteSample       :: Str
-- >     }
-- > 
-- > instance Arg Note where
-- >     argMethods = makeArgMethods to from
-- >         where to (amp, pch, vibr, sample) = Note amp pch vibr sample
-- >               from (Note amp pch vibr sample) = (amp, pch, vibr, sample)
-- 
-- Then you can use this type in an instrument definition.
-- 
-- > instr :: Note -> Out
-- > instr x = ...
class Arg a where
    argMethods :: ArgMethods a

-- | The abstract type of methods for the class 'Arg'.
data ArgMethods a = ArgMethods 
    { arg_      :: Int -> a
    , toNote_   :: a -> GE [E]
    , argArity_ :: a -> Int }

arg :: Arg a => Int -> a
arg = arg_ argMethods

toNote :: Arg a => a -> GE [E]
toNote = toNote_ argMethods

argArity :: Arg a => a -> Int
argArity = argArity_ argMethods

toArg :: Arg a => a
toArg = arg 4

-- | Defines instance of type class 'Arg' for a new type in terms of an already defined one.
makeArgMethods :: (Arg a) => (a -> b) -> (b -> a) -> ArgMethods b
makeArgMethods to from = ArgMethods {
    arg_ = to . arg,
    toNote_ = toNote . from,
    argArity_ = const $ argArity $ proxy to }
    where proxy :: (a -> b) -> a
          proxy = const $ error "i'm a stupid proxy, fix me"

instance Arg () where
    argMethods = ArgMethods 
        { arg_ = const ()
        , toNote_ = pure . const []
        , argArity_ = const 0 }

instance Arg InstrId where
    argMethods = ArgMethods 
        { arg_ = error "method arg is undefined for InstrId"
        , toNote_ = pure . pure . prim . PrimInstrId
        , argArity_ = const 0 }

primArgMethods :: Val a => ArgMethods a
primArgMethods = ArgMethods {
        arg_ = fromE . pn,
        toNote_ = fmap pure . toGE ,
        argArity_ = const 1 }

instance Arg D      where argMethods = primArgMethods
instance Arg Tab    where argMethods = primArgMethods

instance Arg Str    where 
    argMethods = ArgMethods
        { arg_ = fromE . pn
        , toNote_ = \x -> fmap pure $ saveStr . getStringUnsafe =<< toGE x
        , argArity_ = const 1 }
        where 
            getStringUnsafe x = case getPrimUnsafe x of
                PrimString a    -> a
                _               -> error "Arg(Str):getStringUnsafe value is not a string"

instance (Arg a, Arg b) => Arg (a, b) where
    argMethods = ArgMethods arg' toNote' arity' 
        where arg' n = (a, b)
                  where a = arg n
                        b = arg (n + argArity a)
              toNote' (a, b) = liftA2 (++) (toNote a) (toNote b)
              arity' x = let (a, b) = proxy x in argArity a + argArity b    
                  where proxy :: (a, b) -> (a, b)
                        proxy = const (undefined, undefined)

instance (Arg a, Arg b, Arg c) => Arg (a, b, c) where argMethods = makeArgMethods cons3 split3
instance (Arg a, Arg b, Arg c, Arg d) => Arg (a, b, c, d) where argMethods = makeArgMethods cons4 split4
instance (Arg a, Arg b, Arg c, Arg d, Arg e) => Arg (a, b, c, d, e) where argMethods = makeArgMethods cons5 split5
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Arg (a, b, c, d, e, f) where argMethods = makeArgMethods cons6 split6
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg g) => Arg (a, b, c, d, e, f, g) where argMethods = makeArgMethods cons7 split7
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg g, Arg h) => Arg (a, b, c, d, e, f, g, h) where argMethods = makeArgMethods cons8 split8

