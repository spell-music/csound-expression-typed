{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language 
        TypeFamilies,
        FlexibleContexts,
        FlexibleInstances,
        UndecidableInstances #-}
module Csound.Typed.Types.Tuple(
    -- ** Tuple
    Tuple(..), TupleMethods, makeTupleMethods, 
    fromTuple, toTuple, tupleArity, tupleRates, defTuple,

    -- ** Outs
    Out(..), OutMethods, makeOutMethods, outArity, tupleToOut,
    toOut, fromOut, mapOut, pureOut, bindOut, accumOut, traverseOut,
    
    -- *** Multiple outs
    multiOuts,
    ar1, ar2, ar4, ar6, ar8,

    -- ** Arguments
    Arg(..), makeArgMethods, arg, toNote, argArity, toArg,

    -- ** Logic functions
    ifTuple, guardedTuple, caseTuple,
    ifArg, guardedArg, caseArg
) where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Default
import Data.Boolean

import Csound.Dynamic
import Csound.Typed.Types.Prim
import Csound.Typed.GlobalState
import Csound.Typed.Types.TupleHelpers

class Tuple a where
    tupleMethods :: TupleMethods a

data TupleMethods a = TupleMethods
    { fromTuple_  :: a -> GE [E]
    , toTuple_    :: GE [E] -> a
    , tupleArity_ :: a -> Int
    , tupleRates_ :: a -> [Rate]
    , defTuple_   :: a }

fromTuple :: Tuple a => a -> GE [E] 
fromTuple = fromTuple_ tupleMethods

toTuple :: Tuple a => GE [E] -> a
toTuple = toTuple_ tupleMethods

tupleArity :: Tuple a => a -> Int
tupleArity = tupleArity_ tupleMethods

tupleRates :: Tuple a => a -> [Rate]
tupleRates = tupleRates_ tupleMethods

defTuple :: Tuple a => a
defTuple = defTuple_ tupleMethods

-- | Defines instance of type class 'Tuple' for a new type in terms of an already defined one.
makeTupleMethods :: (Tuple a) => (a -> b) -> (b -> a) -> TupleMethods b
makeTupleMethods to from = TupleMethods 
    { fromTuple_  = fromTuple . from
    , toTuple_    = to . toTuple 
    , tupleArity_ = const $ tupleArity $ proxy to
    , tupleRates_ = tupleRates . from
    , defTuple_   = to defTuple }
    where proxy :: (a -> b) -> a
          proxy = undefined

-- | Output of the instrument.
class (Monoid (NoSE a), Tuple (NoSE a)) => Out a where
    type NoSE a :: *
    outMethods :: OutMethods a
   
data OutMethods a = OutMethods 
    { toOut_    :: a -> SE [Sig]
    , fromOut_  :: GE [Sig] -> a
    , mapOut_   :: (Sig -> Sig) -> (a -> a)
    , pureOut_  :: Sig -> a
    , bindOut_  :: a -> (Sig -> SE Sig) -> SE a
    , accumOut_ :: ([Sig] -> Sig) -> [a] -> a }

toOut :: Out a => a -> SE [Sig]
toOut = toOut_ outMethods

fromOut :: Out a => GE [Sig] -> a
fromOut = fromOut_ outMethods

mapOut  :: Out a => (Sig -> Sig) -> (a -> a)
mapOut = mapOut_ outMethods

pureOut :: Out a => Sig -> a
pureOut = pureOut_ outMethods

bindOut :: Out a => a -> (Sig -> SE Sig) -> SE a
bindOut = bindOut_ outMethods

accumOut :: Out a => ([Sig] -> Sig) -> [a] -> a
accumOut = accumOut_ outMethods

makeOutMethods :: Out a => (a -> b) -> (b -> a) -> OutMethods b
makeOutMethods to from = OutMethods toOut' fromOut' mapOut' pureOut' bindOut' accumOut' 
    where
        toOut' = toOut . from
        fromOut' = to . fromOut
        mapOut' f = to . mapOut f . from
        pureOut' = to . pureOut
        bindOut' b f = fmap to $ bindOut (from b) f 
        accumOut' f bs = to $ (accumOut f) (fmap from bs)

traverseOut :: Out a => (Sig -> SE Sig) -> [a] -> SE [a]
traverseOut f = mapM (flip bindOut f)

outArity :: Out a => a -> Int
outArity a = tupleArity (proxy a)
    where proxy :: Out a => a -> NoSE a
          proxy = undefined  

tupleToOut :: Out a => NoSE a -> a
tupleToOut = fromOut . fmap (fmap fromE) . fromTuple

-- Tuple instances

primTupleMethods :: (Val a, Default a) => Rate -> TupleMethods a
primTupleMethods rate = TupleMethods 
        { fromTuple_ = fmap return . toGE
        , toTuple_ = fromGE . fmap head
        , tupleArity_ = const 1
        , tupleRates_ = const [rate]
        , defTuple_   = def }

instance Tuple () where
    tupleMethods = TupleMethods 
        { fromTuple_  = return $ return []
        , toTuple_    = const ()
        , tupleArity_ = const 0
        , tupleRates_ = const []
        , defTuple_   = () }

instance Tuple Sig   where tupleMethods = primTupleMethods Ar        
instance Tuple D     where tupleMethods = primTupleMethods Kr
instance Tuple Tab   where tupleMethods = primTupleMethods Kr
instance Tuple Str   where tupleMethods = primTupleMethods Sr
instance Tuple Spec  where tupleMethods = primTupleMethods Fr

instance (Tuple a, Tuple b) => Tuple (a, b) where    
    tupleMethods = TupleMethods fromTuple' toTuple' tupleArity' tupleRates' defTuple'
        where 
            fromTuple' (a, b) = liftA2 (++) (fromTuple a) (fromTuple b)
            tupleArity' x = let (a, b) = proxy x in tupleArity a + tupleArity b
                where proxy :: (a, b) -> (a, b)
                      proxy = const (undefined, undefined)  
            toTuple' xs = (a, b)
                where a = toTuple $ fmap (take (tupleArity a)) xs
                      xsb = fmap (drop (tupleArity a)) xs  
                      b = toTuple $ fmap (take (tupleArity b)) xsb

            tupleRates' (a, b) = tupleRates a ++ tupleRates b
            defTuple' = (defTuple, defTuple)

instance (Tuple a, Tuple b, Tuple c) => Tuple (a, b, c) where tupleMethods = makeTupleMethods cons3 split3
instance (Tuple a, Tuple b, Tuple c, Tuple d) => Tuple (a, b, c, d) where tupleMethods = makeTupleMethods cons4 split4
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e) => Tuple (a, b, c, d, e) where tupleMethods = makeTupleMethods cons5 split5
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f) => Tuple (a, b, c, d, e, f) where tupleMethods = makeTupleMethods cons6 split6
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g) => Tuple (a, b, c, d, e, f, g) where tupleMethods = makeTupleMethods cons7 split7
instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g, Tuple h) => Tuple (a, b, c, d, e, f, g, h) where tupleMethods = makeTupleMethods cons8 split8

-------------------------------------------------------------------------------
-- multiple outs

multiOuts :: Tuple a => E -> a
multiOuts expr = res
    where res = toTuple $ return $ mo (tupleArity res) expr

ar1 :: Sig -> Sig
ar2 :: (Sig, Sig) -> (Sig, Sig)
ar4 :: (Sig, Sig, Sig, Sig) -> (Sig, Sig, Sig, Sig)
ar6 :: (Sig, Sig, Sig, Sig, Sig, Sig) -> (Sig, Sig, Sig, Sig, Sig, Sig)
ar8 :: (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig) -> (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)

ar1 = id;   ar2 = id;   ar4 = id;   ar6 = id;   ar8 = id   

---------------------------------------------------------------------------------
-- out instances

instance Out () where
    type NoSE () = ()
    outMethods = OutMethods toOut' fromOut' mapOut' pureOut' bindOut' accumOut'
        where
            toOut' = const (return [])
            fromOut' = const ()
            mapOut' = const id
            pureOut' = const ()
            bindOut' a _ = return a    
            accumOut' _ = const ()

instance Out Sig where
    type NoSE Sig = Sig
    outMethods = OutMethods toOut' fromOut' mapOut' pureOut' bindOut' accumOut'
        where
            toOut' = return . return
            fromOut' = \x -> fromGE $ toGE =<< fmap head x
            mapOut' = ($)
            pureOut' = id
            bindOut' = flip ($)
            accumOut' = id

instance (Monoid (NoSE a), Out a, Tuple (NoSE a)) => Out (SE a) where
    type NoSE (SE a) = NoSE a
    outMethods = OutMethods toOut' fromOut' mapOut' pureOut' bindOut' accumOut'
        where
            toOut' = join . fmap toOut
            fromOut' = return . fromOut
            mapOut' f a = fmap (mapOut f) a 
            pureOut' = return . pureOut
            bindOut' a f = fmap (\x -> bindOut x f) a
            accumOut' f as = fmap (accumOut f) $ sequence as

instance (Tuple a, Tuple b, Out a, Out b) => Out (a, b) where
    type NoSE (a, b) = (NoSE a, NoSE b)
    outMethods = OutMethods toOut' fromOut' mapOut' pureOut' bindOut' accumOut'
        where
            toOut' (a, b) = liftA2 (++) (toOut a) (toOut b)
            fromOut' = \x -> toTuple $ mapM toGE =<< x
            mapOut' f (a, b) = (mapOut f a, mapOut f b)
            pureOut' a = (pureOut a, pureOut a)
            bindOut' (a, b) f =  (,) <$> bindOut a f <*> bindOut b f
            accumOut' f xs = (accumOut f a, accumOut f b)
                where (a, b) = unzip xs

instance (Tuple a, Tuple b, Tuple c, Out a, Out b, Out c) => Out (a, b, c) where
    type NoSE (a, b, c) = (NoSE a, NoSE b, NoSE c)
    outMethods = makeOutMethods cons3 split3

instance (Tuple a, Tuple b, Tuple c, Tuple d, Out a, Out b, Out c, Out d) => Out (a, b, c, d) where
    type NoSE (a, b, c, d) = (NoSE a, NoSE b, NoSE c, NoSE d)
    outMethods = makeOutMethods cons4 split4

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Out a, Out b, Out c, Out d, Out e) => Out (a, b, c, d, e) where
    type NoSE (a, b, c, d, e) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e)
    outMethods = makeOutMethods cons5 split5

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Out a, Out b, Out c, Out d, Out e, Out f) => Out (a, b, c, d, e, f) where
    type NoSE (a, b, c, d, e, f) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f)
    outMethods = makeOutMethods cons6 split6

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g, Out a, Out b, Out c, Out d, Out e, Out f, Out g) => Out (a, b, c, d, e, f, g) where
    type NoSE (a, b, c, d, e, f, g) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f, NoSE g)
    outMethods = makeOutMethods cons7 split7

instance (Tuple a, Tuple b, Tuple c, Tuple d, Tuple e, Tuple f, Tuple g, Tuple h, Out a, Out b, Out c, Out d, Out e, Out f, Out g, Out h) => Out (a, b, c, d, e, f, g, h) where
    type NoSE (a, b, c, d, e, f, g, h) = (NoSE a, NoSE b, NoSE c, NoSE d, NoSE e, NoSE f, NoSE g, NoSE h)
    outMethods = makeOutMethods cons8 split8

-- missing tuple monoids

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f) => Monoid (a, b, c, d, e, f) where
    mempty = (mempty, mempty, mempty, mempty, mempty, mempty)
    mappend (a1, a2, a3, a4, a5, a6) (b1, b2, b3, b4, b5, b6) = 
        (mappend a1 b1, mappend a2 b2, mappend a3 b3, mappend a4 b4, mappend a5 b5, mappend a6 b6)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g) => Monoid (a, b, c, d, e, f, g) where
    mempty = (mempty, mempty, mempty, mempty, mempty, mempty, mempty)
    mappend (a1, a2, a3, a4, a5, a6, a7) (b1, b2, b3, b4, b5, b6, b7) = 
        (mappend a1 b1, mappend a2 b2, mappend a3 b3, mappend a4 b4, mappend a5 b5, mappend a6 b6, mappend a7 b7)

instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e, Monoid f, Monoid g, Monoid h) => Monoid (a, b, c, d, e, f, g, h) where
    mempty = (mempty, mempty, mempty, mempty, mempty, mempty, mempty, mempty)
    mappend (a1, a2, a3, a4, a5, a6, a7, a8) (b1, b2, b3, b4, b5, b6, b7, b8) = 
        (mappend a1 b1, mappend a2 b2, mappend a3 b3, mappend a4 b4, mappend a5 b5, mappend a6 b6, mappend a7 b7, mappend a8 b8)

---------------------------------------------------------------------------
-- Arguments

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

-------------------------------------------------------------------------
-- logic functions

-- tuples

newtype BoolTuple = BoolTuple { unBoolTuple :: GE [E] }

toBoolTuple :: Tuple a => a -> BoolTuple
toBoolTuple   = BoolTuple . fromTuple

fromBoolTuple :: Tuple a => BoolTuple -> a
fromBoolTuple = toTuple . unBoolTuple

type instance BooleanOf BoolTuple = BoolSig

instance IfB BoolTuple where
    ifB mp (BoolTuple mas) (BoolTuple mbs) = BoolTuple $ 
        liftA3 (\p as bs -> zipWith (ifB p) as bs) (toGE mp) mas mbs

-- | @ifB@ for tuples of csound values.
ifTuple :: (Tuple a) => BoolSig -> a -> a -> a
ifTuple p a b = fromBoolTuple $ ifB p (toBoolTuple a) (toBoolTuple b)

-- | @guardedB@ for tuples of csound values.
guardedTuple :: (Tuple b) => [(BoolSig, b)] -> b -> b
guardedTuple bs b = fromBoolTuple $ guardedB undefined (fmap (second toBoolTuple) bs) (toBoolTuple b)

-- | @caseB@ for tuples of csound values.
caseTuple :: (Tuple b) => a -> [(a -> BoolSig, b)] -> b -> b
caseTuple a bs other = fromBoolTuple $ caseB a (fmap (second toBoolTuple) bs) (toBoolTuple other)

-- arguments

newtype BoolArg = BoolArg { unBoolArg :: GE [E] }

toBoolArg :: (Arg a, Tuple a) => a -> BoolArg
toBoolArg   = BoolArg . fromTuple

fromBoolArg :: (Arg a, Tuple a) => BoolArg -> a
fromBoolArg = toTuple . unBoolArg

type instance BooleanOf BoolArg = BoolD

instance IfB BoolArg where
    ifB mp (BoolArg mas) (BoolArg mbs) = BoolArg $ 
        liftA3 (\p as bs -> zipWith (ifB p) as bs) (toGE mp) mas mbs

-- | @ifB@ for constants.
ifArg :: (Arg a, Tuple a) => BoolD -> a -> a -> a
ifArg p a b = fromBoolArg $ ifB p (toBoolArg a) (toBoolArg b)

-- | @guardedB@ for constants.
guardedArg :: (Tuple b, Arg b) => [(BoolD, b)] -> b -> b
guardedArg bs b = fromBoolArg $ guardedB undefined (fmap (second toBoolArg) bs) (toBoolArg b)

-- | @caseB@ for constants.
caseArg :: (Tuple b, Arg b) => a -> [(a -> BoolD, b)] -> b -> b
caseArg a bs other = fromBoolArg $ caseB a (fmap (second toBoolArg) bs) (toBoolArg other)


