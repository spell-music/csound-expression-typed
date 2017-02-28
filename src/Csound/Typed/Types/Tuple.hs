{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language 
        TypeFamilies,
        FlexibleContexts,
        FlexibleInstances #-}
module Csound.Typed.Types.Tuple(
    -- ** Tuple
    Tuple(..), TupleMethods, makeTupleMethods, 
    fromTuple, toTuple, tupleArity, tupleRates, defTuple, mapTuple,

    Sig2, Sig3, Sig4, Sig5, Sig6, Sig7, Sig8,

    -- ** Outs
    Sigs, outArity, 

    -- *** Multiple outs
    multiOuts,
    ar1, ar2, ar4, ar6, ar8,

    -- ** Arguments
    Arg, arg, toNote, argArity, toArg,

    -- ** Logic functions
    ifTuple, guardedTuple, caseTuple,
    ifArg, guardedArg, caseArg,

    -- ** Constructors
    pureTuple, dirtyTuple
) where


import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Data.Default
import Data.Boolean

import Csound.Dynamic
import Csound.Typed.Types.Prim
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.Types.TupleHelpers

-- | A tuple of Csound values.
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

mapTuple :: Tuple a => (E -> E) -> a -> a
mapTuple f a = toTuple (fmap (fmap f) $ fromTuple a)

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

-- Tuple instances

primTupleMethods :: (Val a, Default a) => Rate -> TupleMethods a
primTupleMethods rate = TupleMethods 
        { fromTuple_ = fmap return . toGE
        , toTuple_ = fromGE . fmap head
        , tupleArity_ = const 1
        , tupleRates_ = const [rate]
        , defTuple_   = def }

instance Tuple Unit where
    tupleMethods = TupleMethods 
        { fromTuple_  = \x -> unUnit x >> (return [])
        , toTuple_    = \es -> Unit $ es >> return ()
        , tupleArity_ = const 0
        , tupleRates_ = const []
        , defTuple_   = Unit $ return () }

instance Tuple Sig   where tupleMethods = primTupleMethods Ar        
instance Tuple D     where tupleMethods = primTupleMethods Kr
instance Tuple Tab   where tupleMethods = primTupleMethods Kr
instance Tuple Str   where tupleMethods = primTupleMethods Sr
instance Tuple Spec  where tupleMethods = primTupleMethods Fr

instance Tuple TabList where tupleMethods = primTupleMethods Kr

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

-- | The tuples of signals.
class (Tuple a, Num a, Fractional a) => Sigs a where

type Sig2 = (Sig, Sig)
type Sig3 = (Sig, Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)
type Sig5 = (Sig, Sig, Sig, Sig, Sig)
type Sig6 = (Sig, Sig, Sig, Sig, Sig, Sig)
type Sig7 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig)
type Sig8 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)

instance Sigs Sig
instance Sigs Sig2
instance Sigs Sig3
instance Sigs Sig4
instance Sigs Sig5
instance Sigs Sig6
instance Sigs Sig7
instance Sigs Sig8

instance Sigs (Sig2, Sig2)
instance Sigs (Sig2, Sig2, Sig2)
instance Sigs (Sig2, Sig2, Sig2, Sig2)
instance Sigs (Sig2, Sig2, Sig2, Sig2, Sig2)
instance Sigs (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2)

instance Sigs (Sig8, Sig8)
instance Sigs (Sig8, Sig8, Sig8, Sig8)

outArity :: Tuple a => SE a -> Int
outArity = tupleArity . proxy
    where
        proxy :: SE a -> a
        proxy = const undefined

---------------------------------------------------------------------------
-- Arguments

class (Tuple a) => Arg a where

instance Arg Unit
instance Arg D
instance Arg Str
instance Arg Tab
instance Arg TabList

instance (Arg a, Arg b) => Arg (a, b)
instance (Arg a, Arg b, Arg c) => Arg (a, b, c)
instance (Arg a, Arg b, Arg c, Arg d) => Arg (a, b, c, d)
instance (Arg a, Arg b, Arg c, Arg d, Arg e) => Arg (a, b, c, d, e)
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f) => Arg (a, b, c, d, e, f)
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg h) => Arg (a, b, c, d, e, f, h)
instance (Arg a, Arg b, Arg c, Arg d, Arg e, Arg f, Arg h, Arg g) => Arg (a, b, c, d, e, f, h, g)

arg :: Arg a => Int -> a
arg n = toTuple $ return $ fmap pn [n ..]

toArg :: Arg a => a
toArg = arg 4

argArity :: Arg a => a -> Int
argArity = tupleArity

toNote :: Arg a => a -> GE [E]
toNote a = zipWithM phi (tupleRates a) =<< fromTuple a
    where
        phi rate x = case rate of            
            Sr -> saveStr $ getStringUnsafe x
            _  -> return x
            
        getStringUnsafe x = case getPrimUnsafe x of
            PrimString y    -> y
            _               -> error "Arg(Str):getStringUnsafe value is not a string"

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

-----------------------------------------------------------
-- tuple constructors

pureTuple :: Tuple a => GE (MultiOut [E]) -> a
pureTuple a = res
    where res = toTuple $ fmap ($ tupleArity res) a

dirtyTuple :: Tuple a => GE (MultiOut [E]) -> SE a
dirtyTuple a = res
    where 
        res = fmap (toTuple . return) $ SE 
                $ mapM depT =<< (lift $ fmap ($ (tupleArity $ proxy res)) a)

        proxy :: SE a -> a
        proxy = const undefined
        

-------------------------------------------------
-- numeric instances

instance Num Sig2 where
    (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2)
    (a1, a2) * (b1, b2) = (a1 * b1, a2 * b2)
    negate (a1, a2) = (negate a1, negate a2)

    fromInteger n = (fromInteger n, fromInteger n)
    signum (a1, a2) = (signum a1, signum a2)
    abs (a1, a2) = (abs a1, abs a2)

instance Fractional Sig2 where
    recip (a1, a2) = (recip a1, recip a2)
    fromRational n = (fromRational n, fromRational n)

instance Num Sig3 where
    (a1, a2, a3) + (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)
    (a1, a2, a3) * (b1, b2, b3) = (a1 * b1, a2 * b2, a3 * b3)
    negate (a1, a2, a3) = (negate a1, negate a2, negate a3)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3) = (signum a1, signum a2, signum a3)
    abs (a1, a2, a3) = (abs a1, abs a2, abs a3)

instance Fractional Sig3 where
    recip (a1, a2, a3) = (recip a1, recip a2, recip a3)
    fromRational n = (fromRational n, fromRational n, fromRational n)

instance Num Sig4 where
    (a1, a2, a3, a4) + (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)
    (a1, a2, a3, a4) * (b1, b2, b3, b4) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4)
    negate (a1, a2, a3, a4) = (negate a1, negate a2, negate a3, negate a4)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4) = (signum a1, signum a2, signum a3, signum a4)
    abs (a1, a2, a3, a4) = (abs a1, abs a2, abs a3, abs a4)

instance Fractional Sig4 where
    recip (a1, a2, a3, a4) = (recip a1, recip a2, recip a3, recip a4)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n)

instance Num Sig5 where
    (a1, a2, a3, a4, a5) + (b1, b2, b3, b4, b5) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5)
    (a1, a2, a3, a4, a5) * (b1, b2, b3, b4, b5) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5)
    negate (a1, a2, a3, a4, a5) = (negate a1, negate a2, negate a3, negate a4, negate a5)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5) = (signum a1, signum a2, signum a3, signum a4, signum a5)
    abs (a1, a2, a3, a4, a5) = (abs a1, abs a2, abs a3, abs a4, abs a5)

instance Fractional Sig5 where
    recip (a1, a2, a3, a4, a5) = (recip a1, recip a2, recip a3, recip a4, recip a5)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)

instance Num Sig6 where
    (a1, a2, a3, a4, a5, a6) + (b1, b2, b3, b4, b5, b6) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5, a6 + b6)
    (a1, a2, a3, a4, a5, a6) * (b1, b2, b3, b4, b5, b6) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5, a6 * b6)
    negate (a1, a2, a3, a4, a5, a6) = (negate a1, negate a2, negate a3, negate a4, negate a5, negate a6)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5, a6) = (signum a1, signum a2, signum a3, signum a4, signum a5, signum a6)
    abs (a1, a2, a3, a4, a5, a6) = (abs a1, abs a2, abs a3, abs a4, abs a5, abs a6)

instance Fractional Sig6 where
    recip (a1, a2, a3, a4, a5, a6) = (recip a1, recip a2, recip a3, recip a4, recip a5, recip a6)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)

instance Num Sig7 where
    (a1, a2, a3, a4, a5, a6, a7) + (b1, b2, b3, b4, b5, b6, b7) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5, a6 + b6, a7 + b7)
    (a1, a2, a3, a4, a5, a6, a7) * (b1, b2, b3, b4, b5, b6, b7) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5, a6 * b6, a7 * b7)
    negate (a1, a2, a3, a4, a5, a6, a7) = (negate a1, negate a2, negate a3, negate a4, negate a5, negate a6, negate a7)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5, a6, a7) = (signum a1, signum a2, signum a3, signum a4, signum a5, signum a6, signum a7)
    abs (a1, a2, a3, a4, a5, a6, a7) = (abs a1, abs a2, abs a3, abs a4, abs a5, abs a6, abs a7)

instance Fractional Sig7 where
    recip (a1, a2, a3, a4, a5, a6, a7) = (recip a1, recip a2, recip a3, recip a4, recip a5, recip a6, recip a7)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)

instance Num Sig8 where
    (a1, a2, a3, a4, a5, a6, a7, a8) + (b1, b2, b3, b4, b5, b6, b7, b8) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5, a6 + b6, a7 + b7, a8 + b8)
    (a1, a2, a3, a4, a5, a6, a7, a8) * (b1, b2, b3, b4, b5, b6, b7, b8) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5, a6 * b6, a7 + b7, a8 + b8)
    negate (a1, a2, a3, a4, a5, a6, a7, a8) = (negate a1, negate a2, negate a3, negate a4, negate a5, negate a6, negate a7, negate a8)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5, a6, a7, a8) = (signum a1, signum a2, signum a3, signum a4, signum a5, signum a6, signum a7, signum a8)
    abs (a1, a2, a3, a4, a5, a6, a7, a8) = (abs a1, abs a2, abs a3, abs a4, abs a5, abs a6, abs a7, abs a8)

instance Fractional Sig8 where
    recip (a1, a2, a3, a4, a5, a6, a7, a8) = (recip a1, recip a2, recip a3, recip a4, recip a5, recip a6, recip a7, recip a8)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)

instance Num (Sig8, Sig8) where
    (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2)
    (a1, a2) * (b1, b2) = (a1 * b1, a2 * b2)
    negate (a1, a2) = (negate a1, negate a2)

    fromInteger n = (fromInteger n, fromInteger n)
    signum (a1, a2) = (signum a1, signum a2)
    abs (a1, a2) = (abs a1, abs a2)

instance Fractional (Sig8, Sig8) where
    recip (a1, a2) = (recip a1, recip a2)
    fromRational n = (fromRational n, fromRational n)

instance Num (Sig8, Sig8, Sig8, Sig8) where
    (a1, a2, a3, a4) + (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)
    (a1, a2, a3, a4) * (b1, b2, b3, b4) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4)
    negate (a1, a2, a3, a4) = (negate a1, negate a2, negate a3, negate a4)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4) = (signum a1, signum a2, signum a3, signum a4)
    abs (a1, a2, a3, a4) = (abs a1, abs a2, abs a3, abs a4)

instance Fractional (Sig8, Sig8, Sig8, Sig8) where
    recip (a1, a2, a3, a4) = (recip a1, recip a2, recip a3, recip a4)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n)

instance Num (Sig2, Sig2) where
    (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2)
    (a1, a2) * (b1, b2) = (a1 * b1, a2 * b2)
    negate (a1, a2) = (negate a1, negate a2)

    fromInteger n = (fromInteger n, fromInteger n)
    signum (a1, a2) = (signum a1, signum a2)
    abs (a1, a2) = (abs a1, abs a2)

instance Fractional (Sig2, Sig2) where
    recip (a1, a2) = (recip a1, recip a2)
    fromRational n = (fromRational n, fromRational n)

instance Num (Sig2, Sig2, Sig2) where
    (a1, a2, a3) + (b1, b2, b3) = (a1 + b1, a2 + b2, a3 + b3)
    (a1, a2, a3) * (b1, b2, b3) = (a1 * b1, a2 * b2, a3 * b3)
    negate (a1, a2, a3) = (negate a1, negate a2, negate a3)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3) = (signum a1, signum a2, signum a3)
    abs (a1, a2, a3) = (abs a1, abs a2, abs a3)

instance Fractional (Sig2, Sig2, Sig2) where
    recip (a1, a2, a3) = (recip a1, recip a2, recip a3)
    fromRational n = (fromRational n, fromRational n, fromRational n)

instance Num (Sig2, Sig2, Sig2, Sig2) where
    (a1, a2, a3, a4) + (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)
    (a1, a2, a3, a4) * (b1, b2, b3, b4) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4)
    negate (a1, a2, a3, a4) = (negate a1, negate a2, negate a3, negate a4)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4) = (signum a1, signum a2, signum a3, signum a4)
    abs (a1, a2, a3, a4) = (abs a1, abs a2, abs a3, abs a4)

instance Fractional (Sig2, Sig2, Sig2, Sig2) where
    recip (a1, a2, a3, a4) = (recip a1, recip a2, recip a3, recip a4)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n)

instance Num (Sig2, Sig2, Sig2, Sig2, Sig2) where
    (a1, a2, a3, a4, a5) + (b1, b2, b3, b4, b5) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5)
    (a1, a2, a3, a4, a5) * (b1, b2, b3, b4, b5) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5)
    negate (a1, a2, a3, a4, a5) = (negate a1, negate a2, negate a3, negate a4, negate a5)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5) = (signum a1, signum a2, signum a3, signum a4, signum a5)
    abs (a1, a2, a3, a4, a5) = (abs a1, abs a2, abs a3, abs a4, abs a5)

instance Fractional (Sig2, Sig2, Sig2, Sig2, Sig2) where
    recip (a1, a2, a3, a4, a5) = (recip a1, recip a2, recip a3, recip a4, recip a5)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)

instance Num (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where
    (a1, a2, a3, a4, a5, a6) + (b1, b2, b3, b4, b5, b6) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5, a6 + b6)
    (a1, a2, a3, a4, a5, a6) * (b1, b2, b3, b4, b5, b6) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5, a6 * b6)
    negate (a1, a2, a3, a4, a5, a6) = (negate a1, negate a2, negate a3, negate a4, negate a5, negate a6)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5, a6) = (signum a1, signum a2, signum a3, signum a4, signum a5, signum a6)
    abs (a1, a2, a3, a4, a5, a6) = (abs a1, abs a2, abs a3, abs a4, abs a5, abs a6)

instance Fractional (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2) where
    recip (a1, a2, a3, a4, a5, a6) = (recip a1, recip a2, recip a3, recip a4, recip a5, recip a6)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)
