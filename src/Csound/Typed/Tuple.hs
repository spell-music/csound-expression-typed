{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language 
        TypeFamilies,
        FlexibleContexts #-}
module Csound.Typed.Tuple(
    -- * Tuple
    Tuple(..), TupleMethods, makeTupleMethods, 
    fromTuple, toTuple, arityTuple, ratesTuple, defTuple,

    -- * Outs
    Out(..), OutMethods, makeOutMethods, outArity,
    toOut, fromOut, mapOut, pureOut, bindOut, accumOut, traverseOut,
    
    -- * Multiple outs
    multiOuts,
    ar1, ar2, ar4, ar6, ar8
) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Default

import Csound.Dynamic
import Csound.Typed.Types
import Csound.Typed.Control
import Csound.Typed.TupleHelpers

class Tuple a where
    tupleMethods :: TupleMethods a

data TupleMethods a = TupleMethods
    { fromTuple_  :: a -> GE [E]
    , toTuple_    :: GE [E] -> a
    , arityTuple_ :: a -> Int
    , ratesTuple_ :: a -> [Rate]
    , defTuple_   :: a }

fromTuple :: Tuple a => a -> GE [E] 
fromTuple = fromTuple_ tupleMethods

toTuple :: Tuple a => GE [E] -> a
toTuple = toTuple_ tupleMethods

arityTuple :: Tuple a => a -> Int
arityTuple = arityTuple_ tupleMethods

ratesTuple :: Tuple a => a -> [Rate]
ratesTuple = ratesTuple_ tupleMethods

defTuple :: Tuple a => a
defTuple = defTuple_ tupleMethods

-- | Defines instance of type class 'Tuple' for a new type in terms of an already defined one.
makeTupleMethods :: (Tuple a) => (a -> b) -> (b -> a) -> TupleMethods b
makeTupleMethods to from = TupleMethods 
    { fromTuple_  = fromTuple . from
    , toTuple_    = to . toTuple 
    , arityTuple_ = const $ arityTuple $ proxy to
    , ratesTuple_ = ratesTuple . from
    , defTuple_   = to defTuple }
    where proxy :: (a -> b) -> a
          proxy = undefined

-- | Output of the instrument.
class (Monoid (NoSE a), Tuple (NoSE a)) => Out a where
    type NoSE a :: *
    outMethods :: OutMethods a
   
data OutMethods a = OutMethods 
    { toOut_    :: a -> SE [Sig]
    , fromOut_  :: [Sig] -> a
    , mapOut_   :: (Sig -> Sig) -> (a -> a)
    , pureOut_  :: Sig -> a
    , bindOut_  :: a -> (Sig -> SE Sig) -> SE a
    , accumOut_ :: ([Sig] -> Sig) -> [a] -> a }

toOut :: Out a => a -> SE [Sig]
toOut = toOut_ outMethods

fromOut :: Out a => [Sig] -> a
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
outArity a = arityTuple (proxy a)
    where proxy :: Out a => a -> NoSE a
          proxy = undefined  

-- Tuple instances

primTupleMethods :: (Val a, Default a) => Rate -> TupleMethods a
primTupleMethods rate = TupleMethods 
        { fromTuple_ = fmap return . toGE
        , toTuple_ = fromGE . fmap head
        , arityTuple_ = const 1
        , ratesTuple_ = const [rate]
        , defTuple_   = def }

instance Tuple () where
    tupleMethods = TupleMethods 
        { fromTuple_  = return $ return []
        , toTuple_    = const ()
        , arityTuple_ = const 0
        , ratesTuple_ = const []
        , defTuple_   = () }

instance Tuple Sig   where tupleMethods = primTupleMethods Ar        
instance Tuple D     where tupleMethods = primTupleMethods Kr
instance Tuple Tab   where tupleMethods = primTupleMethods Kr
instance Tuple Str   where tupleMethods = primTupleMethods Sr
instance Tuple Spec  where tupleMethods = primTupleMethods Fr

instance (Tuple a, Tuple b) => Tuple (a, b) where    
    tupleMethods = TupleMethods fromTuple' toTuple' arityTuple' ratesTuple' defTuple'
        where 
            fromTuple' (a, b) = liftA2 (++) (fromTuple a) (fromTuple b)
            arityTuple' x = let (a, b) = proxy x in arityTuple a + arityTuple b
                where proxy :: (a, b) -> (a, b)
                      proxy = const (undefined, undefined)  
            toTuple' xs = (a, b)
                where a = toTuple $ fmap (take (arityTuple a)) xs
                      xsb = fmap (drop (arityTuple a)) xs  
                      b = toTuple $ fmap (take (arityTuple b)) xsb

            ratesTuple' (a, b) = ratesTuple a ++ ratesTuple b
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
    where res = toTuple $ return $ mo (arityTuple res) expr

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
            fromOut' = head 
            mapOut' = ($)
            pureOut' = id
            bindOut' = flip ($)
            accumOut' = id

instance (Monoid a, Out a, Tuple a) => Out (SE a) where
    type NoSE (SE a) = a
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
            fromOut' = toTuple . mapM toGE
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

