{-# Language FlexibleInstances, UndecidableInstances #-}
module Csound.Typed.Types.Lift where

import Control.Applicative

import Csound.Dynamic
import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.GlobalState

class PureSingle a where
    pureSingle :: ([E] -> E) -> a

class DirtySingle a where
    dirtySingle :: ([E] -> Dep E) -> a

class Procedure a where
    procedure :: ([E] -> Dep ()) -> a
    
class PureMulti a where
    pureMulti :: ([E] -> MultiOut [E]) -> a
    
class DirtyMulti a where
    dirtyMulti :: ([E] -> MultiOut (Dep [E])) -> a

-- pure single

instance Val a => PureSingle a where
    pureSingle = fromE . ($ []) 

instance (Val a, Val b) => PureSingle (a -> b) where
    pureSingle f = \a -> fromGE $ fmap (f . return) $ toGE a 

instance (Val a1, Val a2, Val b) => PureSingle (a1 -> a2 -> b) where
    pureSingle f = \ma1 ma2 -> fromGE $ (\a1 a2 -> f [a1, a2]) <$> toGE ma1 <*> toGE ma2

-- dirty single

instance Val a => DirtySingle (SE a) where
    dirtySingle = fmap fromGE . fromDep . return . ($ [])    

instance (Val a1, Val a2) => DirtySingle (a1 -> SE a2) where
    dirtySingle f = \ma1 -> fmap fromGE $ fromDep $ (\a1 -> f [a1]) <$> toGE ma1

instance (Val a1, Val a2, Val a3) => DirtySingle (a1 -> a2 -> SE a3) where
    dirtySingle f = \ma1 ma2 -> fmap fromGE $ fromDep $ (\a1 a2 -> f [a1, a2]) <$> toGE ma1 <*> toGE ma2

-- procedure

instance Procedure (SE ()) where
    procedure = fromDep_ . return . ($ [])

instance Val a1 => Procedure (a1 -> SE ()) where
    procedure f = \ma1 -> fromDep_ $ (\a1 -> f [a1]) <$> toGE ma1 

instance (Val a1, Val a2) => Procedure (a1 -> a2 -> SE ()) where
    procedure f = \ma1 ma2 -> fromDep_ $ (\a1 a2 -> f [a1, a2]) <$> toGE ma1 <*> toGE ma2

-- pure multi

moTuple :: Tuple a => GE (MultiOut [E]) -> a
moTuple a = res
    where res = toTuple $ fmap ( $ tupleArity res) a

depMoTuple :: Tuple a => GE (MultiOut (Dep [E])) -> SE a
depMoTuple a = res
    where 
        res = fmap toTuple $ fromDep $ fmap ( $ (tupleArity $ proxy res)) a

        proxy :: SE a -> a
        proxy = const undefined

instance Tuple a => PureMulti a where
    pureMulti = moTuple . return . ($ [])

instance (Val a1, Tuple b) => PureMulti (a1 -> b) where
    pureMulti f = \ma1 -> moTuple $ (\a1 -> f [a1]) <$> toGE ma1
   
instance (Val a1, Val a2, Tuple b) => PureMulti (a1 -> a2 -> b) where
    pureMulti f = \ma1 ma2 -> moTuple $ (\a1 a2 -> f [a1, a2]) <$> toGE ma1 <*> toGE ma2
    
-- dirty multi

instance Tuple a => DirtyMulti (SE a) where
    dirtyMulti = depMoTuple . return . ($ [])

instance (Val a1, Tuple b) => DirtyMulti (a1 -> SE b) where
    dirtyMulti f = \ma1 -> depMoTuple $ (\a1 -> f [a1]) <$> toGE ma1

instance (Val a1, Val a2, Tuple b) => DirtyMulti (a1 -> a2 -> SE b) where
    dirtyMulti f = \ma1 ma2 -> depMoTuple $ (\a1 a2 -> f [a1, a2]) <$> toGE ma1 <*> toGE ma2

