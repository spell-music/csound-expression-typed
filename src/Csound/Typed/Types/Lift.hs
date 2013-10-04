{-# Language FlexibleInstances #-}
module Csound.Typed.Types.Lift(
    -- * Lifters
    -- ** Pure single
    PureSingle, pureSingle,

    -- ** Dirty single
    DirtySingle, dirtySingle,

    -- ** Procedure
    Procedure, procedure,

    -- ** Pure multi 
    PureMulti, Pm, fromPm, fromPmOut, pureMulti,

    -- ** Dirty multi
    DirtyMulti, Dm, fromDm, fromDmOut, dirtyMulti
        
) where

import Control.Applicative

import Csound.Dynamic
import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.GlobalState
    
pureSingle :: PureSingle a => ([E] -> E) -> a
pureSingle = pureSingleGE . return
    
dirtySingle :: DirtySingle a => ([E] -> Dep E) -> a
dirtySingle = dirtySingleGE . return
    
procedure :: Procedure a => ([E] -> Dep ()) -> a
procedure = procedureGE . return

newtype Pm = Pm (GE (MultiOut [E]))

pureMulti :: PureMulti a => ([E] -> MultiOut [E]) -> a
pureMulti = pureMultiGE . return

newtype Dm = Dm (GE (MultiOut (Dep [E])))

dirtyMulti :: DirtyMulti a => ([E] -> MultiOut (Dep [E])) -> a
dirtyMulti = dirtyMultiGE . return

class PureSingle a where
    pureSingleGE :: GE ([E] -> E) -> a

class DirtySingle a where
    dirtySingleGE :: GE ([E] -> Dep E) -> a

class Procedure a where
    procedureGE :: GE ([E] -> Dep ()) -> a
    
class PureMulti a where
    pureMultiGE :: GE ([E] -> MultiOut [E]) -> a
    
class DirtyMulti a where
    dirtyMultiGE :: GE ([E] -> MultiOut (Dep [E])) -> a

-- multi out helpers

fromPm :: Tuple a => Pm -> a
fromPm (Pm a) = res
    where res = toTuple $ fmap ( $ tupleArity res) a

fromPmOut :: (Out a) => Pm -> a
fromPmOut = tupleToOut . fromPm

fromDm :: Tuple a => Dm -> SE a
fromDm (Dm a) = res
    where 
        res = fmap toTuple $ fromDep $ fmap ( $ (tupleArity $ proxy res)) a

        proxy :: SE a -> a
        proxy = const undefined

fromDmOut :: (Out a) => Dm -> SE a
fromDmOut = fmap tupleToOut . fromDm

--fromDmOut :: (Tuple a, Out a) => 

-- pure single

ps0 :: (Val a) => GE ([E] -> E) -> a
ps0 = fromGE . fmap ($ [])

ps1 :: (Val a, PureSingle b) => GE ([E] -> E) -> (a -> b)
ps1 mf = \ma -> pureSingleGE $ (\f a as -> f (a:as)) <$> mf <*> toGE ma

pss :: (Val a, PureSingle b) => GE ([E] -> E) -> ([a] -> b)
pss mf = \mas -> pureSingleGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mapM toGE mas

instance PureSingle Sig  where   pureSingleGE = ps0
instance PureSingle D    where   pureSingleGE = ps0
instance PureSingle Str  where   pureSingleGE = ps0
instance PureSingle Tab  where   pureSingleGE = ps0
instance PureSingle Spec where   pureSingleGE = ps0

instance (PureSingle b) => PureSingle (Sig -> b)    where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (D -> b)      where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Str -> b)    where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Tab -> b)    where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Spec -> b)   where   pureSingleGE = ps1

instance (PureSingle b) => PureSingle ([Sig] -> b)  where   pureSingleGE = pss
instance (PureSingle b) => PureSingle ([D] -> b)    where   pureSingleGE = pss

-- dirty single

ds0 :: (Val a) => GE ([E] -> Dep E) -> SE a
ds0 = fmap fromGE . fromDep . fmap ($ [])

ds1 :: (Val a, DirtySingle b) => GE ([E] -> Dep E) -> (a -> b)
ds1 mf = \ma -> dirtySingleGE $ (\f a as -> f (a:as)) <$> mf <*> toGE ma

dss :: (Val a, DirtySingle b) => GE ([E] -> Dep E) -> ([a] -> b)
dss mf = \mas -> dirtySingleGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mapM toGE mas

instance DirtySingle (SE Sig)  where   dirtySingleGE = ds0
instance DirtySingle (SE D)    where   dirtySingleGE = ds0
instance DirtySingle (SE Str)  where   dirtySingleGE = ds0
instance DirtySingle (SE Tab)  where   dirtySingleGE = ds0
instance DirtySingle (SE Spec) where   dirtySingleGE = ds0

instance (DirtySingle b) => DirtySingle (Sig -> b)    where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (D -> b)      where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Str -> b)    where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Tab -> b)    where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Spec -> b)   where   dirtySingleGE = ds1

instance (DirtySingle b) => DirtySingle ([Sig] -> b)  where   dirtySingleGE = dss
instance (DirtySingle b) => DirtySingle ([D] -> b)    where   dirtySingleGE = dss

-- procedure

instance Procedure (SE ()) where
    procedureGE = fromDep_ . fmap ($ [])

pr1 :: (Val a, Procedure b) => GE ([E] -> Dep ()) -> a -> b
pr1 mf = \ma -> procedureGE $ (\f a as -> f (a:as)) <$> mf <*> toGE ma

prs :: (Val a, Procedure b) => GE ([E] -> Dep ()) -> ([a] -> b)
prs mf = \mas -> procedureGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mapM toGE mas

instance (Procedure b) => Procedure (Sig -> b)    where   procedureGE = pr1
instance (Procedure b) => Procedure (D -> b)      where   procedureGE = pr1
instance (Procedure b) => Procedure (Str -> b)    where   procedureGE = pr1
instance (Procedure b) => Procedure (Tab -> b)    where   procedureGE = pr1
instance (Procedure b) => Procedure (Spec -> b)   where   procedureGE = pr1

instance (Procedure b) => Procedure ([Sig] -> b)  where   procedureGE = prs
instance (Procedure b) => Procedure ([D] -> b)    where   procedureGE = prs

-- pure multi

instance PureMulti Pm where
    pureMultiGE = Pm . fmap ($ [])

pm1 :: (Val a, PureMulti b) => GE ([E] -> MultiOut [E]) -> (a -> b)
pm1 mf = \ma -> pureMultiGE $ (\f a as -> f (a:as)) <$> mf <*> toGE ma

pms :: (Val a, PureMulti b) => GE ([E] -> MultiOut [E]) -> ([a] -> b)
pms mf = \mas -> pureMultiGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mapM toGE mas

instance (PureMulti b) => PureMulti (Sig -> b)    where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (D -> b)      where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Str -> b)    where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Tab -> b)    where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Spec -> b)   where   pureMultiGE = pm1

instance (PureMulti b) => PureMulti ([Sig] -> b)  where   pureMultiGE = pms
instance (PureMulti b) => PureMulti ([D] -> b)    where   pureMultiGE = pms

-- dirty multi

instance DirtyMulti Dm where
    dirtyMultiGE = Dm . fmap ($ [])

dm1 :: (Val a, DirtyMulti b) => GE ([E] -> MultiOut (Dep [E])) -> (a -> b)
dm1 mf = \ma -> dirtyMultiGE $ (\f a as -> f (a:as)) <$> mf <*> toGE ma

dms :: (Val a, DirtyMulti b) => GE ([E] -> MultiOut (Dep [E])) -> ([a] -> b)
dms mf = \mas -> dirtyMultiGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mapM toGE mas

instance (DirtyMulti b) => DirtyMulti (Sig -> b)    where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (D -> b)      where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Str -> b)    where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Tab -> b)    where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Spec -> b)   where   dirtyMultiGE = dm1

instance (DirtyMulti b) => DirtyMulti ([Sig] -> b)  where   dirtyMultiGE = dms
instance (DirtyMulti b) => DirtyMulti ([D] -> b)    where   dirtyMultiGE = dms

