{-# Language TypeFamilies #-}
module Csound.Typed.Types(
    Sig, D, Tab, Str, Spec, BoolSig, BoolD, Val(..),
    
    -- * constructors
    double, int, str,

    -- * converters
    ar, kr, ir, sig,

    -- * lifters
    on0, on1, on2, on3,

    -- * numeric funs
    ceilSig, floorSig, roundSig, intSig, fracSig,
    ceilD, floorD, roundD, intD, fracD        
    
) where

import Control.Applicative hiding ((<*))
import Data.Monoid

import Data.Default
import Data.Boolean

import Csound.Dynamic hiding (double, int, str)
import qualified Csound.Dynamic as D(double, int, str)
import Csound.Typed.Control

-- | Signals
newtype Sig  = Sig  { unSig :: GE E }

-- | Constant numbers
newtype D    = D    { unD   :: GE E }

-- | Strings
newtype Str  = Str  { unStr :: GE E }

-- | Spectrums
newtype Spec = Spec { unSpec :: GE E }

-- Booleans

newtype BoolSig = BoolSig { unBoolSig :: GE E }
newtype BoolD   = BoolD   { unBoolD   :: GE E }

type instance BooleanOf Sig  = BoolSig

type instance BooleanOf D    = BoolD
type instance BooleanOf Str  = BoolD
type instance BooleanOf Tab  = BoolD
type instance BooleanOf Spec = BoolD

-- tables

-- | Tables (or arrays)
newtype Tab  = Tab  { unTab :: TabE }

data TabE 
    = TabPlain (GE E)
    | TabPre PreTab

data PreTab = PreTab

renderTab :: PreTab -> GE E
renderTab = undefined

-------------------------------------------------------------------------------
-- constructors

double :: Double -> D
double = fromE . D.double

int :: Int -> D
int = fromE . D.int

str :: String -> Str
str = fromE . D.str

-------------------------------------------------------------------------------
-- converters

ar :: Sig -> Sig
ar = on1 $ setRate Ar

kr :: Sig -> Sig
kr = on1 $ setRate Kr

ir :: Sig -> D
ir = on1 $ setRate Ir

sig :: D -> Sig
sig = on1 $ setRate Kr

-------------------------------------------------------------------------------
-- single wrapper

class Val a where
    fromGE  :: GE E -> a
    toGE    :: a -> GE E

    fromE   :: E -> a
    fromE = fromGE . return

instance Val Sig    where { fromGE = Sig    ; toGE = unSig  }
instance Val D      where { fromGE = D      ; toGE = unD    }
instance Val Str    where { fromGE = Str    ; toGE = unStr  }
instance Val Spec   where { fromGE = Spec   ; toGE = unSpec }

instance Val Tab where 
    fromGE = Tab . TabPlain 
    toGE x = case unTab x of
        TabPlain a -> a
        TabPre   a -> renderTab a

instance Val BoolSig where { fromGE = BoolSig ; toGE = unBoolSig }
instance Val BoolD   where { fromGE = BoolD   ; toGE = unBoolD   }

on0 :: Val a => E -> a
on0 = fromE

on1 :: (Val a, Val b) => (E -> E) -> (a -> b)
on1 f a = fromGE $ fmap f $ toGE a

on2 :: (Val a, Val b, Val c) => (E -> E -> E) -> (a -> b -> c)
on2 f a b = fromGE $ liftA2 f (toGE a) (toGE b)

on3 :: (Val a, Val b, Val c, Val d) => (E -> E -> E -> E) -> (a -> b -> c -> d)
on3 f a b c = fromGE $ liftA3 f (toGE a) (toGE b) (toGE c)

-------------------------------------------------------------------------------
-- defaults

instance Default Sig    where def = 0
instance Default D      where def = 0
instance Default Tab    where def = fromE 0
instance Default Str    where def = str ""
instance Default Spec   where def = fromE 0 

-------------------------------------------------------------------------------
-- monoid

instance Monoid Sig     where { mempty = on0 mempty     ; mappend = on2 mappend }
instance Monoid D       where { mempty = on0 mempty     ; mappend = on2 mappend }

-------------------------------------------------------------------------------
-- numeric

instance Num Sig where 
    { (+) = on2 (+); (*) = on2 (*); negate = on1 negate; (-) = on2 (\a b -> a - b)   
    ; fromInteger = on0 . fromInteger; abs = on1 abs; signum = on1 signum }

instance Num D where 
    { (+) = on2 (+); (*) = on2 (*); negate = on1 negate; (-) = on2 (\a b -> a - b)   
    ; fromInteger = on0 . fromInteger; abs = on1 abs; signum = on1 signum }

instance Fractional Sig  where { (/) = on2 (/);    fromRational = on0 . fromRational }
instance Fractional D    where { (/) = on2 (/);    fromRational = on0 . fromRational }

instance Floating Sig where
    { pi = on0 pi;  exp = on1 exp;  sqrt = on1 sqrt; log = on1 log;  logBase = on2 logBase; (**) = on2 (**)
    ;  sin = on1 sin;  tan = on1 tan;  cos = on1 cos; sinh = on1 sinh; tanh = on1 tanh; cosh = on1 cosh
    ; asin = on1 asin; atan = on1 atan;  acos = on1 acos ; asinh = on1 asinh; acosh = on1 acosh; atanh = on1 atanh }

instance Floating D where
    { pi = on0 pi;  exp = on1 exp;  sqrt = on1 sqrt; log = on1 log;  logBase = on2 logBase; (**) = on2 (**)
    ;  sin = on1 sin;  tan = on1 tan;  cos = on1 cos; sinh = on1 sinh; tanh = on1 tanh; cosh = on1 cosh
    ; asin = on1 asin; atan = on1 atan;  acos = on1 acos ; asinh = on1 asinh; acosh = on1 acosh; atanh = on1 atanh }

ceilSig, floorSig, fracSig, intSig, roundSig :: Sig -> Sig

ceilSig = on1 ceilE;    floorSig = on1 floorE;  fracSig = on1 fracE;  intSig = on1 intE;    roundSig = on1 roundE

ceilD, floorD, fracD, intD, roundD :: D -> D

ceilD = on1 ceilE;    floorD = on1 floorE;  fracD = on1 fracE;  intD = on1 intE;    roundD = on1 roundE

errorMsg :: String -> a
errorMsg name = error $ name ++ " is not defined for Csound values"

enum1 :: Val a => (E -> [E]) -> a -> [a]
enum1 f a = fmap fromE $ f $ unsafePerformGE $ toGE a

enum2 :: Val a => (E -> E -> [E]) -> a -> a -> [a]
enum2 f a b = fmap fromE $ f (unsafePerformGE $ toGE a) (unsafePerformGE $ toGE b)

enum3 :: Val a => (E -> E -> E -> [E]) -> a -> a -> a -> [a]
enum3 f a b c = fmap fromE $ f (unsafePerformGE $ toGE a) (unsafePerformGE $ toGE b) (unsafePerformGE $ toGE c)

instance Enum Sig where
    { succ = on1 succ;  pred = on1 pred; toEnum = on0 . toEnum ; fromEnum = errorMsg "fromEnum" 
    ; enumFrom = enum1 enumFrom; enumFromThen = enum2 enumFromThen; enumFromTo = enum2 enumFromTo; enumFromThenTo = enum3 enumFromThenTo }

instance Enum D where
    { succ = on1 succ;  pred = on1 pred; toEnum = on0 . toEnum ; fromEnum = errorMsg "fromEnum"    
    ; enumFrom = enum1 enumFrom; enumFromThen = enum2 enumFromThen; enumFromTo = enum2 enumFromTo; enumFromThenTo = enum3 enumFromThenTo }

instance Real Sig where toRational = errorMsg "toRational" 
instance Real D   where toRational = errorMsg "toRational" 

instance Integral Sig where 
    { quot = on2 quot;  rem = on2 rem;  div = on2 div;  mod = on2 mod; toInteger = errorMsg "toInteger"
    ; quotRem a b = (quot a b, rem a b) }

instance Integral D   where 
    { quot = on2 quot;  rem = on2 rem;  div = on2 div;  mod = on2 mod; toInteger = errorMsg "toInteger" 
    ; quotRem a b = (quot a b, rem a b) }
  
instance Ord Sig where { compare = errorMsg "compare" }
instance Ord D   where { compare = errorMsg "compare" }
  
instance Eq Sig where { (==) = errorMsg "(==)" }
instance Eq D   where { (==) = errorMsg "(==)" }

-------------------------------------------------------------------------------
-- logic

instance Boolean BoolSig  where { true = on0 true;  false = on0 false;  notB = on1 notB;  (&&*) = on2 (&&*);  (||*) = on2 (||*) }
instance Boolean BoolD    where { true = on0 true;  false = on0 false;  notB = on1 notB;  (&&*) = on2 (&&*);  (||*) = on2 (||*) }

instance IfB Sig  where ifB = on3 ifB
instance IfB D    where ifB = on3 ifB
instance IfB Tab  where ifB = on3 ifB
instance IfB Str  where ifB = on3 ifB
instance IfB Spec where ifB = on3 ifB

instance EqB Sig  where { (==*) = on2 (==*);    (/=*) = on2 (/=*) }
instance EqB D    where { (==*) = on2 (==*);    (/=*) = on2 (/=*) }

instance OrdB Sig where { (<*)  = on2 (<*) ;    (>*)  = on2 (>*);     (<=*) = on2 (<=*);    (>=*) = on2 (>=*) }
instance OrdB D   where { (<*)  = on2 (<*) ;    (>*)  = on2 (>*);     (<=*) = on2 (<=*);    (>=*) = on2 (>=*) }

