{-# Language TypeFamilies #-}
module Csound.Typed.Types.Prim(
    Sig, D, Tab, Str, Spec, Wspec, BoolSig, BoolD, Val(..), hideGE, SigOrD,

    -- ** Tables
    preTab, TabSize(..), TabArgs(..), updateTabSize,
    fromPreTab, getPreTabUnsafe, skipNorm, forceNorm,

    -- ** constructors
    double, int, str, idur,

    -- ** converters
    ar, kr, ir, sig,

    -- ** lifters
    on0, on1, on2, on3,

    -- ** numeric funs
    quotSig, remSig, divSig, modSig, ceilSig, floorSig, roundSig, intSig, fracSig,
    quotD, remD, divD, modD, ceilD, floorD, roundD, intD, fracD,        
   
    -- ** logic funs
    when, boolSig
) where

import Control.Applicative hiding ((<*))
import Control.Monad hiding (when)
import Data.Monoid
import qualified Data.IntMap as IM

import Data.Default
import Data.Boolean

import Csound.Dynamic hiding (double, int, str, when)
import qualified Csound.Dynamic as D(double, int, str, when)
import Csound.Typed.GlobalState

-- | Signals
newtype Sig  = Sig  { unSig :: GE E }

-- | Constant numbers
newtype D    = D    { unD   :: GE E }

-- | Strings
newtype Str  = Str  { unStr :: GE E }

-- | Spectrums
newtype Spec  = Spec  { unSpec  :: GE E }
newtype Wspec = Wspec { unWspec :: GE E }

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
data Tab  
    = TabExp (GE E)
    | TabPre PreTab

preTab :: TabSize -> Int -> TabArgs -> Tab
preTab size gen args = TabPre $ PreTab size gen args

data PreTab = PreTab
    { preTabSize    :: TabSize
    , preTabGen     :: Int
    , preTabArgs    :: TabArgs }

-- Table size.
data TabSize 
    -- Size is fixed by the user.
    = SizePlain Int
    -- Size is relative to the renderer settings.
    | SizeDegree 
    { hasGuardPoint :: Bool
    , sizeDegree    :: Int      -- is the power of two
    }

instance Default TabSize where
    def = SizeDegree
        { hasGuardPoint = False
        , sizeDegree = 0 }
    
-- Table arguments can be
data TabArgs 
    -- absolute
    = ArgsPlain [Double]
    -- or relative to the table size (used for tables that implement interpolation)
    | ArgsRelative [Double]
    -- GEN 16 uses unusual interpolation scheme, so we need a special case
    | ArgsGen16 [Double]
    | FileAccess String [Double]

renderTab :: PreTab -> GE E
renderTab a = saveGen =<< fromPreTab a 

getPreTabUnsafe :: String -> Tab -> PreTab
getPreTabUnsafe msg x = case x of
    TabPre a    -> a
    _           -> error msg

fromPreTab :: PreTab -> GE Gen
fromPreTab a = withOptions $ \opt -> go (setTabFi opt) a
    where
        go :: TabFi -> PreTab -> Gen
        go tabFi tab = Gen size (preTabGen tab) args file
            where size = defineTabSize (getTabSizeBase tabFi tab) (preTabSize tab)
                  (args, file) = defineTabArgs size (preTabArgs tab)

getTabSizeBase :: TabFi -> PreTab -> Int
getTabSizeBase tf tab = IM.findWithDefault (tabFiBase tf) (preTabGen tab) (tabFiGens tf)

defineTabSize :: Int -> TabSize -> Int
defineTabSize base x = case x of
       SizePlain n -> n
       SizeDegree guardPoint degree ->          
                byGuardPoint guardPoint $
                byDegree base degree
    where byGuardPoint guardPoint 
            | guardPoint = (+ 1)
            | otherwise  = id
            
          byDegree zero n = 2 ^ max 0 (zero + n) 

defineTabArgs :: Int -> TabArgs -> ([Double], Maybe String)
defineTabArgs size args = case args of
    ArgsPlain as -> (as, Nothing)
    ArgsRelative as -> (fromRelative size as, Nothing)
    ArgsGen16 as -> (formRelativeGen16 size as, Nothing)
    FileAccess filename as -> (as, Just filename)
    where fromRelative n as = substEvens (mkRelative n $ getEvens as) as
          getEvens xs = case xs of
            [] -> []
            _:[] -> []
            _:b:as -> b : getEvens as
            
          substEvens evens xs = case (evens, xs) of
            ([], as) -> as
            (_, []) -> []
            (e:es, a:_:as) -> a : e : substEvens es as
            _ -> error "table argument list should contain even number of elements"
            
          mkRelative n as = fmap ((fromIntegral :: (Int -> Double)) . round . (s * )) as
            where s = fromIntegral n / sum as
          
          -- special case. subst relatives for Gen16
          formRelativeGen16 n as = substGen16 (mkRelative n $ getGen16 as) as

          getGen16 xs = case xs of
            _:durN:_:rest    -> durN : getGen16 rest
            _                -> xs

          substGen16 durs xs = case (durs, xs) of 
            ([], as) -> as
            (_, [])  -> []
            (d:ds, valN:_:typeN:rest)   -> valN : d : (typeN * d) : substGen16 ds rest
            (_, _)   -> xs

-- | Skips normalization (sets table size to negative value)
skipNorm :: Tab -> Tab
skipNorm x = case x of
    TabExp _ -> error "you can skip normalization only for primitive tables (made with gen-routines)"
    TabPre a -> TabPre $ a{ preTabGen = negate $ abs $ preTabGen a }

-- | Force normalization (sets table size to positive value).
-- Might be useful to restore normalization for table 'Csound.Tab.doubles'.
forceNorm :: Tab -> Tab
forceNorm x = case x of
    TabExp _ -> error "you can force normalization only for primitive tables (made with gen-routines)"
    TabPre a -> TabPre $ a{ preTabGen = abs $ preTabGen a }

----------------------------------------------------------------------------
-- change table size

updateTabSize :: (TabSize -> TabSize) -> Tab -> Tab
updateTabSize phi x = case x of
    TabExp _ -> error "you can change size only for primitive tables (made with gen-routines)"
    TabPre a -> TabPre $ a{ preTabSize = phi $ preTabSize a }

-------------------------------------------------------------------------------
-- constructors

double :: Double -> D
double = fromE . D.double

int :: Int -> D
int = fromE . D.int

str :: String -> Str
str = fromE . D.str

idur :: D 
idur = fromE $ pn 3

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

hideGE :: Val a => GE a -> a
hideGE = fromGE . join . fmap toGE

instance Val Sig    where { fromGE = Sig    ; toGE = unSig  }
instance Val D      where { fromGE = D      ; toGE = unD    }
instance Val Str    where { fromGE = Str    ; toGE = unStr  }
instance Val Spec   where { fromGE = Spec   ; toGE = unSpec }
instance Val Wspec  where { fromGE = Wspec  ; toGE = unWspec}

instance Val Tab where 
    fromGE = TabExp 
    toGE x = case x of
        TabExp a -> a
        TabPre a -> renderTab a

instance Val BoolSig where { fromGE = BoolSig ; toGE = unBoolSig }
instance Val BoolD   where { fromGE = BoolD   ; toGE = unBoolD   }

class Val a => SigOrD a where

instance SigOrD Sig where
instance SigOrD D   where

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
quotSig, remSig, divSig, modSig :: Sig -> Sig -> Sig

ceilSig = on1 ceilE;    floorSig = on1 floorE;  fracSig = on1 fracE;  intSig = on1 intE;    roundSig = on1 roundE
quotSig = on2 quot; remSig = on2 rem;   divSig = on2 div;   modSig = on2 mod

ceilD, floorD, fracD, intD, roundD :: D -> D
quotD, remD, divD, modD :: Sig -> Sig -> Sig

ceilD = on1 ceilE;    floorD = on1 floorE;  fracD = on1 fracE;  intD = on1 intE;    roundD = on1 roundE
quotD = on2 quot;   remD = on2 rem; divD = on2 div; modD = on2 mod

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

when :: BoolSig -> SE () -> SE ()
when p body = do
    bodyDep <- fmap return body
    fromDep_ $ do
        pDep <- toGE p
        return $ D.when pDep bodyDep 

boolSig :: BoolD -> BoolSig
boolSig = fromGE . toGE

