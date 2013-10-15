module Csound.Typed.Types(
    -- * Primitives
    module Csound.Typed.Types.Prim,
    module Csound.Typed.Types.Lift,
    -- * Init values
    withInits, withDs, withSigs, withTabs, withD, withSig, withTab, withSeed,
    -- * Tuples
    module Csound.Typed.Types.Tuple,        
    -- * Events
    module Csound.Typed.Types.Evt
) where

import Control.Applicative
import qualified Csound.Dynamic as D

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.Types.Evt
import Csound.Typed.Types.Lift

import Csound.Typed.GlobalState(evalSE, SE)

-- appends inits

withInits :: (Val a, Tuple b) => a -> b -> a
withInits a b = genWithInits a (fromTuple b)

withDs :: Val a => a -> [D] -> a
withDs a ds = genWithInits a (mapM toGE ds)

withD :: Val a => a -> D -> a
withD = withInits

withSigs :: Val a => a -> [Sig] -> a
withSigs a sigs = genWithInits a (mapM toGE sigs)

withSig :: Val a => a -> Sig -> a
withSig = withInits

withTabs :: Val a => a -> [Tab] -> a
withTabs a tabs = genWithInits a (mapM toGE tabs)

withTab :: Val a => a -> Tab -> a
withTab = withInits

withSeed :: SE Sig -> D -> Sig
withSeed a d = evalSE $ fmap (flip withD d) a

genWithInits :: (Val a) => a -> GE [E] -> a
genWithInits a vals = fromGE $ liftA2 D.withInits (toGE a) vals




