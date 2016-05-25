{-# LANGUAGE RankNTypes #-}

module Linca.Lens () where

-- TODO: change this to import Data.Functor.Const after upgrading to base 4.9
import Control.Applicative
import Data.Functor.Identity

type Lens object value = forall f. Functor f => (value -> f value) -> (object -> f object)
type View object value = object -> value
type Over object value = (value -> value) -> (object -> object)
type Set object value = value -> (object -> object)

lens :: View object value -> Over object value -> Lens object value
-- view :: object -> value
-- over :: (value -> value) -> (object -> object)
-- f :: value -> f value
-- object :: object
-- fmap :: (a -> b) -> (f a -> f b)
-- result :: f object
lens view over f object = fmap (\value -> over (const value) object) (f (view object))

lens' :: View object value -> Set object value -> Lens object value
lens' view set f object = fmap (\value -> set value object) (f (view object))

view :: Lens object value -> View object value
view lens = getConst . lens Const

over :: Lens object value -> Over object value
over lens f = runIdentity . lens (Identity . f)
