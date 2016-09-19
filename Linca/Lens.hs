module Linca.Lens (Lens, lens, view, set, over, at) where

import Data.Map
import Data.Functor.Const
import Data.Functor.Identity

type Lens object value = forall f. Functor f => (value -> f value) -> (object -> f object)

lens :: (object -> value) -> (value -> object -> object) -> Lens object value
lens view set f object = fmap (flip set object) (f (view object))

view :: Lens object value -> (object -> value)
view lens = getConst . lens Const

set :: Lens object value -> value -> (object -> object)
set lens value = over lens (const value)

over :: Lens object value -> (value -> value) -> (object -> object)
over lens f = runIdentity . lens (Identity . f)

at :: Ord key => key -> Lens (Map key value) value
at key = lens g s where
	g map = map ! key
	s value map = insert key value map
