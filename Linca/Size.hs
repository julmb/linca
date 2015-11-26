module Linca.Size () where

import Numeric.Natural

class Size a where
	size :: a -> Natural

instance Size a => Size [a] where
	size = sum . map size
