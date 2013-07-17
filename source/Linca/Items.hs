module Linca.Items where

equal :: Eq a => a -> a -> a
equal a b
	| a /= b = error "Linca.Items.equal: items were not equal"
	| otherwise = a
