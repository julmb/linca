module Linca.Range where

data Range value = Range { start :: value, end :: value } deriving (Eq, Show, Read)

contains :: Ord value => value -> Range value -> Bool
contains value range
	| start range > end range = error "Linca.Range.contains: parameter range is not a well-formed ordered range"
	| otherwise               = value >= start range && value <= end range
