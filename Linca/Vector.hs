module Linca.Vector (Vector2 (Vector2), Coordinates (vx, vy), VectorScaling ((*.), (.*)), VectorAddition ((.+.))) where

infix 7 *.
infix 7 .*
infix 6 .+.

class Coordinates vector value | vector -> value where
	vx :: vector -> value
	vy :: vector -> value

class VectorScaling vector value | vector -> value where
	(*.) :: value -> vector -> vector
	factor *. vector = vector .* factor
	(.*) :: vector -> value -> vector
	vector .* factor = factor *. vector

class VectorAddition vector where
	(.+.) :: vector -> vector -> vector

data Vector2 value = Vector2 value value deriving (Eq, Show, Read)

instance Coordinates (Vector2 value) value where
	vx (Vector2 x _) = x
	vy (Vector2 _ y) = y

instance Num value => VectorScaling (Vector2 value) value where
	factor *. vector = Vector2 (factor * vx vector) (factor * vy vector)

instance Num value => VectorAddition (Vector2 value) where
	vector0 .+. vector1 = Vector2 (vx vector0 + vx vector1) (vy vector0 + vy vector1)
