square :: Float -> Float
square n = n ^ 2

newtype Circle = Circle { radius :: Float }
                deriving(Show, Eq)

newtype Square = Square { side :: Float }
                deriving(Show, Eq)

class Shape a where
    area :: a -> Float

instance Shape Circle where
    area :: Circle -> Float
    area c = pi * square (radius c)

instance Shape Square where
    area :: Square -> Float
    area s = square (side s)

data Donut = Donut { outterCircle :: Circle
                    , innerCircle :: Circle
                    } deriving(Show, Eq)


