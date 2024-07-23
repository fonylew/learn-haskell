import Control.DeepSeq (rnf2)
square :: Float -> Float
square n = n ^ 2

newtype Circle = Circle { radius :: Float }
                deriving(Show, Eq)

newtype Square = Square { side :: Float }
                deriving(Show, Eq)

-- doable แต่ใส่ radius ไม่ได้
-- data Shape' = Circle' Circle | Square' Square
--                 deriving(Show, Eq)

-- areaShape' :: Shape' -> Float
-- areaShape' (Circle' r) = pi * square r
-- areaShape' (Square' s) = square s

--------

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

-- mkDonut :: Float -> Float -> Donut
-- mkDonut r1 r2
--     | r1 > r2 = Donut (Circle r1) (Circle r2)
--     | otherwise = Donut (Circle r2) (Circle r1)

mkDonut :: Circle -> Circle -> Donut
mkDonut c1 c2
    | c1 > c2 = Donut c1 c2
    | otherwise = Donut c2 c1

instance Shape Donut where
    area :: Donut -> Float
    area donut = area (outterCircle donut) - area (innerCircle donut)

