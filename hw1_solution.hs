square :: Float -> Float
square n = n ^ 2

half :: Float -> Float
half n = n / 2

isSmallEnough :: Float -> Bool
isSmallEnough n = n < epsilon
    where
        epsilon = 0.001

hypotanuse :: Float -> Float -> Float
hypotanuse a b = sqrt $ square a + square b

hypotanuseEq :: Float -> Float
hypotanuseEq a = hypotanuse a a

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

class HasArea a where
    area :: a -> Float

instance HasArea Circle where
    area :: Circle -> Float
    area c = pi * square (radius c)

instance HasArea Square where
    area :: Square -> Float
    area s = square (side s)

data Donut = Donut { outterCircle :: Circle
                    , innerCircle' :: Circle
                    } deriving(Show, Eq)

-- mkDonut :: Float -> Float -> Donut
-- mkDonut r1 r2
--     | r1 > r2 = Donut (Circle r1) (Circle r2)
--     | otherwise = Donut (Circle r2) (Circle r1)

mkDonut :: Circle -> Circle -> Donut
mkDonut c1 c2
    | radius c1 > radius c2 = Donut c1 c2
    | otherwise = Donut c2 c1

instance HasArea Donut where
    area :: Donut -> Float
    area donut = area (outterCircle donut) - area (innerCircle' donut)

data Petal = Petal { frameSquare :: Square
                   , innerCircle :: Circle
                   , innerSquare :: Square
                   } deriving(Show, Eq)

mkPetal :: Float -> Petal
mkPetal frameSize = Petal frameSquare innerCircle innerSquare
    where
        frameSquare = Square frameSize
        innerCircle = Circle halfFrameSize
        innerSquare = Square (hypotanuseEq halfFrameSize)
        halfFrameSize = half frameSize

instance HasArea Petal where
    area :: Petal -> Float
    area petal = area (frameSquare petal) - area (innerSquare petal) + area (innerCircle petal)

-- Recursive type
data FramedFlower = EmptyFlower
                    | Flower { petal :: Petal
                                   , innerFlower :: FramedFlower
                                   }
                    deriving(Show, Eq)

mkFramedFlower :: Float -> FramedFlower
mkFramedFlower frameSize
    | isSmallEnough frameSize = EmptyFlower
    | otherwise               = Flower petal innerFlower
    where
        petal           = mkPetal frameSize
        innerFlower     = mkFramedFlower (half frameSize)
        innerFrameSize  = hypotanuseEq (half frameSize)

instance HasArea FramedFlower where
    area :: FramedFlower -> Float
    area flower
        | flower == EmptyFlower = 0
        | otherwise             = area (petal flower) + area (innerFlower flower)

data Shape = forall a. (Show a, Eq a, HasArea a) => Shape { shape :: a }

deriving instance Show Shape

instance HasArea Shape where
    area :: Shape -> Float
    area (Shape s) = area s

instance Eq Shape where
    (==) :: Shape -> Shape -> Bool
    -- a and b are not the same type
    -- (Shape a) == (Shape b) = a == b
    -- compare string instead
    a == b = show a == show b

instance Ord Shape where
    (<=) :: Shape -> Shape -> Bool
    a <= b = area a <= area b

-- sort ได้แล้วว