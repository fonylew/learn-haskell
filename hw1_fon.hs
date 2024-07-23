import Data.List

data Shape = Circle Float | Square Float

-- task 1
area :: Shape -> Float
area (Circle radius) = pi * radius ^ 2

-- task 2
area (Square side) = side ^ 2

-- task 3
areaDonut :: Shape -> Shape -> Float
areaDonut (Circle outer) (Circle inner) = abs(area (Circle outer) - area (Circle inner))

-- task 4
hypotanuse :: Float -> Float
hypotanuse a = a * sqrt 2

halfHypotanuse :: Float -> Float
halfHypotanuse a = hypotanuse a / 2

areaVisibleCircle :: Float -> Float
areaVisibleCircle side = area (Circle (side / 2) ) - area (Square (halfHypotanuse side))

-- task 5
epsilon :: Float
epsilon = 0.01

tooSmall :: Float -> Float -> Bool
tooSmall number epsilon = number < epsilon

areaOfShape :: Float -> Float
areaOfShape side = if tooSmall (currentAreaOfShape - nextAreaOfShape) epsilon then 0  else currentAreaOfShape + areaOfShape(halfHypotanuse side)
    where
        currentAreaOfShape = areaVisibleCircle side
        nextAreaOfShape = areaVisibleCircle(halfHypotanuse side)

-- task 6
task6 = map areaOfShape [5, 6 .. 17]