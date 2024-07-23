import Data.Time (DayOfWeek(Sunday))
data Boolean = Yes | No
                deriving (Show, Eq)

toString :: Boolean -> String
toString Yes = "Yes"
toString No = "No"

-- instance Show Boolean where
--     show :: Boolean -> String
--     show = toString

-- :i Show
-- instance Show Boolean -- Defined at class3.hs:7:10

not' :: Boolean -> Boolean
not' b
    | b == Yes = No
    | otherwise = Yes

-- Ord ช่วยให้เปรียบทียบกันได้
data Weekday = Mon | Tue | Wed| Thu | Fri | Sat | Sunday
                deriving (Show, Eq) --, Ord)

-- type Fullname = String
-- type Nicknme = String

-- data Person = Person Fullname Nicknme deriving (Show)

data Score = Score { midtermScore :: Int
                    , finalScore :: Int
                    , homeworkScore :: Int
                    , projectScore :: Int
                    } deriving (Show)

totalScore :: Score -> Int
totalScore s = midtermScore s + finalScore s + homeworkScore s + projectScore s

instance Eq Score where
    (==) :: Score -> Score -> Bool
    s1 == s2 = totalScore s1 == totalScore s2

-- Recursive type
data Nat = Zero | Succ Nat
            deriving (Show, Eq, Ord)

instance Num Nat where
    (+) :: Nat -> Nat -> Nat
    m + Zero = m
    m + (Succ n) = Succ (m + n)

    m * Zero = Zero
    m * (Succ n) = m * n + m

toInt :: Nat -> Integer
toInt Zero = 0
toInt (Succ n) = 1 + toInt n

-- Stack
data Stack a = EmptyStack | Stack a (Stack a)
                deriving (Show)

push :: a -> Stack a -> Stack a
-- push x stk = Stack x stk
push = Stack

-- Optional:
-- pop :: Stack a -> Maybe (Stack a)
-- pop EmptyStack = Nothing

pop :: Stack a -> Stack a
pop EmptyStack = error "cannot pop empty stack"
pop (Stack _ stk) = stk

top :: Stack a -> a
top EmptyStack = error " empty stack has no top element"
top (Stack x _) = x

-- Build stack from list
newtype StackL a = StackL [a] deriving (Show)

push' :: a -> StackL a -> StackL a
push' x (StackL xs) = StackL (x:xs)

pop' :: StackL a -> StackL a
pop' (StackL []) = error "cannot pop empty stack"
pop' (StackL (_:xs)) = StackL xs

top' :: StackL a -> a
top' (StackL []) = error "empty stack has no top element"
top' (StackL (x:_)) = x
