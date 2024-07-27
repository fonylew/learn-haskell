import Data.List
import Data.Char
import GHC.IO.Handle (hGetContents')

-- Nothing = None, Just a = Some a
-- :i Maybe
-- Maybe a = Nothing | Just a
-- instance Applicative Maybe -- Defined in ‘GHC.Base’
-- instance Functor Maybe -- Defined in ‘GHC.Base’
-- instance Monad Maybe -- Defined in ‘GHC.Base’

data Optional a = None | Some a
                    deriving (Show, Eq)

-- มี optional ของ function ได้
-- :t Optional(Some sort)

f :: Int -> Int
f _ = 7

g :: Int -> Int
g _ = 9

h :: Int -> Int
h _ = 17

f' :: Int -> Optional Int
f' n
    | n == 17   = None
    | otherwise = Some n

g' :: Int -> Optional Int
g' n
    | even n    = None
    | otherwise = Some n

h' :: Int -> Optional Int
h' n
    | even n    = Some (n+1)
    | otherwise = None

-- harder to compose function
-- fog' :: Int -> Optional Int
fog' :: Int -> Optional Int
fog' n = 
    case g' n of
        None    -> None
        Some x  -> f' x

-- มันต้องมี if เกิดขึ้น เป็น pyramid
fogoh' :: Int -> Optional Int
fogoh' n = 
    case h' n of
        None    -> None
        Some x  -> case g' x of
                    None    -> None
                    Some y  -> f' y

-- ต้องทำ optional chaining

    -- [For reference]
    -- data Optional a = None | Some a
    --                     deriving (Show, Eq)

-- Optional เป็น Functor ได้
instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap _ None     = None
    fmap f (Some x) = Some (f x)

instance Applicative Optional where
    pure :: a -> Optional a
    pure = Some

    (<*>) :: Optional (a -> b) -> Optional a -> Optional b
    _ <*> None            = None
    None <*> (Some x)     = None
    (Some f) <*> (Some x) = Some (f x)
    -- None <*> _ = None
    -- _ <*> None = None
    -- (Some f) <*> (Some x) = Some (f x)

instance Monad Optional where
    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    x >>= f = case x of
                None    -> None
                Some x  -> f x

-- [For Reference]
-- fogoh' :: Int -> Optional Int
-- fogoh' n = 
--     case h' n of
--         None    -> None
--         Some x  -> case g' x of
--                     None    -> None
--                     Some y  -> f' y

-- fogoh ที่อยากได้
-- (a -> m b) -> m b

-- :t return
-- return :: Monad m => a -> m a

fogohM :: Int -> Optional Int
-- fogohM n = return n >>= h' >>= g' >>= f'
fogohM n = h' n >>= g' >>= f'

-- syntax sugar "do"
-- fogohM' n = 
--     h' n >>= (\y ->
--     g' y >>= (\z ->
--     f' z))

fogohM' :: Int -> Optional Int
fogohM' n = do
    y1 <- h' n
    y2 <- g' y1
    f' y2

fogohM'' n = do
    y <- h' n
    z <- g' y
    return $ f' z
