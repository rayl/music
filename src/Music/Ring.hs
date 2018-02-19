
-- Model pitch class space as R/12Z

module Music.Ring
    ( X, mkX, unX
    , Y(..), mkY
    )
where

import Test.QuickCheck


-- -----------------------------------------------------------------------------
-- the set of 12 equally spaced pitch classes

newtype X = X { unX :: Int }
    deriving (Eq, Show)

x_min = 0
x_max = 11
        
instance Bounded X where
    minBound = X x_min
    maxBound = X x_max
               
instance Arbitrary X where
    arbitrary = choose (x_min, x_max) >>= return . mkX

mkX :: Int -> X
mkX x | x < x_min = error $ "X too small: " ++ show x
      | x > x_max = error $ "X too large: " ++ show x
      | otherwise = X x


-- -----------------------------------------------------------------------------
-- the set of 12 pitch class intervals

newtype Y = Y { unY :: Int }
    deriving (Eq, Ord, Show)

y_min = 0
y_max = 11
        
instance Bounded Y where
    minBound = Y y_min
    maxBound = Y y_max
               
instance Arbitrary Y where
    arbitrary = choose (y_min, y_max) >>= return . mkY

mkY :: Int -> Y
mkY y | y < y_min = error $ "Y too small: " ++ show y
      | y > y_max = error $ "Y too large: " ++ show y
      | otherwise = Y y
