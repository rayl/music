{-# LANGUAGE TemplateHaskell #-}

module Music.Util
    ( rotate
    , enums
    , mm
    , runTests
    )
where

import Test.QuickCheck

-- -----------------------------------------------------------------------------
-- rotate a list

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = let n' = mod n (length xs)
              in zipWith const (drop n' (cycle xs)) xs

-- the length of xs does not change when rotating
prop_rotate_length n xs = length (rotate n xs) == length xs

-- the first element of rotated non-empty list is the n mod length
-- element of xs
prop_rotate_head n xs = xs /= [] ==>
                        let n' = mod n (length xs)
                        in head (rotate n xs) == xs !! n'
    

-- -----------------------------------------------------------------------------
-- list all enums of a type, in order

enums :: (Bounded e, Enum e) => [e]
enums = enumFrom minBound


-- -----------------------------------------------------------------------------
-- calculate modulo for a Bounded Enum
mm :: (Bounded e, Enum e) => e -> Int
mm e = fromEnum (maxBound `asTypeOf` e) + 1


-- -----------------------------------------------------------------------------
-- QuickCheck test harness

return []
runTests' = quickCheckWithResult stdArgs { maxSuccess = 1000 }
runTests = $forAllProperties runTests'
