module Main where

import Music
import Music.Util

main :: IO Bool
main = do
    Music.Util.runTests
    Music.runTests
