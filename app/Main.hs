module Main where

import Day1

main :: IO ()
main = getContents >>= print . solve
