{-# LANGUAGE LambdaCase #-}

module Day1 (solve) where

import Control.Applicative (asum)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

import Common (Day (..))

solve :: String -> (Int, Int)
solve s = (solveDay I s, solveDay II s)
 where
  solveDay day = sum . map (solveLine day) . lines
  solveLine day s =
    let ps = mapMaybe (parse day) $ tails s
     in 10 * head ps + last ps
  parse day s = asum $ map (parse' s) $ nums day
  parse' s (pat, val) = if pat `isPrefixOf` s then Just val else Nothing
  nums = \case
    I -> nums1
    II -> nums1 <> nums2
  nums1 =
    [ ("1", 1)
    , ("2", 2)
    , ("3", 3)
    , ("4", 4)
    , ("5", 5)
    , ("6", 6)
    , ("7", 7)
    , ("8", 8)
    , ("9", 9)
    ]
  nums2 =
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]
