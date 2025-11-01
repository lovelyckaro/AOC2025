module Solutions
  ( solution,
  )
where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromJust)
import Log
import SantaLib
import Solutions.Day01 (day01)
import Solutions.Day02 (day02)
import Solutions.Day03 (day03)
import Solutions.Day04 (day04)
import Solutions.Day05 (day05)
import Solutions.Day06 (day06)
import Solutions.Day07 (day07)
import Solutions.Day08 (day08)
import Solutions.Day09 (day09)
import Solutions.Day10 (day10)
import Solutions.Day11 (day11)
import Solutions.Day12 (day12)

solution :: (MonadLog m, MonadIO m, MonadError String m) => Day -> Solution m
solution day =
  fromJust $
    lookup day $
      zip [mkDay_ 1 .. mkDay_ 12] [day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12]
