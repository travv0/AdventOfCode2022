module Main where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = R | L | U | D deriving (Show, Read, Eq)

data Rope = Rope (Int, Int) (Int, Int) deriving (Show)

touching :: (Int, Int) -> (Int, Int) -> Bool
touching (x1, y1) (x2, y2) = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

step :: Direction -> Rope -> Rope
step U (Rope (hx, hy) tl) = Rope newHead $ stepTail (Rope newHead tl)
  where
    newHead = (hx, hy - 1)
step D (Rope (hx, hy) tl) = Rope newHead $ stepTail (Rope newHead tl)
  where
    newHead = (hx, hy + 1)
step L (Rope (hx, hy) tl) = Rope newHead $ stepTail (Rope newHead tl)
  where
    newHead = (hx - 1, hy)
step R (Rope (hx, hy) tl) = Rope newHead $ stepTail (Rope newHead tl)
  where
    newHead = (hx + 1, hy)

stepTail :: Rope -> (Int, Int)
stepTail (Rope h@(hx, hy) t@(tx, ty))
  | touching h t = t
  | otherwise =
      ( if hx > tx then tx + 1 else if hx < tx then tx - 1 else hx,
        if hy > ty then ty + 1 else if hy < ty then ty - 1 else hy
      )

stepTimes :: Int -> Direction -> Set (Int, Int) -> Rope -> (Rope, Set (Int, Int))
stepTimes 0 _ ts rope = (rope, ts)
stepTimes n dir ts rope =
  let newRope@(Rope _ t) = step dir rope
   in stepTimes (n - 1) dir (Set.insert t ts) newRope

parse :: String -> [(Direction, Int)]
parse = map (\l -> (read $ take 1 l, read $ drop 2 l)) . lines

main :: IO ()
main = do
  motions <- parse <$> readFile "input.txt"
  let (_, ts) =
        foldl'
          (\(rope, t) (dir, n) -> stepTimes n dir t rope)
          (Rope (0, 0) (0, 0), Set.empty)
          motions
  print $ Set.size ts