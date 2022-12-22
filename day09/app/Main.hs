module Main where

import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = R | L | U | D deriving (Show, Read, Eq)

type Rope = [(Int, Int)]

touching :: (Int, Int) -> (Int, Int) -> Bool
touching (x1, y1) (x2, y2) = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

stepHead :: Direction -> (Int, Int) -> (Int, Int)
stepHead dir (hx, hy) = case dir of
  U -> (hx, hy - 1)
  D -> (hx, hy + 1)
  L -> (hx - 1, hy)
  R -> (hx + 1, hy)

step :: Direction -> Rope -> Rope
step _ [] = error "out of rope"
step dir [h] = [stepHead dir h]
step dir (h : t) = newHead : stepTail newHead t
  where
    newHead = stepHead dir h

stepTail :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
stepTail _ [] = []
stepTail h@(hx, hy) tl@(t@(tx, ty) : ts)
  | touching h t = tl
  | otherwise =
      newHead
        : stepTail newHead ts
  where
    newHead =
      ( if hx > tx then tx + 1 else if hx < tx then tx - 1 else hx,
        if hy > ty then ty + 1 else if hy < ty then ty - 1 else hy
      )

stepTimes :: Int -> Direction -> Set (Int, Int) -> Rope -> (Rope, Set (Int, Int))
stepTimes 0 _ ts rope = (rope, ts)
stepTimes n dir ts rope =
  let newRope = step dir rope
      t = last newRope
   in stepTimes (n - 1) dir (Set.insert t ts) newRope

parse :: String -> [(Direction, Int)]
parse = map (\l -> (read $ take 1 l, read $ drop 2 l)) . lines

doMotions :: Foldable t => Int -> t (Direction, Int) -> (Rope, Set (Int, Int))
doMotions len =
  foldl'
    (\(rope, t) (dir, n) -> stepTimes n dir t rope)
    (replicate len (0, 0), Set.empty)

main :: IO ()
main = do
  motions <- parse <$> readFile "input.txt"
  let (_, ts2) = doMotions 2 motions
  print $ Set.size ts2
  let (_, ts10) = doMotions 10 motions
  print $ Set.size ts10
