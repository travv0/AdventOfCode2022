{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO

main :: IO ()
main = do
    input <- TIO.readFile "input.txt"
    print $ getWrongScore $ parseWrong input
    print $ getRightScore $ parseRight input

data Shape = Rock | Paper | Scissors deriving (Enum, Show)
data Outcome = Draw | Win | Loss deriving (Enum, Show)

opponentToPlay 'A' = Rock
opponentToPlay 'B' = Paper
opponentToPlay 'C' = Scissors
opponentToPlay c   = error $ "Invalid input to opponentToPlay '" <> [c] <> "'"

youToPlay 'X' = Rock
youToPlay 'Y' = Paper
youToPlay 'Z' = Scissors
youToPlay c   = error $ "Invalid input to youToPlay '" <> [c] <> "'"

getOutcome :: Shape -> Shape -> Outcome
getOutcome yourShape opponentShape =
    toEnum $ (fromEnum yourShape - fromEnum opponentShape) `mod` 3

score shape outcome = shapeScore shape + outcomeScore outcome
  where
    shapeScore Rock     = 1
    shapeScore Paper    = 2
    shapeScore Scissors = 3

    outcomeScore Loss = 0
    outcomeScore Draw = 3
    outcomeScore Win  = 6

parseWrong = fmap (parseMatch . T.unpack) . T.lines
    where parseMatch [opp, _, you] = (youToPlay you, opponentToPlay opp)

getWrongScore :: [(Shape, Shape)] -> Int
getWrongScore = sum . fmap (\(you, opp) -> score you (getOutcome you opp))

requiredOutcome 'X' = Loss
requiredOutcome 'Y' = Draw
requiredOutcome 'Z' = Win
requiredOutcome c   = error $ "Invalid input to requiredOutcome '" <> [c] <> "'"

shapeForOutcome :: Shape -> Outcome -> Shape
shapeForOutcome opponentShape outcome =
    toEnum $ (fromEnum outcome + fromEnum opponentShape) `mod` 3

parseRight = fmap (parseMatch . T.unpack) . T.lines
  where
    parseMatch [opp, _, outcome] =
        (opponentToPlay opp, requiredOutcome outcome)

getRightScore :: [(Shape, Outcome)] -> Int
getRightScore =
    sum . fmap (\(opp, outcome) -> score (shapeForOutcome opp outcome) outcome)
