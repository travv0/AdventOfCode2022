{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Foldable                  ( Foldable(toList) )
import           Data.List                      ( sort
                                                , sortBy
                                                , sortOn
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Text.Read                      ( readMaybe )

parse :: Text -> [[Int]]
parse = fmap (mapMaybe readMaybe . lines . T.unpack) . T.splitOn "\n\n"

topElf :: [[Int]] -> Int
topElf cals = maximum $ fmap sum cals

topThreeElves :: [[Int]] -> [Int]
topThreeElves cals = take 3 $ reverse $ sort $ fmap sum cals

main :: IO ()
main = do
    input <- TIO.readFile "input.txt"
    let cals = parse input
    print $ topElf cals
    print $ sum $ topThreeElves cals
