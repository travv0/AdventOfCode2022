module Main where

main :: IO ()
main = do
    input <- readFile "input.txt"
    print $ findMarker 4 input
    print $ findMarker 14 input

findMarker :: Int -> String -> Int
findMarker len = findStartOfPacketMarker' len len
  where
    findStartOfPacketMarker' pos len buffer
        | anyDupes (take len buffer) = findStartOfPacketMarker'
            (succ pos)
            len
            (tail buffer)
        | otherwise = pos

anyDupes :: Eq a => [a] -> Bool
anyDupes (c : cs) = c `elem` cs || anyDupes cs
anyDupes _        = False
