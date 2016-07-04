import System.IO
import Control.Monad
import Data.List.Split

main :: IO ()
main = do

    let n = 5
    let temps = "1 -4 5 2 -3"

    if n == 0 
        then putStrLn "0"
    else do
        let splitted = splitOn " " temps
        let numbers = map (read::String->Int) splitted
        let positives = filter (>0) numbers
        let negatives = filter (<0) numbers
        
        let closerToZero a [] = min a
            closerToZero [] b = max b
            closerToZero a b = if min a <= max(abs b) then min a else max b
        
        let result = closerToZero positives negatives
        putStrLn result
    return ()