import System.IO
import Control.Monad
import Data.List
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
        
        let closerToZero a [] = minimum a
            closerToZero [] b = maximum b
            closerToZero a b = if minimum a <= abs(maximum b) then minimum a else maximum b
        
        let result = closerToZero positives negatives
        putStrLn (show result)
        
    return ()