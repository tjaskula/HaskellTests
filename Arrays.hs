import System.IO
import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    input_line <- getLine
    let l = read input_line :: Int
    input_line <- getLine
    let h = read input_line :: Int
    t <- getLine
    
    let ascii_start = 65
    let ascii_end = 90
    
    rows <- mapM (\i -> do
                    row <- getLine
                    return (row)) $ [0..(h - 1)]
    
    let word = map (\c -> do
                    let n = ord (toUpper c)
                    let offset = if (n < ascii_start) || (n > ascii_end)
                                    then ascii_end - ascii_start + 1
                                    else n - ascii_start
                    let start_letter = l * offset
                    let subWord = map (\i -> do
                                        let line = rows!!i
                                        let sub = take l . drop start_letter $ line
                                        sub) $ [0..(h - 1)]
                    subWord) $ t
        
    let w = map (\i -> do                    
                    let line = map (\j -> do
                                        word!!j!!i) $ [0..((length t) -1)]
                    intercalate "" line
                    ) $ [0..(h - 1)]
    
    putStrLn (intercalate "\n" w)
    
    return ()