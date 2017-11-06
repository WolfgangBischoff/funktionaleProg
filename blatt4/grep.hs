
import System.Environment
import Data.Char
import Data.List

word = "abc"

merge (a,b) = show a ++ show b --like toString

main = do 
 a <- getArgs --bind arguments
 let lines = zip [1..] a --build tupels with line number
--return strings, of fitting tuples
 let b = [merge x|x<-lines, word `isInfixOf` (snd x)] 
-- implementation without line numbers
-- let b = [x|x<-a, word `isInfixOf` x]
 mapM_ putStrLn b --mapM_ is something with a monad

 
