module Main where
import System.Exit
import System.IO
import Data.List.Split
import Control.Monad.RWS
import Board
import Moves
import Game

help :: String
help = 
  "Enter one move, a list of moves separated by spaces, or one of the following commands:\n" ++
  "moves : prints available moves\n" ++
  "new : starts a new game\n" ++
  "board : displays the board\n" ++
  "game : displays the game so far\n" ++
  "exit : exits the program"

gameMoves :: Game -> String
gameMoves (Game ps b t prohibCas ep) = 
  "There are " ++ 
  (show . length $ mvs) ++ 
  " possible moves for " ++ 
  show t ++ 
  ":\n" ++ 
  (show . map (showMove b ps t) $ mvs)
  where mvs = legalMoves b ps t ep (getProhibitedCastles prohibCas t)
  
showGameLog :: [String] -> String
showGameLog lg = foldr showLine "" $ zip ([1..]::[Integer]) $ chunksOf 2 lg where
  showLine ln acc = (show . fst $ ln) ++ ". " ++ foldr (\x ac -> x ++ "\t" ++ ac) "" (snd ln) ++ "\n" ++ acc
  
showBoard :: Game -> String
showBoard g = prettyPieces . pieces $ g
  
run :: Game -> [String] -> IO ()
run g lg = do
  putStr "> "
  hFlush stdout
  l <- getLine
  when (l == "") $ do
    run g lg
  when (l == "help") $ do  
    putStrLn help
    run g lg
  when (l == "moves") $ do
    putStrLn $ gameMoves g
    run g lg 
  when (l == "new") $ do
    putStrLn "Starting new game."
    run newGame []
  when (l == "board") $ do
    putStrLn $ showBoard g
    run g lg
  when (l == "game") $ do
    putStrLn "Game so far:"
    putStrLn $ showGameLog lg
    run g lg
  when (l == "exit") $ do
    putStrLn "Thanks for playing!"
    exitSuccess
  let input = splitOn " " l
  let (res', g', lg') = runRWS (playGame input) () g
  case res' of
    Right _ -> putStrLn $ "Made move(s): " ++ l
    Left s -> putStrLn s
  run g' (lg++lg')

main :: IO ()
main = do
  putStrLn help
  run newGame []