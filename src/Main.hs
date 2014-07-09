module Main where
import Data.List.Split
import System.Exit
import Control.Monad.State
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
  
run :: Game -> [String] -> IO ()
run g@(Game ps b t) lg = do
  l <- getLine
  when (l == "help") $ do  
    putStrLn help
    run g lg
  when (l == "moves") $ do
    let mvs = allMoves b ps t
    putStrLn $ "There are " ++ show (length mvs) ++ " possible moves for " ++ show t ++ ":" 
    putStrLn $ show . map (showMove b ps t) $ mvs
    run g lg 
  when (l == "new") $ do
    putStrLn "Starting new game."
    run newGame []
  when (l == "board") $ do
    putStrLn $ prettyPieces . pieces $ g
    run g lg
  when (l == "game") $ do
    putStrLn "Game so far:"
    putStrLn $ show lg
    run g lg
  when (l == "exit") $ do
    putStrLn "Thanks for playing!"
    exitSuccess
  let input = splitOn " " l
  let (res', g', lg') = runRWS (playGame input True) () g
  case res' of
    True -> putStrLn $ "Made move(s): " ++ l
    False -> putStrLn $ "Invalid move(s): " ++ l
  run g' (lg++lg')

main :: IO ()
main = do
  putStrLn help
  run newGame []