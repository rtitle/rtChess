{- |
Main.hs
Main program, contains a game player.
-}
module Main(main) where
import System.Exit
import System.IO
import Data.List.Split
import Control.Monad.RWS
import Board
import Moves
import Game

-- |Returns a printable help String.
help :: String
help = 
  "Enter one move, a list of moves separated by spaces, or one of the following commands:\n" ++
  "moves : prints available moves\n" ++
  "new : starts a new game\n" ++
  "board : displays the board\n" ++
  "game : displays the game so far\n" ++
  "exit : exits the program"

-- |Given a game, returns printable String containing the legal moves.
gameMoves :: Game -> String
gameMoves g@(Game ps b t _ ep) = 
  "There are " ++ 
  (show . length $ mvs) ++ 
  " possible moves for " ++ 
  show t ++ 
  ":\n" ++ 
  (show . map (showMove b ps t) $ mvs)
  where mvs = legalMoves b ps t ep (getProhibitedCastles g)
  
-- |Returns a printable String of the game log (current value of the Writer monad in Game.hs).
showGameLog :: [String] -> String
showGameLog lg = foldr showLine "" $ zip ([1..]::[Integer]) $ chunksOf 2 lg where
  showLine ln acc = (show . fst $ ln) ++ ". " ++ foldr (\x ac -> x ++ "\t" ++ ac) "" (snd ln) ++ "\n" ++ acc

-- |Returns a printable String of the game board.
showBoard :: Game -> String
showBoard g = prettyPieces . pieces $ g

-- |Game player.  Displays a prompt in a loop and performs the appropriate action.
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
  --let input = splitOn " " l
  let input = l
  let (res', g'@(Game ps b t _ _), lg') = runRWS (playMove input) () g
  case res' of
    Right m -> putStrLn $ "Made move: " ++ l ++ "\nComputer made move: " ++ (showMove b ps t m)
    Left s -> putStrLn s
  run g' (lg++lg')

-- |Prints the help string and starts a new game.
main :: IO ()
main = do
  putStrLn help
  run newGame []
