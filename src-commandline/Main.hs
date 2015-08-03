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

-- |Given a position, returns printable String containing the legal moves.
positionMoves :: Position -> String
positionMoves p@(Position ps b t _ _ ep) = 
  "There are " ++ 
  (show . length $ mvs) ++ 
  " possible moves for " ++ 
  show t ++ 
  ":\n" ++ 
  (show . map (showMove b ps t) $ mvs)
  where mvs = legalMoves b ps t ep (getProhibitedCastles p)
  
-- |Returns a printable String of the game log (current value of the Writer monad in Game.hs).
showGameLog :: [String] -> String
showGameLog lg = foldr showLine "" $ zip ([1..]::[Integer]) $ chunksOf 2 lg where
  showLine ln acc = (show . fst $ ln) ++ ". " ++ foldr (\x ac -> x ++ "\t" ++ ac) "" (snd ln) ++ "\n" ++ acc

-- |Returns a printable String of the position.
showBoard :: Position -> String
showBoard p = prettyPieces . pieces $ p

-- |Game player.  Displays a prompt in a loop and performs the appropriate action.
run :: Position -> [String] -> IO ()
run p lg = do
  putStr "> "
  hFlush stdout
  l <- getLine
  when (l == "") $ do
    run p lg
  when (l == "help") $ do  
    putStrLn help
    run p lg
  when (l == "moves") $ do
    putStrLn $ positionMoves p
    run p lg 
  when (l == "new") $ do
    putStrLn "Starting new game."
    run initialPosition []
  when (l == "board") $ do
    putStrLn $ showBoard p
    run p lg
  when (l == "game") $ do
    putStrLn "Game so far:"
    putStrLn $ showGameLog lg
    run p lg
  when (l == "exit") $ do
    putStrLn "Thanks for playing!"
    exitSuccess
  --let input = splitOn " " l
  let input = l
  let (res', p'@(Position ps b t _ _ _), lg') = runRWS (playMove input) () p
  case res' of
    Right m -> putStrLn $ "Made move: " ++ l ++ "\nComputer made move: " ++ (showMove b ps t m)
    Left s -> putStrLn s
  run p' (lg++lg')

-- |Prints the help string and starts a new game.
main :: IO ()
main = do
  putStrLn help
  run initialPosition []
