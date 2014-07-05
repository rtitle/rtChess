module Game where
import Control.Monad.State
import Board
import Moves
import qualified Data.Vector as V
import System.Random

data Game = Game { pieces :: Pieces,
                   board :: Board,
                   turn :: PieceColor }
                             
makeMove :: Move -> State Game ()
makeMove move = do
               (Game ps b t) <- get
               put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t)

updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 p2 _) = p2 : filter (/=p1) ps

updateBoard :: Board -> Move -> Board
updateBoard b (Move Piece {square = fromSq} p@Piece {square = toSq} _) = b V.// [(fromSq, Nothing), (toSq, Just p)]

newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White

randomMove :: StdGen -> [Move] -> (StdGen, Move)
randomMove generator m = let (value, newGenerator) = randomR (0, length m - 1) generator
                         in (newGenerator, m !! value)

playRandomGame :: StdGen -> Int -> State Game Pieces
playRandomGame _ 0 = do
                     (Game ps _ _) <- get
                     return ps
playRandomGame gen n = do
                       (Game ps b t) <- get
                       let (newGen, m) = randomMove gen $ allMoves b ps t
                       makeMove m
                       playRandomGame newGen (n-1)
                       
playGame :: Int -> State Game Pieces
playGame n = playRandomGame (mkStdGen n) n
