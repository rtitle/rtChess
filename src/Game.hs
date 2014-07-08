module Game where
import qualified Data.Vector as V
import Control.Monad.State
import Board
import Moves

data Game = Game { pieces :: Pieces,
                   board :: Board,
                   turn :: PieceColor }
                             
makeMove :: Move -> State Game ()
makeMove move = do
               (Game ps b t) <- get
               put $ Game (updatePieces ps move) (updateBoard b move) (reverseColor t)
               
maybeMakeMove :: Maybe Move -> State Game ()
maybeMakeMove (Just m) = makeMove m
maybeMakeMove Nothing = return ()
    
showMove :: Game -> Move -> String
showMove (Game ps b t) m@(Move from to cap)
  -- pawn capture (exd4)
  | pieceType from == Pawn && cap /= Nothing = (head fromPieceSquare) : "x" ++ toPieceSquare
    
  -- pawn move (d6)
  | pieceType from == Pawn = showPieceSquare to
     
  -- piece capture (Qxh2)
  | cap /= Nothing = show (pieceType to) ++ rankOrFile ++ "x" ++ toPieceSquare
    
  -- piece move (Nf3)
  | otherwise = show (pieceType to) ++ rankOrFile ++ toPieceSquare
  where 
    fromPieceSquare = showPieceSquare from
    toPieceSquare = showPieceSquare to
    disambiguate [] = ""
    disambiguate (x:[]) = take 1 $ filter (`notElem` (showPieceSquare x)) fromPieceSquare
    disambiguate _ = fromPieceSquare
    ambiguous m' = (pieceType . fromPiece $ m) == (pieceType . fromPiece $ m') &&
                   (pieceColor . fromPiece $ m) == (pieceColor . fromPiece $ m') &&
                   (square . toPiece $ m) == (square . toPiece $ m') &&
                   (square . fromPiece $ m) /= (square $ fromPiece $ m')
    rankOrFile = disambiguate $ map fromPiece (filter ambiguous (allMoves b ps t))
               
readMove :: Game -> String -> Maybe Move
readMove (Game ps b t) s = 
  let (toSq, pt, fromFileOrRank) = case s of 
       r:f:[] -> (readSquare (r:[f]), Just Pawn, Nothing)
       p:r:f:[] -> (readSquare (r:[f]), readPieceType p, Nothing)
       p:'x':r:f:[] -> case readPieceType p of
                         Just ptype -> (readSquare (r:[f]), Just ptype, Nothing)
                         Nothing -> (readSquare (r:[f]), Just Pawn, Just p)
       p:rf:r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf)
       p:rf:'x':r:f:[] -> (readSquare (r:[f]), readPieceType p, Just rf)
       _ -> (readSquare "a1", Nothing, Nothing)
      fm m = toSq == (square . toPiece $ m) &&
             pt == Just (pieceType . toPiece $ m) &&
             (fromFileOrRank == Nothing || any (\x -> (Just x) == fromFileOrRank) (showSquare . square . fromPiece $ m))
  in headMaybe $ filter fm (allMoves b ps t)
  where headMaybe (x:[]) = Just x
        headMaybe _ = Nothing

updatePieces :: Pieces -> Move -> Pieces
updatePieces ps (Move p1 p2 Nothing) = p2 : filter (/=p1) ps
updatePieces ps (Move p1 p2 (Just c)) = p2 : filter (liftM2 (&&) (/=p1) (/=c)) ps

updateBoard :: Board -> Move -> Board
updateBoard b (Move p1 p2 Nothing) = b V.// [(square p1, Nothing), (square p2, Just p2)]
updateBoard b (Move p1 p2 (Just c)) = b V.// [(square p1, Nothing), (square p2, Just p2), (square c, Just p2)]

newGame :: Game
newGame = Game initialPieces (toBoard initialPieces) White

playGame :: [String] -> State Game [Move]
playGame [] = do
              (Game ps b t) <- get
              return $ allMoves b ps t
playGame (x:xs) = do
                  g <- get
                  let maybeM = readMove g x
                  maybeMakeMove maybeM
                  playGame $ case maybeM of 
                    Just _ -> xs
                    Nothing -> []
