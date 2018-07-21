--
-- Types.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Chess.Types where
  --(
  --) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
--import Control.Monad.State.Lazy
import Control.Monad.RWS.Lazy

import Data.Char (ord, chr)
import Data.Maybe (mapMaybe, maybe)

data Color 
  = White
  | Black
  deriving (Read, Show, Eq, Ord, Enum)

data PieceType
  = King 
  | Queen 
  | Bishop
  | Knight
  | Rook
  | Pawn
  deriving (Read, Show, Eq, Ord, Enum)

data Piece = Piece PieceType Color
  deriving (Read, Show, Eq, Ord)--, Enum)

type SimpleMove = (BoardPos,BoardPos, Maybe PieceType)

type Turn = (Int, Color)

instance Enum (Int, Color) where
  fromEnum (n,White) = 2*n
  fromEnum (n,Black) = 2*n + 1
  toEnum n = (n `div` 2, if n `mod` 2 == 0 then White else Black)

data MoveType
  = Move PieceType BoardPos BoardPos
  | Take PieceType BoardPos BoardPos
  | PromoteMove BoardPos BoardPos PieceType
  | PromoteTake BoardPos BoardPos PieceType
  | EnPassant BoardPos BoardPos BoardPos -- start, end, taken piece
  | CastleKingside
  | CastleQueenside
  deriving (Read, Show, Eq, Ord)--, Enum)

-- ending game state after normal move
data CheckType = NoCheck | MvCheck | MvMate | MvDraw | MvStalemate
  deriving (Read, Show, Eq, Ord, Enum)

data Move 
  = StdMove Turn MoveType CheckType
  | AnnounceResignation Turn
  deriving (Read, Show, Eq, Ord)--, Enum)

type BoardPos = (Int, Int)

type Board = Map BoardPos (Piece,Int)

type Chess = RWST () [Move] GameState IO

type Player = Chess Move

data GameStatus
  = Checkmate
  | Resignation
  | Stalemate
  | DrawPawn
  | DrawRepeat
  | DrawAgreement
  | Check
  | NoStatus
  deriving (Read, Show, Eq, Ord, Enum)

data GameEndStatus
  = WhiteWin
  | BlackWin
  | Draw
  deriving (Read, Show, Eq, Ord, Enum)

data GameState = GameState 
  { nextTurn :: Turn
  , status :: GameStatus
  , board :: Board
  }
  deriving (Read, Show, Eq, Ord)

checkTypeToStatus :: CheckType -> GameStatus
checkTypeToStatus NoCheck = NoStatus
checkTypeToStatus MvCheck = Check
checkTypeToStatus MvMate = Checkmate
checkTypeToStatus MvDraw = DrawAgreement
checkTypeToStatus MvStalemate = Stalemate

turnColor :: Turn -> Color
turnColor (_,c) = c

turnNumber :: Turn -> Int
turnNumber (n,_) = n

gameEndStatus :: GameState -> Maybe GameEndStatus
gameEndStatus gs = checkGameEndStatus (turnColor $ nextTurn gs) (status gs)

gameOver :: GameState -> Bool
gameOver = maybe False (const True) . gameEndStatus

draw :: GameEndStatus
draw = Draw

winFor :: Color -> GameEndStatus
winFor White = WhiteWin
winFor Black = BlackWin

lossFor :: Color -> GameEndStatus
lossFor = winFor . flipColor

-- color is the next turn's color
checkGameEndStatus :: Color -> GameStatus -> Maybe GameEndStatus
checkGameEndStatus ntc Checkmate = Just $ lossFor ntc
checkGameEndStatus ntc Resignation = Just $ winFor ntc
checkGameEndStatus _ Stalemate = Just draw
checkGameEndStatus _ DrawPawn = Just draw
checkGameEndStatus _ DrawRepeat = Just draw
checkGameEndStatus _ DrawAgreement = Just draw
checkGameEndStatus _ _ = Nothing

moveTurn :: Move -> Turn
moveTurn (StdMove t _ _) = t
moveTurn (AnnounceResignation t) = t

gameEndingMove :: Move -> Bool
gameEndingMove (AnnounceResignation _) = True
gameEndingMove (StdMove _ _ MvMate) = True
gameEndingMove (StdMove _ _ MvStalemate) = True
gameEndingMove (StdMove _ _ MvDraw) = True
gameEndingMove _ = False

flipColor :: Color -> Color
flipColor Black = White
flipColor White = Black

makePiece :: PieceType -> Color -> Piece
makePiece = Piece

color :: Piece -> Color
color (Piece _ c) = c

pieceType :: Piece -> PieceType
pieceType (Piece t _) = t

endingSquare :: Turn -> MoveType -> BoardPos
endingSquare _ (Move _ _ ed) = ed
endingSquare _ (Take _ _ ed) = ed
endingSquare _ (PromoteMove _ ed _) = ed
endingSquare _ (PromoteTake _ ed _) = ed
endingSquare _ (EnPassant _ ed _) = ed
endingSquare (_,c) CastleKingside = (6, if c==White then 0 else 7)
endingSquare (_,c) CastleQueenside = (2, if c==White then 0 else 7)

makeBoardPos :: Int -> Int -> BoardPos
makeBoardPos = (,)

file :: BoardPos -> Char
file (x,_) = toFile x

toFile :: Int -> Char
toFile x = chr (x + ord 'a')


rank :: BoardPos -> Int
rank (_,y) = toRank y

toRank :: Int -> Int
toRank y = y+1

xCoord :: BoardPos -> Int
xCoord (x,_) = x

yCoord :: BoardPos -> Int
yCoord (_,y) = y

lookAt :: Board -> BoardPos -> Maybe Piece
lookAt brd bp = fmap fst $ Map.lookup bp brd

lookAtLastMove :: Board -> BoardPos -> Maybe (Piece,Int)
lookAtLastMove = flip Map.lookup

lookAtLastTurn :: Board -> BoardPos -> Maybe (Piece, Turn)
lookAtLastTurn brd bp = do
  (p@(Piece _ c), n) <- lookAtLastMove brd bp
  return (p,(n,c))

findPiecesTC :: PieceType -> Color -> Board -> [BoardPos]
findPiecesTC pc c = findPieces (Piece pc c)

findPieces :: Piece -> Board -> [BoardPos]
findPieces p brd = concatMap (\(pos,(pc,_)) -> if pc==p then [pos] else []) $ Map.toList brd

distance :: BoardPos -> BoardPos -> Int
distance (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

onBoard :: BoardPos -> Bool
onBoard (a,b) = a < 8 && b < 8 && a >= 0 && b >= 0

pieceLocations :: Color -> Board -> [(BoardPos,Piece)]
pieceLocations c = mapMaybe (\(bp,(p@(Piece _ c2),_)) -> if c==c2 then Just (bp,p) else Nothing) . Map.toList

pieces :: Color -> Board -> [Piece]
pieces c brd = map (\(_,p) -> p) $ pieceLocations c brd

pieceTypeOfMoveType :: MoveType -> PieceType
pieceTypeOfMoveType (Move pt _ _) = pt
pieceTypeOfMoveType (Take pt _ _) = pt
pieceTypeOfMoveType (PromoteMove _ _ _) = Pawn
pieceTypeOfMoveType (PromoteTake _ _ _) = Pawn
pieceTypeOfMoveType (EnPassant _ _ _) = Pawn
pieceTypeOfMoveType (CastleKingside) = King
pieceTypeOfMoveType (CastleQueenside) = King



