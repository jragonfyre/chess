--
-- Notation.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Chess.Notation where
  --(
  --) where

import Chess.Types

import Data.Char (ord, chr)

readBoardPos :: String -> Maybe BoardPos
readBoardPos (x:y:[]) = 
  let
    bp = (ord x - ord 'a', ord y - ord '1')
  in
    if onBoard bp
    then
      Just bp
    else
      Nothing
readBoardPos _ = Nothing

fileStr :: BoardPos -> String
fileStr bp = [file bp]

toFileStr :: Int -> String
toFileStr x = [toFile x]

rankStr :: BoardPos -> String
rankStr bp = show $ rank bp

toRankStr :: Int -> String
toRankStr y = show $ toRank y

posStr :: BoardPos -> String
posStr bp = file bp : show (rank bp)

checkStr :: CheckType -> String
checkStr NoCheck = ""
checkStr MvCheck = "+"
checkStr MvMate = "#"
checkStr MvDraw = " draw"
checkStr MvStalemate = " stalemate"

notateMove :: Move -> [BoardPos] -> String
notateMove (StdMove turn mt ct) disambigs = (notateMoveT mt disambigs) ++ (checkStr ct)
notateMove (AnnounceResignation (_,White)) disambigs = "0-1"
notateMove (AnnounceResignation (_,Black)) disambigs = "1-0"

notateMoveT :: MoveType -> [BoardPos] -> String
notateMoveT CastleKingside _ = "O-O"
notateMoveT CastleQueenside _ = "O-O-O"
notateMoveT (Move pt st ed) disambigs 
  =  (pieceTStr pt)
  ++ (disambigStr st disambigs)
  ++ (posStr ed)
notateMoveT (PromoteMove st ed pt) disambigs 
  = notateMoveT (Move Pawn st ed) [] ++ promStr
  where 
    promStr = '=' : pieceTStr pt
notateMoveT (EnPassant st ed tk) _ = notateMoveT (Take Pawn st ed) []
notateMoveT (PromoteTake st ed pt) _
  = notateMoveT (Take Pawn st ed) [] ++ promStr
  where 
    promStr = '=' : pieceTStr pt
notateMoveT (Take Pawn st ed) _
  =  (fileStr st)
  -- ++ (disambigStr st disambigs)
  ++ "x"
  ++ (posStr ed)
notateMoveT (Take pt st ed) disambigs 
  =  (pieceTStr pt)
  ++ (disambigStr st disambigs)
  ++ "x"
  ++ (posStr ed)

disambigStr :: BoardPos -> [BoardPos] -> String
disambigStr st [] = "" -- no disambiguation required
disambigStr st disambigs =  -- we *must* disambiguate
  let
    sharedXs = any (\bp -> xCoord bp == xCoord st) disambigs
    sharedYs = any (\bp -> yCoord bp == yCoord st) disambigs
  in
    if sharedXs
    then
      if sharedYs
      then
        posStr st
      else 
        show $ rank st
    else
      [file st]

pieceTLetter :: PieceType -> Char
pieceTLetter King = 'K'
pieceTLetter Queen = 'Q'
pieceTLetter Bishop = 'B'
pieceTLetter Knight = 'N'
pieceTLetter Rook = 'R'
pieceTLetter Pawn = 'P'

pieceTStr :: PieceType -> String
pieceTStr Pawn = ""
pieceTStr pc = [pieceTLetter pc]






