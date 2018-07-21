module Chess where
    --( someFunc
    --) where

import Chess.Types

import Utils

import Data.Char (ord, chr)
import Data.List (nub, sort)

import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map


startingBoard :: Board
startingBoard = Map.map (,0) $ Map.fromList 
  [ ((0,0), Piece Rook White)
  , ((1,0), Piece Knight White)
  , ((2,0), Piece Bishop White)
  , ((3,0), Piece Queen White)
  , ((4,0), Piece King White)
  , ((5,0), Piece Bishop White)
  , ((6,0), Piece Knight White)
  , ((7,0), Piece Rook White)
  , ((0,7), Piece Rook Black)
  , ((1,7), Piece Knight Black)
  , ((2,7), Piece Bishop Black)
  , ((3,7), Piece Queen Black)
  , ((4,7), Piece King Black)
  , ((5,7), Piece Bishop Black)
  , ((6,7), Piece Knight Black)
  , ((7,7), Piece Rook Black)
  , ((0,1), Piece Pawn White)
  , ((1,1), Piece Pawn White)
  , ((2,1), Piece Pawn White)
  , ((3,1), Piece Pawn White)
  , ((4,1), Piece Pawn White)
  , ((5,1), Piece Pawn White)
  , ((6,1), Piece Pawn White)
  , ((7,1), Piece Pawn White)
  , ((0,6), Piece Pawn Black)
  , ((1,6), Piece Pawn Black)
  , ((2,6), Piece Pawn Black)
  , ((3,6), Piece Pawn Black)
  , ((4,6), Piece Pawn Black)
  , ((5,6), Piece Pawn Black)
  , ((6,6), Piece Pawn Black)
  , ((7,6), Piece Pawn Black)
  ]


rankPoss :: BoardPos -> [BoardPos]
rankPoss (x,y) = [ (a,y) | a <- [0..7], a /= x]

filePoss :: BoardPos -> [BoardPos]
filePoss (x,y) = [ (x,a) | a <- [0..7], a /= y]

diagonalPoss :: BoardPos -> [BoardPos]
diagonalPoss (x,y) = filter (/= (x,y)) . filter onBoard $ 
  [ (a,a-(x-y)) 
  | a <- [0 .. 7]
  ] ++
  [ (a,x+y-a)
  | a <- [0 .. 7]
  ]

knightPoss :: BoardPos -> [BoardPos]
knightPoss (x,y) = filter onBoard $ 
  [ (x+2,y+1)
  , (x-2,y+1)
  , (x+2,y-1)
  , (x-2,y-1)
  , (x+1,y+2)
  , (x-1,y+2)
  , (x+1,y-2)
  , (x-1,y-2)
  ]

kingPoss :: Color -> BoardPos -> [BoardPos]
kingPoss cl (x,y) =
  let
    strk = if cl == White then 0 else 7
  in 
    filter (/= (x,y)) . filter onBoard $ 
      [ (x+c,y+d)
      | c <- [-1..1]
      , d <- [-1..1]
      ] ++ 
      [ (2,strk)
      , (6,strk)
      ]

pawnPoss :: Color -> BoardPos -> [BoardPos]
pawnPoss c (x,y) = 
  let
    inc = case c of 
      White ->
        1
      Black -> 
        -1
  in
    filter onBoard $
      [ (x+c,y+inc)
      | c <- [-1..1]
      ] ++ 
      ( if (2*y) == 7-5*inc
        then
          [ (x,y+2*inc)
          ]
        else
          []
      )

possibleMovesForPieceAt :: Piece -> BoardPos -> [BoardPos]
possibleMovesForPieceAt (Piece Queen _) bp = rankPoss bp ++ filePoss bp ++ diagonalPoss bp
possibleMovesForPieceAt (Piece Rook _) bp = rankPoss bp ++ filePoss bp
possibleMovesForPieceAt (Piece Bishop _) bp = diagonalPoss bp
possibleMovesForPieceAt (Piece Knight _) bp = knightPoss bp
possibleMovesForPieceAt (Piece King c) bp = kingPoss c bp
possibleMovesForPieceAt (Piece Pawn c) bp = pawnPoss c bp

dontTakeSelf :: Color -> BoardPos -> Board -> Bool
dontTakeSelf c ed brd = 
  case lookAt brd ed of 
    Nothing ->
      True
    Just (Piece _ c2) ->
      c /= c2

-- color is the defending color, not the attacking color
-- castling can never attack, so disable castling
underAttack :: GameState -> BoardPos -> Bool
underAttack gs bp = 
  let 
    t = nextTurn gs
    brd = board gs
    nt@(_,opc) = succ t
  in --bp `elem` validMoves (succ t) brd
    any
      ( \(st,p) -> 
          case p of 
            Piece King _ ->
              if distance st bp <= 1
              then
                True
              else
                False
            Piece Pawn _ ->
              let
                (x,y)=st
                (x2,y2)=bp
              in
                (y2 == y + pawnDirection opc)
                &&
                (abs (x-x2) == 1)
            _ ->
              isValidMove False gs{nextTurn=nt} p (st,bp,Nothing)
              -- this doesn't skip castling or require pawns to move by taking
      )
      $ pieceLocations opc brd

isKingInCheck :: GameState -> Bool
isKingInCheck gs@GameState{nextTurn=t@(_,c), board=brd} = case findPieces (Piece King c) brd of
  -- [] -> 
  --   False -- This should be an error, maybe comment out this whole case
  [bp] ->
    underAttack gs bp


-- works only for squares in same rank, file, or diagonal
squaresBetween :: BoardPos -> BoardPos -> [BoardPos]
squaresBetween (x1,y1) (x2,y2)
  | x1==x2 = [(x1,y) | y <- rangeExclusive y1 y2 ]
  | y1==y2 = [(x,y1) | x <- rangeExclusive x1 x2 ]
  | otherwise = zip (rangeExclusive x1 x2) $ 
      let
        yran = (rangeExclusive y1 y2)
      in
        if (y1-y2) == (x1-x2)
        then
          yran
        else
          reverse yran

unblockedBetween :: BoardPos -> BoardPos -> Board -> Bool
unblockedBetween st ed brd = all (\sq -> lookAt brd sq == Nothing) (squaresBetween st ed)

-- no validation, just makes the move.
makeSimpleMove :: Int -> Piece -> SimpleMove -> Board -> Board
makeSimpleMove n (Piece _ c) (st,ed,Just nt) = Map.insert ed (Piece nt c, n) . Map.delete st
makeSimpleMove n p (st,ed,Nothing) = Map.insert ed (p,n) . Map.delete st


-- ignores whether or not you are in check
-- the boolean is whether or not to enforce that the SimpleMove is correct with respect to promotion
isValidMove :: Bool -> GameState -> Piece -> SimpleMove -> Bool
isValidMove False gs p (st,ed,_) = 
  any (\prom -> isValidMove True gs p (st,ed,prom)) [Nothing, Just Queen]
isValidMove True gs p mv = case validate gs p mv of 
  Nothing ->
    False
  Just _ ->
    True

canMoveOrTake :: Piece -> SimpleMove -> Board -> Maybe MoveType
canMoveOrTake (Piece Pawn c) (st, ed, Just pt) brd = 
  case lookAt brd ed of 
    Nothing ->
      Just $ PromoteMove st ed pt
    Just (Piece _ c2) ->
      if c /= c2
      then
        Just $ PromoteTake st ed pt
      else
        Nothing
canMoveOrTake (Piece pt c) (st, ed, prom) brd =
  case lookAt brd ed of 
    Nothing ->
      Just $ Move pt st ed
    Just (Piece _ c2) ->
      if c /= c2
      then
        Just $ Take pt st ed
      else
        Nothing

-- does *no* validation, assumes everything is valid and just updates the board
makeMoveType :: MoveType -> GameState -> Board
makeMoveType (Move pt st ed) GameState{nextTurn=t@(n,c),board=brd} 
  = (Map.insert ed (Piece pt c, n) . Map.delete st) brd
makeMoveType (Take pt st ed) GameState{nextTurn=t@(n,c),board=brd} 
  = (Map.insert ed (Piece pt c, n) . Map.delete st) brd
makeMoveType (PromoteMove st ed pt) GameState{nextTurn=t@(n,c),board=brd} 
  = (Map.insert ed (Piece pt c, n) . Map.delete st) brd
makeMoveType (PromoteTake st ed pt) GameState{nextTurn=t@(n,c),board=brd} 
  = (Map.insert ed (Piece pt c, n) . Map.delete st) brd
makeMoveType (EnPassant st ed op) GameState{nextTurn=t@(n,c),board=brd}
  = (Map.insert ed (Piece Pawn c, n) . Map.delete st . Map.delete op) brd
makeMoveType (CastleKingside) GameState{nextTurn=t@(n,c),board=brd} = 
  let
    rkNum = case c of 
      White ->
        0
      Black ->
        7
  in
    (Map.insert (6,rkNum) (Piece King c, n) . Map.insert (5,rkNum) (Piece Rook c, n) . Map.delete (7,rkNum) . Map.delete (4,rkNum)) brd
makeMoveType (CastleQueenside) GameState{nextTurn=t@(n,c),board=brd} = 
  let
    rkNum = case c of 
      White ->
        0
      Black ->
        7
  in
    (Map.insert (2,rkNum) (Piece King c, n) . Map.insert (3,rkNum) (Piece Rook c, n) . Map.delete (0,rkNum) . Map.delete (4,rkNum)) brd

makeMove :: Move -> GameState -> GameState
makeMove (StdMove turn mt ct) gs =
  gs{nextTurn = succ turn, board = makeMoveType mt gs, status = checkTypeToStatus ct}
makeMove (AnnounceResignation turn) gs = gs{nextTurn=succ turn, status = Resignation}

validate :: GameState -> Piece -> SimpleMove -> Maybe MoveType
validate gs@GameState{board=brd} p@(Piece Knight c) mv@(st,ed,Nothing) = 
  if (ed `elem` possibleMovesForPieceAt p st)
  then
    canMoveOrTake p mv brd
  else 
    Nothing
validate gs@GameState{board=brd} p@(Piece Bishop c) mv@(st,ed,Nothing) = 
  if (ed `elem` possibleMovesForPieceAt p st) && (unblockedBetween st ed brd)
  then
    canMoveOrTake p mv brd
  else 
    Nothing
validate gs@GameState{board=brd} p@(Piece Rook c) mv@(st,ed,Nothing) = 
  if (ed `elem` possibleMovesForPieceAt p st) && (unblockedBetween st ed brd)
  then
    canMoveOrTake p mv brd
  else 
    Nothing
validate gs@GameState{board=brd} p@(Piece Queen c) mv@(st,ed,Nothing) = 
  if (ed `elem` possibleMovesForPieceAt p st) && (unblockedBetween st ed brd)
  then
    canMoveOrTake p mv brd
  else 
    Nothing
validate gs@GameState{board=brd} p@(Piece King c) mv@(st,ed,Nothing) = 
  if (ed `elem` possibleMovesForPieceAt p st) && (distance st ed == 1)
  then
    canMoveOrTake p mv brd
  else -- attempt to castle!
    let
      (x,y) = st
      (x2,y2) = ed
      (rkPos@(x3,_),mv) = if x2 < x then ((0,y),CastleQueenside) else ((7, y), CastleKingside)
    in
      if 
        (unblockedBetween st rkPos brd)
        && 
        (case lookAtLastMove brd st of
          Just ((Piece King c2),0) ->
            c==c2 -- this is a super redundant test
          _ ->
            False
        )
        &&
        (case lookAtLastMove brd rkPos of
          Just ((Piece Rook c2),0) ->
            c==c2 -- this is also fairly redundant xP, if there's an unmoved rook, welp xD
          _ ->
            False
        )
        &&
        (all (not . underAttack gs) $ st:ed:(squaresBetween st ed))
      then
        Just mv
      else
        Nothing
validate gs@GameState{board=brd,nextTurn=t} p@(Piece Pawn c) mv@(st,ed@(x,y),promotion) = 
  if
    (ed `elem` possibleMovesForPieceAt p st)
  then
    let
      stdMv =  -- TODO: clean this the hellll up.
        case promotion of 
          Nothing ->
            if (rank ed /= oppositeGoalRank c)
            then
              Just $ Move Pawn st ed
            else
              Nothing
          Just pt ->
            if (rank ed == oppositeGoalRank c)
            then
              Just $ PromoteMove st ed pt
            else
              Nothing
      stdTk =  -- TODO: clean this the hellll up.
        case promotion of 
          Nothing ->
            if (rank ed /= oppositeGoalRank c)
            then
              Just $ Take Pawn st ed
            else
              Nothing
          Just pt ->
            if (rank ed == oppositeGoalRank c)
            then
              Just $ PromoteTake st ed pt
            else
              Nothing
    in 
      case lookAt brd ed of
        Nothing ->  -- TODO: doesn't handle en passant!
          if (file st == file ed)
          then
            if distance st ed == 2
            then
              if 
                (rank st == pawnStartingRank c)
                && 
                (unblockedBetween st ed brd)
              then
                stdMv
              else
                Nothing
            else
              stdMv
          else -- MUST BE en passant!!
            let 
              mbPawn = (x,y-(pawnDirection c))
            in
              case lookAtLastTurn brd mbPawn of
                Just (Piece Pawn c2, t2) ->
                  if c /= c2 && t2 == pred t
                    -- TODO: still doesn't work, since the pawn can move twice and be taken by en passant
                  then
                    Just $ EnPassant st ed mbPawn
                  else
                    Nothing
                _ ->
                  Nothing
        Just (Piece _ c2) -> 
          if 
            (file st /= file ed)
            &&
            (c /= c2)
          then
            stdTk
          else
            Nothing
  else
    Nothing
validate _ _ _ = Nothing

validateNoCheck :: GameState -> Piece -> SimpleMove -> Maybe MoveType
validateNoCheck gs@GameState{nextTurn=t@(_,c),board=brd} p smv = do
  mv <- validate gs p smv
  let 
    nbrd = makeMoveType mv gs
  if isKingInCheck gs{board=nbrd}
  then
    Nothing
  else
    Just mv

checkCheck :: GameState -> MoveType -> Maybe Move
checkCheck gs@GameState{nextTurn=t,board=brd} mv = 
  let
    nbrd = makeMoveType mv gs
    newst = gs{board=nbrd}
    inChk = isKingInCheck newst
    opst = newst{nextTurn=(succ t)}
    opChk = isKingInCheck opst
    anyValidOp = anyValidMovesNoCheck opst
  in 
    if inChk
    then
      Nothing
    else
      Just $
        StdMove
          t
          mv
          ( if opChk
            then
              if anyValidOp
              then
                MvCheck
              else
                MvMate
            else
              if anyValidOp
              then
                NoCheck
              else
                MvStalemate
          )
      

validateAndCheckCheck :: GameState -> Piece -> SimpleMove -> Maybe Move
validateAndCheckCheck gs p smv = do
  mv <- validate gs p smv
  checkCheck gs mv

anyValidMovesNoCheck :: GameState -> Bool
anyValidMovesNoCheck = not . null . validMovesNoCheck 

--validMoves (w/o check detection) is essential for attack detection in the first place, so here we are
validMovesNoCheck :: GameState -> [MoveType]
validMovesNoCheck gs@GameState{nextTurn=t@(_,c),board=brd} =
  nub . sort . concatMap
    (uncurry $ flip (validMovesForPieceAtNoCheck gs))
    $ pieceLocations c brd

validMovesForPieceAtNoCheck :: GameState -> Piece -> BoardPos -> [MoveType]
validMovesForPieceAtNoCheck gs p st =
  mapMaybe 
    (\(ed,prom) -> validateNoCheck gs p (st,ed,prom)) 
    $ [ (ed,prom) 
      | ed <- possibleMovesForPieceAt p st
      , prom <- [Nothing, Just Queen, Just Bishop, Just Knight, Just Rook]
      ]


pawnDirection :: Color -> Int
pawnDirection White = 1
pawnDirection Black = -1

pawnStartingRank :: Color -> Int
pawnStartingRank White = 2
pawnStartingRank Black = 7

oppositeGoalRank :: Color -> Int
oppositeGoalRank White = 8
oppositeGoalRank Black = 1

validMovesForPieceAt :: GameState -> Piece -> BoardPos -> [Move]
validMovesForPieceAt gs p st =
  mapMaybe (checkCheck gs) 
    $ validMovesForPieceAtNoCheck gs p st

validMoves :: GameState -> [Move]
validMoves gs@GameState{nextTurn=t@(_,c), board=brd} =
  sort . concatMap
    ( \(st,p) -> 
        validMovesForPieceAt gs p st
    )
    $ pieceLocations c brd

