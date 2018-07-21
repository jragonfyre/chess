--
-- Player.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Chess.Player where
  --(
  --) where

import Chess.Types
import Chess

import Data.Maybe (fromMaybe)

import Control.Monad.RWS.Lazy
--type LogProb = Float

type Evaluation = (Float,Float)

--sumLogProbs :: [LogProb] -> LogProb
--sumLogProbs 

pWin :: Evaluation -> Float
pWin (pw,_) = exp pw

pDraw :: Evaluation -> Float
pDraw (_,pd) = exp pd

pLost :: Evaluation -> Float
pLost (pw,pd) = 1.0-pw-pd

makeEvaluation :: Float -> Float -> Evaluation
makeEvaluation = (,)

-- conjugate takes an evaluation for one player to the evaluation for the other player
conjugate :: Evaluation -> Evaluation
conjugate (pw,pd) = (1-pw-pd,pd)

-- pWin, pDraw
-- pLose = 1-pWin-pDraw
type Evaluator = GameState -> Evaluation

argMaxBy :: (Ord b) => (a -> b) -> a -> [a] -> a
argMaxBy f val [] = val
argMaxBy f val (x:xs) = argMaxBy f (if f x > f val then x else val) xs

argMaxByMaybe :: (Ord b) => (a -> b) -> b -> [a] -> Maybe a
argMaxByMaybe f val xs =
  argMaxBy
    (\mv -> case mv of
      Nothing ->
        val
      Just v ->
        f v
    )
    Nothing
    (map Just xs)


-- zero lookahead just directly applies the evaluator to the board
-- otherwise applies lookaheadEvaluator (depth-1) eval to each of the possible moves the current turn player
-- can make
lookaheadEvaluator :: Int -> Evaluator -> Evaluator
lookaheadEvaluator 0 eval = eval
lookaheadEvaluator n eval = \gameState -> 
  let
    turn=nextTurn gameState
    nmoEval = lookaheadEvaluator (n-1) eval
    vm = validMoves gameState
    nbrds = map (\mv -> conjugate . nmoEval $ makeMove mv gameState) vm
  in 
    argMaxBy scoreEvaluation (0,0) nbrds


-- this can throw errors
-- TODO: fix the validMoves thing
moveType :: Move -> MoveType
moveType (StdMove _ mt _) = mt

scoreEvaluation :: Evaluation -> Float
scoreEvaluation (pw,pd) = 2*pw + pd

aiPlayer :: Evaluator -> Chess Move
aiPlayer eval = do
  gameState <- get
  let 
    turn = nextTurn gameState
    brd = board gameState
  let 
    possMoves = validMoves gameState
    maybeBestMove = 
      argMaxByMaybe
        (\mv -> scoreEvaluation . conjugate . eval $ makeMove mv gameState)
        0.0 --(AnnounceResignation turn)
        possMoves
    bestMove = fromMaybe (AnnounceResignation turn) maybeBestMove 
  return bestMove

materialValue :: PieceType -> Float
materialValue King = 0
materialValue Queen = 9
materialValue Pawn = 1
materialValue Bishop = 3.5
materialValue Knight = 3
materialValue Rook = 5

materialCount :: Color -> Board -> Float
materialCount c brd = sum . map (materialValue . pieceType) $ pieces c brd

-- takes difference in material/totalMaterial + 1
-- together with drawRateEstimator must be symmetric, so that 
-- winRateEstimator rat + drawRateEstimator rat + winRateEstimator -rat = 1
-- which means that drawRateEstimator = 1 - winRateEstimator rat - winRateEstimator -rat
winRateEstimator :: Float -> Float
winRateEstimator x = 0.9/(1 + exp (-5*x) + exp (-5*x*x))

drawRateEstimator :: Float -> Float
drawRateEstimator rat = 1 - winRateEstimator rat - winRateEstimator (-rat)

simpleEvaluator :: Evaluator
simpleEvaluator gameState@GameState{nextTurn=turn@(_,c),board=brd} = 
  let
    oc = flipColor c
    omat = materialCount oc brd
    mat = materialCount c brd
    tmat = omat+mat
    dmat = mat-omat
    rat = dmat/(tmat+1)
  in
    makeEvaluation (winRateEstimator rat) (drawRateEstimator rat)

simpleAiPlayer :: Player
simpleAiPlayer = aiPlayer (lookaheadEvaluator 1 simpleEvaluator)




