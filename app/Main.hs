module Main where

import Chess
import Chess.Types
import Chess.Notation
import Chess.Player
import Chess.Player.Human

import System.Console.ANSI hiding (Color, Black, White)
import qualified System.Console.ANSI as ANSI
import System.Console.Readline

import Control.Monad
import Control.Monad.RWS.Lazy

import Data.Maybe (maybe)
import Data.Char (toLower)

import qualified Data.Map.Strict as Map

-- TODO: fix en passant implementation
-- TODO: refactor chessLoop to allow for "players"
-- TODO: Detect game end.


newGame = GameState (1,White) NoStatus startingBoard

chessLoop :: Player -> Player -> Chess GameEndStatus
chessLoop currentPlayer nextPlayer = do
  gameState <- get
  let 
    turn = nextTurn gameState
    brd = board gameState
  mv <- currentPlayer
  liftIO . putStrLn $ show mv
  liftIO . putStrLn $ notateMove mv []
  let
    ngst = makeMove mv gameState
  put ngst
  case gameEndStatus ngst of
    Nothing ->
      chessLoop nextPlayer currentPlayer
    Just ges ->
      return ges

playChess :: Player -> Player -> IO ()
playChess player1 player2 = do
  runRWST (chessLoop player1 player2) () newGame
  return ()

playComputer :: IO ()
playComputer = playChess humanPlayer simpleAiPlayer

playHuman :: IO ()
playHuman = playChess humanPlayer humanPlayer

{- test sequence:
 - e2
 - e4
 - e7
 - e5
 - f1
 - c4
 - f8
 - b4
 - g1
 - h3
 - g8
 - h6
 -}

main :: IO ()
main = playComputer
