--
-- Human.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Chess.Player.Human where
  --(
  --) where
  
import Chess.Types
import Chess
import Chess.Notation

import System.Console.ANSI hiding (Color, Black, White)
import qualified System.Console.ANSI as ANSI
import System.Console.Readline

import Data.Char (toLower)

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy

humanPlayer :: Player
humanPlayer = do
  gameState <- get
  let 
    turn = nextTurn gameState
    brd = board gameState
    tn = turnNumber turn
    tcol = turnColor turn
  mbSt <- liftIO . fix $ \loop ->
    do
      mbSq <-
        promptSquare
          (show turn ++ ": ")
          (displayChessBoard brd (map (endingSquare turn) $ validMovesNoCheck gameState))
      case mbSq of 
        Nothing ->
          return Nothing 
        Just sq ->
          case lookAt brd sq of
            Nothing -> do
              putStrLn "Please select a piece."
              loop
            Just p@(Piece _ c) -> do
              putStrLn $ show p
              if c == tcol
              then
                return $ Just (sq,p)
              else do
                putStrLn "You can't move that piece!"
                loop
  case mbSt of 
    Nothing ->
      return (AnnounceResignation turn)
    Just (st,pc) -> do
      let possMoves = map (endingSquare turn) $ validMovesForPieceAtNoCheck gameState pc st
      mbEd <- liftIO . fix $ \loop ->
        do
          mbSq <-
            promptSquare
              ("Move " ++ (posStr st) ++ " to: ")
              (displayChessBoard brd possMoves)
          case mbSq of 
            Nothing -> 
              return Nothing
            Just sq -> 
              if sq==st
              then
                return Nothing
              else
                if
                  sq `elem` possMoves
                then
                  return $ Just sq
                else do
                  putStrLn "Invalid move, select one of the highlighted squares."
                  loop
      case mbEd of 
        Nothing ->
          humanPlayer
        Just ed ->
          do
            mbmbProm <- 
              if pieceType pc == Pawn && rank ed == 8
              then do
                pt <- liftIO $ promptPromotion "Promote pawn to (x to cancel): " (displayChessBoard brd [st,ed])
                return $ case pt of
                  Nothing ->
                    Nothing
                  _ ->
                    Just pt
              else
                return $ Just Nothing
            case mbmbProm of 
              Nothing ->
                -- this is where cancellation should go to.
                humanPlayer
              Just mbProm ->
                maybe 
                  (do
                    liftIO $ putStrLn "Move is invalid: Don't know why. xD This shouldn't happen xP"
                    humanPlayer 
                  )
                  (\mv -> do
                      return mv
                  )
                  $ validateAndCheckCheck gameState pc (st,ed,mbProm)
  


promptSquare :: String -> IO () -> IO (Maybe BoardPos)
promptSquare prompt act = do
  act
  mStr <- readline $ prompt 
  case mStr of
    Nothing ->
      promptSquare prompt act
    Just "q" ->
      return Nothing
    Just str ->
      do
        addHistory str
        case readBoardPos str of
          Nothing ->
            do
              putStrLn "Invalid square."
              promptSquare prompt act
          Just bp ->
            return $ Just bp

promptPromotion :: String -> IO () -> IO (Maybe PieceType)
promptPromotion prompt act = do
  act
  mStr <- readline $ prompt 
  case fmap (map toLower) mStr of
    Nothing ->
      promptPromotion prompt act
    Just "x" ->
      return Nothing
    Just "q" ->
      return $ Just Queen
    Just "queen" ->
      return $ Just Queen
    Just "r" ->
      return $ Just Rook
    Just "rook" ->
      return $ Just Rook
    Just "b" ->
      return $ Just Bishop
    Just "bishop" ->
      return $ Just Bishop
    Just "n" ->
      return $ Just Knight
    Just "knight" ->
      return $ Just Knight
    _ -> do
      putStrLn "Enter either the name or character of the piece you want to promote to (x to cancel)"
      promptPromotion prompt act

-- board and highlights to display
displayChessBoard :: Board -> [BoardPos] -> IO ()
displayChessBoard brd highlights = do
  putStrLn "displayChessBoard enter"
  forM_ [7,6..0] $ \y -> do
    --putStrLn "test 1"
    setSGR [Reset, SetConsoleIntensity BoldIntensity]
    --putStrLn "test 2"
    putStr $ toRankStr y
    --putStrLn "test 3"
    forM_ [0..7] $ \x -> do
      --putStrLn "test 4"
      let bp = makeBoardPos x y
      setSGR [ ( if bp `elem` highlights
                 then SetColor Background Vivid Red
                 else
                   SetColor Background Dull $
                     if (x+y) `mod` 2 == 0
                     then ANSI.Blue
                     else ANSI.Yellow) ]
      --putStrLn "test 5"
      case lookAt brd bp of
        Nothing -> 
          putStr " "
        Just (Piece pt c) -> do
          setSGR [ SetColor Foreground Vivid (if c==White then ANSI.White else ANSI.Black) ]
          putStr [pieceTLetter pt]
      --putStrLn "test 6"
    setSGR [Reset]
    putStrLn ""
  setSGR [SetConsoleIntensity BoldIntensity]
  putStrLn " abcdefgh "
  setSGR [Reset]
  putStrLn "displayChessBoard exit"
