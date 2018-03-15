module Player where
import Function
import HelperFunctions
import System.Random
import System.IO
import Control.Exception
import Data.List (maximumBy)
import Data.Function (on)

data PlayerType = CLIPlayer | HardAIPlayer | RandomPlayer deriving(Show)
data PlayerState = PlayerState {
   playerType :: PlayerType,
   index :: Int,
   score :: Integer,
   hand :: [Function]
}

instance Show PlayerState where
   show (PlayerState {playerType=playerType, index=index, score=score, hand=hand}) = text
      where
         text = (show playerType) ++ " " ++ (show index) ++ "\n" ++ "Score: " ++ (show score) ++ "\n" ++ drawHand hand ++ "\n"
         drawHand hand = "Hand: " ++ (concat $ map (\(i,x) -> i ++ " <==> " ++ (show x) ++ "  ") (zip (map (:[]) ['a'..]) hand))
 
class Player a where 
   chooseFunction :: a -> StdGen -> IO (a, Function, StdGen)

instance Player PlayerState where
   chooseFunction playerState@(PlayerState {playerType=CLIPlayer, index=index, score=score, hand=hand}) g = do
      putStr $ "Player " ++ (show index) ++ ", select your function: "
      hFlush stdout
      userInput <- withEcho False getLine
      putChar '\n'
      let functionIndex = (fromEnum $ userInput !! 0) - 97
      if functionIndex >= length hand
         then
            chooseFunction playerState g
         else do
            let function = hand !! functionIndex
            let newHand = deleteElementAtIndex functionIndex hand
            return (PlayerState {playerType=CLIPlayer, index=index, score=score, hand=newHand}, function, g)

   chooseFunction playerState@(PlayerState {playerType=HardAIPlayer, index=index, score=score, hand=hand}) g = do
      {- pick the function that maximizes the score-}
      let (function, functionIndex) = maximumBy (\(f1,i1) (f2,i2) -> compare (execute f1 score) (execute f2 score)) (zip hand [1..])
      let newHand = deleteElementAtIndex functionIndex hand
      return (PlayerState {playerType=HardAIPlayer, index=index, score=score, hand=newHand}, function, g)

   chooseFunction playerState@(PlayerState {playerType=RandomPlayer, index=index, score=score, hand=hand}) g = do
      let (functionIndex, g') = randomR (0,(length hand - 1)) g :: (Int, StdGen)
      let function = hand !! functionIndex
      let newHand = deleteElementAtIndex functionIndex hand
      return (PlayerState {playerType=RandomPlayer, index=index, score=score, hand=newHand}, function, g)


chooseFunctions :: [PlayerState] -> StdGen -> IO ([PlayerState], [Function], StdGen)
chooseFunctions [] g = return ([], [], g)
chooseFunctions (state:rest) g = do
   (newState, f, g') <- chooseFunction state g
   (newRest, fs, g'') <- chooseFunctions rest g'
   return (newState : newRest, f:fs, g'')