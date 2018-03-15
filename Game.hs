import GameState
import Player
import Function
import HelperFunctions
import Data.List (maximumBy)
import Data.Function (on)
import System.Random

initPlayerState :: Int -> PlayerType -> StdGen -> (PlayerState, StdGen)
initPlayerState i t g = do
      let (hand, g') = randomHand 3 g
      (PlayerState {playerType=t, index=i, score=(toInteger (1000*i*(-1)^i)), hand=hand}, g')

initPlayerStates :: [PlayerType] -> Int -> StdGen -> ([PlayerState], StdGen)
initPlayerStates [] i g = ([], g)
initPlayerStates (t:ts) i g = do
      let (playerState, g') = initPlayerState i t g
      let (tail, g'') = initPlayerStates ts (i+1) g'
      (playerState : tail, g'')

initGameState :: [PlayerType] -> StdGen -> (GameState, StdGen)
initGameState pts g = do
   let (playerStates, g') = initPlayerStates pts 1 g
   (GameState {turn=1, playerStates=playerStates}, g')


getFunction :: PlayerState -> Int -> (PlayerState, Function)
getFunction playerState@(PlayerState {playerType=playerType, index=index, score=score, hand=hand}) i = do
   if i > length hand
      then error ("Invlid index " ++ (show i))
   else do
      let function = hand !! i
      let newHand = deleteElementAtIndex i hand
      (PlayerState {playerType=playerType, index=index, score=score, hand=newHand}, function)

executeFunction :: PlayerState -> Function -> PlayerState
executeFunction (PlayerState {playerType=playerType, index=index, score=score, hand=hand}) f = do
   PlayerState {playerType=playerType, index=index, score=(execute f score), hand=hand}

executeFunctions :: PlayerState -> [Function] -> PlayerState 
executeFunctions playerState functions = do
   foldl executeFunction playerState functions

addRandomFunctionsToPlayerHands :: [PlayerState] -> StdGen -> ([PlayerState], StdGen)
addRandomFunctionsToPlayerHands [] g = ([], g)
addRandomFunctionsToPlayerHands states g = do
   let state = head states
   let (f, g') = randomFunction g
   let newPlayerState = PlayerState {playerType=(playerType state), index=(index state), score=(score state), hand=(hand state) ++ [f]}
   let (rest, g'') = addRandomFunctionsToPlayerHands (tail states) g'
   (newPlayerState:rest, g'')


gameLoop :: GameState -> StdGen -> IO ()
gameLoop (GameState {turn=4, playerStates=playerStates}) _ = do
   let scores = map score playerStates
   let winner = maximumBy (compare `on` score) playerStates
   putStrLn $ "The winner is player " ++ (show (index winner)) ++ " with a score of " ++ (show (score winner))
   return ()

gameLoop gameState@(GameState {turn=turn, playerStates=playerStates}) g = do
   putStrLn $ "Turn " ++ (show turn)      
   print gameState
   (playerStates', chosenFunctions, g') <- chooseFunctions playerStates g
   let playerStates'' = map (\s -> executeFunctions s chosenFunctions) playerStates'
   putStrLn $ concat $ map (\f -> concat (map (\s -> showExecution f (score s) ++ "\n") playerStates')) chosenFunctions
   let (newPlayerStates, g') = addRandomFunctionsToPlayerHands playerStates'' g
   let newGameState = GameState {turn=(turn+1), playerStates=newPlayerStates}
   gameLoop newGameState g


main = do
   g <- newStdGen
   putStrLn "Welcome to the Haskell function game."
   let (gameState, g') = initGameState [CLIPlayer, HardAIPlayer] g
   gameLoop gameState g'
   putStrLn $ "Thanks for playing!"
