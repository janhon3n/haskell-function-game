import GameState
import Data.List (maximumBy)
import Data.Function (on)
import System.Random

deleteElementAtIndex :: Int -> [a] -> [a]
deleteElementAtIndex _ []     = []
deleteElementAtIndex i (a:as)
  | i == 0    = as
  | otherwise = a : deleteElementAtIndex (i-1) as

initPlayerState :: Int -> StdGen -> (PlayerState, StdGen)
initPlayerState i g = do
      let (hand, g') = randomHand 5 g
      (PlayerState {index=i, score=(1000*i*(-1)^i), hand=hand}, g')


initPlayerStates :: Int -> Int -> StdGen -> ([PlayerState], StdGen)
initPlayerStates 0 _ g = ([], g)
initPlayerStates n i g = do
      let (playerState, g') = initPlayerState i g
      let (tail, g'') = initPlayerStates (n-1) (i+1) g'
      (playerState : tail, g'')

initGameState :: StdGen -> (GameState, StdGen)
initGameState g = do
   let (playerStates, g') = initPlayerStates 2 1 g
   (GameState {turn=1, playerStates=playerStates}, g')


getFunction :: PlayerState -> Int -> (PlayerState, Function)
getFunction playerState@(PlayerState {index=index, score=score, hand=hand}) i = do
   if i > length hand
      then error ("Invlid index " ++ (show i))
   else do
      let function = hand !! i
      let newHand = deleteElementAtIndex i hand
      (PlayerState {index=index, score=score, hand=newHand}, function)

executeFunctions :: PlayerState -> [Function] -> PlayerState 
executeFunctions playerState functions = foldl executeFunction playerState functions
   where executeFunction (PlayerState {index=index, score=score, hand=hand}) f = PlayerState {index=index, score=(execute f score), hand=hand}

addRandomFunctionsToPlayerHands :: [PlayerState] -> StdGen -> ([PlayerState], StdGen)
addRandomFunctionsToPlayerHands [] g = ([], g)
addRandomFunctionsToPlayerHands states g = do
      let state = head states
      let (f, g') = randomFunction g
      let newPlayerState = PlayerState {index=(index state), score=(score state), hand=(hand state) ++ [f]}
      let (rest, g'') = addRandomFunctionsToPlayerHands (tail states) g'
      (newPlayerState:rest, g'')

askUsersForInput :: Int -> Int -> IO [Int]
askUsersForInput 0 _ = return []
askUsersForInput n i = do
   putStrLn $ "Player "++ (show i) ++ ", select your function:"
   line <- getLine
   let index = fromEnum (line !! 0) - 97
   rest <- askUsersForInput (n-1) (i+1)
   return (index :  rest)


gameLoop :: GameState -> StdGen -> IO ()
gameLoop (GameState {turn=4, playerStates=playerStates}) _ = do
   let scores = map score playerStates
   let winner = maximumBy (compare `on` score) playerStates
   putStrLn $ "The winner is player " ++ (show (index winner)) ++ " with a score of " ++ (show (score winner))
   return ()

gameLoop gameState@(GameState {turn=turn, playerStates=playerStates}) g = do
   putStrLn $ "Turn " ++ (show turn)
   print gameState
   functionIndicies <- askUsersForInput (length playerStates) 1
   let (playerStates', chosenFunctions)  = unzip $ map (\(x, y) -> getFunction x y) (zip playerStates functionIndicies)
   let playerStates'' = map (\s -> executeFunctions s chosenFunctions) playerStates'
   let (newPlayerStates, g') = addRandomFunctionsToPlayerHands playerStates'' g
   let newGameState = GameState {turn=(turn+1), playerStates=newPlayerStates}
   gameLoop newGameState g


main = do
   g <- newStdGen
   putStrLn "Welcome to the Haskell function game."
   let (gameState, g') = initGameState g
   gameLoop gameState g'
   putStrLn $ "Thanks for playing!"
