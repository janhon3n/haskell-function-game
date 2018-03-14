import GameState
import System.Random

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


chooseFunction :: PlayerState -> (PlayerState, Function)
chooseFunction (PlayerState {index=index, score=score, hand=hand}) = do
   let function = head hand
   let newHand = tail hand
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

gameLoop :: GameState -> StdGen -> IO ()
gameLoop (GameState {turn=4, playerStates=_}) _ = return ()
gameLoop gameState@(GameState {turn=turn, playerStates=playerStates}) g = do
   putStrLn $ "Turn " ++ (show turn)
   print gameState
   let (playerStates', chosenFunctions)  = unzip $ map chooseFunction playerStates
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
