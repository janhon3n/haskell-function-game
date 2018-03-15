module GameState where
import Player
import Function
import System.Random



data GameState = GameState {
   turn :: Int,
   playerStates :: [PlayerState]
}

instance Show GameState where
   show (GameState {turn=turn, playerStates=playerStates}) = "\n" ++ (concat $ map (\x -> (show x) ++ "\n") playerStates)
   

randomFunction :: StdGen -> (Function, StdGen)
randomFunction g = do
   let (smallInt, g1) = randomR (0,10) g :: (Int, StdGen)
   let (mediumInt, g2) = randomR (-100,100) g1 :: (Int, StdGen)
   let (largeInt, g3) = randomR (-1000,1000) g2 :: (Int, StdGen)
   let (randomFunctionIndex, g4) = randomR (0,3) g3 :: (Int, StdGen)
   case randomFunctionIndex of
      0 -> (Addition largeInt, g4)
      1 -> (Multiplication mediumInt, g4)
      2 -> (Division mediumInt, g4)
      3 -> (Exponentiation smallInt, g4)

randomHand :: Int -> StdGen -> ([Function], StdGen)
randomHand 0 g = ([], g)
randomHand n g = do
   let (f, g') = randomFunction g
   let (tail, g'') = randomHand (n-1) g'
   (f : tail, g'')