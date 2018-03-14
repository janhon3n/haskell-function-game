module GameState where
import System.Random

data Function = Addition Int | Multiplication Int | Division Int

class Exceutable a where
   execute :: a -> Int -> Int

instance Exceutable Function where 
   execute (Addition i) = (+i)
   execute (Multiplication i) = (*i)
   execute (Division i) = flip quot i

instance Show Function where
   show (Addition i)
      | i >= 0 = "x+" ++ (show i)
      | i < 0 = "x" ++ (show i)
   show (Multiplication i)
      | i >= 0 = "x*" ++ (show i)
      | i < 0 = "x*(" ++ (show i) ++ ")"
   show (Division i)
      | i >= 0 = "x/" ++ (show i)
      | i < 0 = "x/(" ++ (show i) ++ ")"

data PlayerState = PlayerState {
   index :: Int,
   score :: Int,
   hand :: [Function]
}

data GameState = GameState {
   turn :: Int,
   playerStates :: [PlayerState]
}
instance Show PlayerState where
   show (PlayerState {index=index, score=score, hand=hand}) = text
      where
         text = "Player " ++ (show index) ++ "\n" ++ "Score: " ++ (show score) ++ "\n" ++ drawHand hand ++ "\n"
         drawHand hand = "Hand: " ++ (concat $ map (\(i,x) -> i ++ " <==> " ++ (show x) ++ "  ") (zip (map (:[]) ['a'..]) hand))

instance Show GameState where
   show (GameState {turn=turn, playerStates=playerStates}) = "\n" ++ (concat $ map (\x -> (show x) ++ "\n") playerStates)
   

randomFunction :: StdGen -> (Function, StdGen)
randomFunction g = do
   let (randomInteger, g') = randomR (0,999) g :: (Int, StdGen)
   let (randomFunctionIndex, g'') = randomR (0,2) g' :: (Int, StdGen)
   case randomFunctionIndex of
      0 -> (Addition randomInteger, g'')
      1 -> (Multiplication randomInteger, g'')
      2 -> (Division randomInteger, g'')

randomHand :: Int -> StdGen -> ([Function], StdGen)
randomHand 0 g = ([], g)
randomHand n g = do
   let (f, g') = randomFunction g
   let (tail, g'') = randomHand (n-1) g'
   (f : tail, g'')