module HelperFunctions where
import System.IO
import Control.Exception

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
   old <- hGetEcho stdin
   bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

deleteElementAtIndex :: Int -> [a] -> [a]
deleteElementAtIndex _ [] = []
deleteElementAtIndex i (a:as)
  | i == 0    = as
  | otherwise = a : deleteElementAtIndex (i-1) as
