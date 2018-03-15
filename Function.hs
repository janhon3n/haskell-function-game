module Function where

data Function = Addition Int | Multiplication Int | Division Int | Exponentiation Int    

class Exceutable a where
   execute :: a -> Integer -> Integer
   showExecution :: a -> Integer -> String
 
instance Exceutable Function where 
   execute (Addition i) = (+ (toInteger i))
   execute (Multiplication i) = (* (toInteger i))
   execute (Division i) = flip quot (toInteger i)
   execute (Exponentiation i) = flip (^) (toInteger i)
   
   showExecution f score = "Function " ++ (show f) ++ " turned " ++ (show score) ++ " into " ++ (show (execute f score))

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
   show (Exponentiation i)
      | i >= 0 = "x^" ++ (show i)
      | i < 0 = "x^(" ++ (show i) ++ ")"
