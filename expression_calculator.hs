import Data.Char (isDigit)
import Data.List
import Data.Maybe (fromMaybe)

-- Define a data type for arithmetic expressions
data Expr = Number Double | Add Expr Expr | Subtract Expr Expr | Multiply Expr Expr | Divide Expr Expr
  deriving (Show)

-- Define a data type for errors
data Error = InvalidInput | DivisionByZero | UnrecognizedOperator | InsufficientOperands
  deriving (Show)

-- Define a type for history
type History = [(Int, Double)]

-- Function to parse a prefix expression
parseExpr :: [String] -> Either Error (Expr, [String])
parseExpr (x : xs)
  | isPrefixOf "+" x = do
      (expr1, rest1) <- parseExpr xs
      (expr2, rest2) <- parseExpr rest1
      return (Add expr1 expr2, rest2)
  | isPrefixOf "-" x = do
      (expr1, rest1) <- parseExpr xs
      (expr2, rest2) <- parseExpr rest1
      return (Subtract expr1 expr2, rest2)
  | isPrefixOf "*" x = do
      (expr1, rest1) <- parseExpr xs
      (expr2, rest2) <- parseExpr rest1
      return (Multiply expr1 expr2, rest2)
  | isPrefixOf "/" x = do
      (expr1, rest1) <- parseExpr xs
      (expr2, rest2) <- parseExpr rest1
      if evalExpr expr2 == 0
        then Left DivisionByZero
        else return (Divide expr1 expr2, rest2)
  | all isDigit x = return (Number (read x), xs)
  | otherwise = Left UnrecognizedOperator

-- Function to evaluate an expression
evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Subtract e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Multiply e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Divide e1 e2) = evalExpr e1 / evalExpr e2

-- Function to evaluate a prefix expression
evaluate :: String -> History -> Either Error (Double, History)
evaluate str history = do
  let tokens = words str
  (expr, remainingTokens) <- parseExpr tokens
  if null remainingTokens
    then Right (evalExpr expr, (length history + 1, evalExpr expr) : history)
    else Left InvalidInput

-- Function to handle user input and maintain history
handleInput :: History -> IO ()
handleInput history = do
  putStrLn "Enter a prefix expression (e.g., + 2 / * 3 8 2):"
  expr <- getLine
  case evaluate expr history of
    Right (result, updatedHistory) -> do
      putStrLn $ "Result (ID " ++ show (length updatedHistory) ++ "): " ++ show result
      handleInput updatedHistory
    Left err -> do
      putStrLn "Error: Invalid Expression"
      handleInput history

main :: IO ()
main = do
  putStrLn "Expression Calculator (Prefix Notation)"
  handleInput []
