import Data.Char (isDigit)
import Data.List
import Data.Maybe (fromMaybe)

-- Define a data type for arithmetic expressions
data Expr = Number Double | Add Expr Expr | Multiply Expr Expr | Divide Expr Expr | Negate Expr
  deriving (Show)

-- Define a data type for errors
data Error = InvalidInput | DivisionByZero | UnrecognizedOperator | InsufficientOperands
  deriving (Show)

-- Define a type for history
type History = [(Int, Double)]

-- Function to parse a prefix expression
parseExpr :: History -> String -> Either Error (Expr, String)
parseExpr history (x : xs)
  | x `elem` "+*/" = do
      (expr1, rest1) <- parseExpr history xs
      (expr2, rest2) <- parseExpr history rest1
      return
        ( case x of
            '+' -> Add expr1 expr2
            '*' -> Multiply expr1 expr2
            '/' -> Divide expr1 expr2,
          rest2
        )
  | x == '-' = do
      (expr, rest) <- parseExpr history xs
      return (Negate expr, rest)
  | x == '$' = do
      let (refId, remaining) = span isDigit xs
      let maybeValue = lookup (read refId :: Int) history
      case maybeValue of
        Just value -> Right (Number value, remaining)
        Nothing -> Left InvalidInput
  | isDigit x = Right (Number (read [x]), xs)
  | x == ' ' = parseExpr history xs
  | otherwise = Left UnrecognizedOperator
parseExpr _ [] = Left InsufficientOperands

-- Function to evaluate an expression
evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Multiply e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Divide e1 e2) = evalExpr e1 / evalExpr e2
evalExpr (Negate e) = -(evalExpr e)

-- Function to evaluate a prefix expression
evaluate :: String -> History -> Either Error (Double, History)
evaluate str history
  | str == "exit" = Left InvalidInput
  | otherwise = do
      (expr, remaining) <- parseExpr history str
      if null remaining
        then Right (evalExpr expr, (length history + 1, evalExpr expr) : history)
        else Left InvalidInput

-- Function to handle user input and maintain history
handleInput :: History -> IO ()
handleInput history = do
  putStrLn "Enter a prefix expression (e.g., + 2 / * 3 8 2):"
  expr <- getLine
  if expr == "exit"
    then putStrLn "Exiting..."
    else do
      case evaluate expr history of
        Right (result, updatedHistory) -> do
          putStrLn $ "Result (ID " ++ show (length updatedHistory) ++ "): " ++ show result
          handleInput updatedHistory
        Left InvalidInput -> do
          putStrLn "Error: Invalid Expression"
          handleInput history -- Prompt the user to enter the expression again
        Left _ -> do
          putStrLn "Error: Invalid Expression"
          handleInput history -- Prompt the user to enter the expression again

main :: IO ()
main = do
  putStrLn "Expression Calculator (Prefix Notation)"
  handleInput []
