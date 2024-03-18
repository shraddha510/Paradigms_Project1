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
parseExpr :: History -> [String] -> Either Error (Expr, [String])
parseExpr history (x : xs)
  -- Check for addition operator
  | isPrefixOf "+" x = do
      -- Parse the first expression
      (expr1, rest1) <- parseExpr history xs
      -- Parse the second expression
      (expr2, rest2) <- parseExpr history rest1
      -- Return the addition expression and the remaining tokens
      return (Add expr1 expr2, rest2)
  -- Check for multiplication operator
  | isPrefixOf "*" x = do
      -- Parse the first expression
      (expr1, rest1) <- parseExpr history xs
      -- Parse the second expression
      (expr2, rest2) <- parseExpr history rest1
      -- Return the multiplication expression and the remaining tokens
      return (Multiply expr1 expr2, rest2)
  -- Check for division operator
  | isPrefixOf "/" x = do
      -- Parse the first expression
      (expr1, rest1) <- parseExpr history xs
      -- Parse the second expression
      (expr2, rest2) <- parseExpr history rest1
      -- Check for division by zero
      if evalExpr expr2 == 0
        then Left DivisionByZero
        else return (Divide expr1 expr2, rest2)
  -- Check for negation operator
  | isPrefixOf "-" x = do
      -- Parse the expression to be negated
      (expr, rest) <- parseExpr history xs
      -- Return the negated expression and the remaining tokens
      return (Negate expr, rest)
  -- Check for history reference
  | "$" `isPrefixOf` x = do
      -- Extract the reference ID
      let refId = read (drop 1 x) :: Int
      -- Lookup the value corresponding to the reference ID in the history
      let maybeValue = lookup refId history
      -- If value found, return it as a number
      case maybeValue of
        Just value -> return (Number value, xs)
        -- If value not found, report it as an error
        Nothing -> Left InvalidInput
  -- Check for literal number
  | all isDigit x = return (Number (read x), xs)
  -- If none of the above, report it as an unrecognized operator
  | otherwise = Left UnrecognizedOperator

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
  -- Check if the input is "exit"
  | str == "exit" = Left InvalidInput
  -- Otherwise, proceed with evaluation
  | otherwise = do
      let tokens = words str
      -- Parse the expression
      (expr, remainingTokens) <- parseExpr history tokens
      -- If there are no remaining tokens, return the result and update the history
      if null remainingTokens
        then Right (evalExpr expr, (length history + 1, evalExpr expr) : history)
        -- Otherwise, report it as an invalid input
        else Left InvalidInput

-- Function to handle user input and maintain history
handleInput :: History -> IO ()
handleInput history = do
  putStrLn "Enter a prefix expression (e.g., + 2 / * 3 8 2):"
  -- Read user input
  expr <- getLine
  -- Evaluate the expression
  case evaluate expr history of
    -- If evaluation succeeds, print the result and proceed with the updated history
    Right (result, updatedHistory) -> do
      putStrLn $ "Result (ID " ++ show (length updatedHistory) ++ "): " ++ show result
      handleInput updatedHistory
    -- If evaluation fails, handle the error
    Left InvalidInput -> putStrLn "Exiting..."
    Left err -> do
      putStrLn "Error: Invalid Expression"
      handleInput history

main :: IO ()
main = do
  putStrLn "Expression Calculator (Prefix Notation)"
  -- Start handling user input with an empty history
  handleInput []
