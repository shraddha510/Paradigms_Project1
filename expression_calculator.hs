-- Importing necessary functions from Data.Char and Data.List modules
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)

-- Define a data type for arithmetic expressions
data Expr = Number Double | Add Expr Expr | Multiply Expr Expr | Divide Expr Expr | Negate Expr
  deriving (Show)

-- Define a data type for errors
data Error = ExitProcess | DivisionByZero | UnrecognizedOperator | InsufficientOperands | NoSuchHistoryId | InvalidExpression
  deriving (Show)

-- Define a type for history
type History = [(Int, Double)]


-- Function to parse a prefix expression
parse :: History -> String -> Either Error (Expr, String)
parse history (' ' : xs) = parse history xs -- Skip spaces
parse history (x : xs)
  | x `elem` "+*/" = do
      -- Parse the first operand
      (expr1, rest1) <- parse history xs
      -- Parse the second operand
      (expr2, rest2) <- parse history rest1
      -- Return the appropriate expression and the remaining string
      return
        ( case x of
            '+' -> Add expr1 expr2
            '*' -> Multiply expr1 expr2
            '/' -> Divide expr1 expr2,
          rest2
        )
  | x == '-' = do
      -- Parse the operand for the unary negation
      (expr, rest) <- parse history xs
      -- Return the Negate expression and the remaining string
      return (Negate expr, rest)
  | x == '$' = do
      -- Extract the reference ID from the input string
      let (refId, remaining) = span isDigit xs
      -- If there are no digits following '$', indicate an invalid expression
      if null refId
        then Left UnrecognizedOperator
        -- Look up the value corresponding to the ID in the history
        else do
          let maybeValue = lookup (read refId :: Int) history
          -- Return the value if found, otherwise indicate the error
          case maybeValue of
            Just value -> Right (Number value, remaining)
            Nothing -> Left NoSuchHistoryId
  | isDigit x = do
      -- Parse the number until a non-digit character is encountered
      let (num, remaining) = span isDigit (x : xs)
      -- Return the parsed number as a Number expression and the remaining string
      Right (Number (read num), remaining)
  | otherwise = Left InvalidExpression
-- If the input string is empty, return an error for insufficient operands
parse _ [] = Left InsufficientOperands


-- Function to evaluate an expression based on operation
evalExpr :: Expr -> Double
evalExpr (Number n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Multiply e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Divide e1 e2) = evalExpr e1 / evalExpr e2
evalExpr (Negate e) = -(evalExpr e)

-- Function to evaluate a prefix expression
evaluate :: String -> History -> Either Error (Double, History)
evaluate str history
  -- If the input is "exit", exit the program
  | str == "exit" = Left ExitProcess
  | otherwise = do
      -- Parse the expression and get the result along with the remaining string
      (expr, remaining) <- parse history str
      -- If there's nothing remaining, return the result along with the updated history
      if null remaining
        then Right (evalExpr expr, (length history + 1, evalExpr expr) : history)
        -- Otherwise, indicate an invalid input
        else Left InvalidExpression

-- Function to trim leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- Function to handle user input and maintain history
mainProcess :: History -> IO ()
mainProcess history = do
  -- Prompt the user for a prefix expression
  putStrLn "Enter a prefix expression (type 'exit' to exit):"
  expr <- fmap trim getLine
  -- Evaluate the expression and handle the result
  case evaluate expr history of
    Right (result, updatedHistory) -> do
      -- Print the result along with its ID
      putStrLn $ "Result (ID " ++ show (length updatedHistory) ++ "): " ++ show result
      -- Recursively handle the next input with the updated history
      mainProcess updatedHistory
    -- If the input is invalid or there's an error, handle it accordingly
    Left ExitProcess -> putStrLn "Exiting..."
    Left NoSuchHistoryId -> do
      -- Handling the no history ID case
      putStrLn "Error: History ID does not exist"
      mainProcess history
    Left InvalidExpression -> do
      -- Handling the invalid expression case
      putStrLn "Error: Invalid Expression"
      mainProcess history
    Left _ -> do
      -- Catch any other errors and treat them as invalid expression
      putStrLn "Error: Invalid Expression"
      mainProcess history

-- Main function to start the program
main :: IO ()
main = do
  -- Print the initial message
  putStrLn "Expression Calculator (Prefix Notation)"
  -- Start handling user input with an empty history
  mainProcess []
