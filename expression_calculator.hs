-- Importing necessary function from Data.Char module
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd)

-- Define a data type for arithmetic expressions
data Expr = Number Double | Add Expr Expr | Multiply Expr Expr | Divide Expr Expr | Negate Expr
  deriving (Show)

-- Define a data type for errors
data Error = ExitProcess | DivisionByZero | UnrecognizedOperator | InsufficientOperands | NoSuchHistoryId
  deriving (Show)

-- Define a type for history
type History = [(Int, Double)]

-- Function to parse a prefix expression
parseExpr :: History -> String -> Either Error (Expr, String)
parseExpr history (' ' : xs) = parseExpr history xs -- Skip spaces
parseExpr history (x : xs)
  | x `elem` "+*/" = do
      -- Parse the first operand
      (expr1, rest1) <- parseExpr history xs
      -- Parse the second operand
      (expr2, rest2) <- parseExpr history rest1
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
      (expr, rest) <- parseExpr history xs
      -- Return the Negate expression and the remaining string
      return (Negate expr, rest)
  | x == '$' = do
      -- Extract the reference ID from the input string
      let (refId, remaining) = span isDigit xs
      -- Look up the value corresponding to the ID in the history
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
  | otherwise = Left UnrecognizedOperator
-- If the input string is empty, return an error for insufficient operands
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
  -- If the input is "exit", exit the program
  | str == "exit" = Left ExitProcess
  | otherwise = do
      -- Parse the expression and get the result along with the remaining string
      (expr, remaining) <- parseExpr history str
      -- If there's nothing remaining, return the result along with the updated history
      if null remaining
        then Right (evalExpr expr, (length history + 1, evalExpr expr) : history)
        -- Otherwise, indicate an invalid input
        else Left ExitProcess

-- Function to trim leading and trailing spaces from a string
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- Function to handle user input and maintain history
handleInput :: History -> IO ()
handleInput history = do
  -- Prompt the user for a prefix expression
  putStrLn "Enter a prefix expression (type 'exit' to exit):"
  expr <- fmap trim getLine
  -- Evaluate the expression and handle the result
  case evaluate expr history of
    Right (result, updatedHistory) -> do
      -- Print the result along with its ID
      putStrLn $ "Result (ID " ++ show (length updatedHistory) ++ "): " ++ show result
      -- Recursively handle the next input with the updated history
      handleInput updatedHistory
    -- If the input is invalid or there's an error, handle it accordingly
    Left ExitProcess -> putStrLn "Exiting..."
    Left NoSuchHistoryId -> do
      -- Handling the new error case
      putStrLn "Error: History ID does not exist"
      handleInput history
    Left _ -> do
      putStrLn "Error: Invalid Expression"
      handleInput history

-- Main function to start the program
main :: IO ()
main = do
  -- Print the initial message
  putStrLn "Expression Calculator (Prefix Notation)"
  -- Start handling user input with an empty history
  handleInput []
