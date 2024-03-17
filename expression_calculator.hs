import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)

-- Define types
type Expression = [String]

type Result = Int

type History = [(Result, Expression)]

-- Function to evaluate an expression and update history
evaluateAndUpdateHistory :: Expression -> History -> Maybe History
evaluateAndUpdateHistory expr history =
  case evaluateExpression expr history of
    Just (result, remainingExpr) -> Just ((result, expr) : history)
    Nothing -> Nothing

-- Function to evaluate an expression
evaluateExpression :: Expression -> History -> Maybe (Result, Expression)
evaluateExpression [] _ = Nothing -- Empty expression is invalid
evaluateExpression (op : rest) history
  | all isDigit op = Just (read op, rest) -- If the first element is a number, return it as the result
  | op == "+" = evaluateBinaryOperation (+) rest history
  | op == "-" = evaluateUnaryOperation negate rest history
  | op == "*" = evaluateBinaryOperation (*) rest history
  | op == "/" = evaluateBinaryOperation divWithCheck rest history
  | '$' `elem` op = evaluateFromHistory (read $ tail op) rest history
  | otherwise = Nothing -- Invalid operator

-- Function to evaluate a binary operation
evaluateBinaryOperation :: (Result -> Result -> Result) -> Expression -> History -> Maybe (Result, Expression)
evaluateBinaryOperation _ [] _ = Nothing
evaluateBinaryOperation _ [_] _ = Nothing
evaluateBinaryOperation op (x : y : rest) history = do
  (xVal, _) <- evaluateExpression [x] history
  (yVal, remaining) <- evaluateExpression [y] history
  let result = xVal `op` yVal
  return (result, remaining ++ rest)

-- Function to evaluate a unary operation
evaluateUnaryOperation :: (Result -> Result) -> Expression -> History -> Maybe (Result, Expression)
evaluateUnaryOperation _ [] _ = Nothing
evaluateUnaryOperation op (x : rest) history = do
  (xVal, remaining) <- evaluateExpression [x] history
  let result = op xVal
  return (result, remaining ++ rest)

-- Function to evaluate expression from history
evaluateFromHistory :: Result -> Expression -> History -> Maybe (Result, Expression)
evaluateFromHistory n rest history = do
  (val, remaining) <- lookupHistory n history
  evaluateExpression (show val : rest) history

-- Function to lookup value from history
lookupHistory :: Result -> History -> Maybe (Result, Expression)
lookupHistory _ [] = Nothing
lookupHistory 1 ((val, expr) : _) = Just (val, expr)
lookupHistory n (_ : rest) = lookupHistory (n - 1) rest

-- Function to check and handle division by zero
divWithCheck :: Result -> Result -> Result
divWithCheck _ 0 = error "Division by zero"
divWithCheck x y = x `div` y

-- Function to parse input into expression tokens
parseInput :: String -> Expression
parseInput = filter (not . null) . words

-- Function to handle user input and evaluation
evalLoop :: History -> IO ()
evalLoop history = do
  putStrLn "Enter expression in prefix notation:"
  input <- getLine
  let expr = parseInput input
  case evaluateAndUpdateHistory expr history of
    Just newHistory -> do
      putStrLn "Result:"
      printResult (head newHistory)
      evalLoop newHistory
    Nothing -> do
      putStrLn "Invalid Expression"
      evalLoop history

-- Function to print result with history id
printResult :: (Result, Expression) -> IO ()
printResult (result, expr) = putStrLn $ "History ID " ++ show (length expr) ++ ": " ++ show result

-- Main function
main :: IO ()
main = evalLoop []
