import Data.Char (isDigit)

type History = [(Double, Int)] -- Updated to store the value along with its index

-- Function to evaluate a prefix expression
evaluateExpression :: History -> String -> (Maybe Double, History)
evaluateExpression history expr = (result, newHistory)
  where
    eval :: [String] -> Maybe Double
    eval [] = Nothing
    eval (x : xs)
      | all isDigit x = Just (read x)
      | length xs < 2 = Nothing
      | otherwise = case x of
          "+" -> binOp (+)
          "-" -> binOp (-)
          "*" -> binOp (*)
          "/" -> binOp (/)
          _ -> Nothing
      where
        binOp f = case (eval (init xs), eval [last xs]) of
          (Just operand1, Just operand2) -> Just (f operand1 operand2)
          _ -> Nothing

    (result, newId) = case eval (words expr) of
      Just res -> (Just res, length history)
      Nothing -> (Nothing, length history)

    newHistory =
      ( case result of
          Just res -> (res, newId)
          Nothing -> (0, newId)
      )
        : history

printHistoryValue :: History -> Int -> IO ()
printHistoryValue history id = do
  let maybeValue = lookup id (zip (map snd history) [1 ..]) -- Look up the index
  case maybeValue of
    Just index -> case lookup index history of
      Just value -> putStrLn $ "History id " ++ show id ++ " (Index " ++ show index ++ "): " ++ show value
      Nothing -> putStrLn $ "Value not found for history id " ++ show id
    Nothing -> putStrLn $ "History id " ++ show id ++ " not found"

main :: IO ()
main = do
  putStrLn "Enter a prefix expression (or 'exit' to quit): "
  loop []

loop :: History -> IO ()
loop history = do
  expression <- getLine
  if expression == "exit"
    then putStrLn "Goodbye!"
    else do
      let (result, newHistory) = evaluateExpression history expression
      case result of
        Just res -> do
          putStrLn $ "Result: " ++ show res ++ " (History id: " ++ show (length history + 1) ++ ")" -- Incrementing index
          putStrLn "Enter a prefix expression (or 'exit' to quit): " -- Prompt for the next expression
          loop newHistory
        Nothing -> do
          putStrLn "Invalid expression"
          putStrLn "Enter a prefix expression (or 'exit' to quit): " -- Prompt for the next expression
          loop history
