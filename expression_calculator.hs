import Text.Read (readMaybe)

-- Custom function to split a string into tokens
splitExpr :: String -> [String]
splitExpr "" = []
splitExpr s@(c : cs)
  | c `elem` "+-*/" = [c] : splitExpr cs
  | otherwise =
      let (token, rest) = span (`elem` "+-*/") s
       in token : splitExpr rest

-- Function to evaluate a prefix expression
evaluate :: String -> Maybe Double
evaluate expr = case splitExpr expr of
  [] -> Nothing
  (op : ops) -> case op of
    "+" -> foldl (\acc x -> (+) <$> acc <*> readMaybe x) (Just 0) ops
    "-" -> case ops of
      [x] -> negate <$> readMaybe x
      xs -> negate <$> evaluate (unwords xs)
    "*" -> foldl (\acc x -> (*) <$> acc <*> readMaybe x) (Just 1) ops
    "/" -> foldl (\acc x -> (/) <$> acc <*> readMaybe x) (Just 1) ops
    _ -> Nothing

-- Function to continuously prompt the user for expressions
promptLoop :: IO ()
promptLoop = do
  putStrLn "Enter a prefix expression (e.g., + 3 4):"
  input <- getLine
  case evaluate input of
    Just result -> putStrLn $ "Result: " ++ show result
    Nothing -> putStrLn "Invalid expression. Please try again."
  promptLoop

main :: IO ()
main = do
  putStrLn "Prefix Expression Calculator"
  putStrLn "-----------------------------"
  promptLoop
