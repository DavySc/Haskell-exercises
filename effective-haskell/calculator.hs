module Calculator where

import Text.Read (readEither)

data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval :: Expr -> Int
eval expr = case expr of
  Lit num -> num
  Add arg1 arg2 -> eval arg1 + eval arg2
  Sub arg1 arg2 -> eval arg1 - eval arg2
  Mul arg1 arg2 -> eval arg1 * eval arg2
  Div arg1 arg2 -> eval arg1 `div` eval arg2

safeEval :: Expr -> Either String Int
safeEval expr = case expr of
  Lit num -> Right num
  Add arg1 arg2 -> eval' (ophelper (+)) arg1 arg2
  Sub arg1 arg2 -> eval' (ophelper (-)) arg1 arg2
  Mul arg1 arg2 -> eval' (ophelper (*)) arg1 arg2
  Div arg1 arg2 -> eval' safeDiv arg1 arg2
  where
    safeDiv a b
      | b == 0 = Left "Error: Division by zero"
      | otherwise = Right $ a `div` b

    ophelper :: (Int -> Int -> Int) -> Int -> Int -> Either String Int
    ophelper op a b = Right $ a `op` b
    eval' :: (Int -> Int -> Either String Int) -> Expr -> Expr -> Either String Int
    eval' operator arg1 arg2 =
      case safeEval arg1 of
        Left err -> Left err
        Right a ->
          case safeEval arg2 of
            Left err -> Left err
            Right b -> operator a b

parse :: String -> Either String Expr
parse str = case parse' (words str) of
  Left err -> Left err
  Right (e, []) -> Right e
  Right (_, rest) -> Left $ "Found extra tokens: " <> unwords rest

parse' :: [String] -> Either String (Expr, [String])
parse' [] = Left "unexpected end of expression"
parse' (token : rest) = case token of
  "+" -> parseBinary Add rest
  "-" -> parseBinary Sub rest
  "*" -> parseBinary Mul rest
  "`div`" -> parseBinary Div rest
  lit -> case readEither lit of
    Left err -> Left err
    Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
parseBinary exprConstructor args =
  case parse' args of
    Left err -> Left err
    Right (firstArg, rest') ->
      case parse' rest' of
        Left err -> Left err
        Right (secondArg, rest'') ->
          Right (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
  case parse expr of
    Left err -> "Error: " <> err
    Right expr' ->
      let answer = show $ eval expr'
       in "The answer is: " <> answer
