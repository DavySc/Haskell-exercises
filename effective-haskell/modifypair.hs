module ModifyPair where

modifyPair p@(a, b)
  | a == "Hello" = "this is a salutation"
  | b == "George" = "this is a message for George"
  | otherwise = "I don't know what" <> show p <> "means"
