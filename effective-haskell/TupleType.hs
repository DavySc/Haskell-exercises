module TupleTypes where

permuteThruple :: (a, b, c) -> ((a, b, c), (a, c, b), (b, a, c), (b, c, a), (c, a, b), (c, b, a))
permuteThruple (a, b, c) = ((a, b, c), (a, c, b), (b, a, c), (b, c, a), (c, a, b), (c, b, a))

mergeFirstTwo :: (a, b, c) -> (a -> b -> d) -> (d, c)
mergeFirstTwo (a, b, c) f = (f a b, c)

showFields :: String
showFields =
  let (a, b) = combinePermutations . permuteThruple $ ("hello", "world", 10)
   in unlines [fst a, fst b]
  where
    joinFields a b = show a <> " - " <> b
    combinePermutations (a, b, c, d, e, f) =
      ( mergeFirstTwo a joinFields,
        mergeFirstTwo c joinFields
      )
