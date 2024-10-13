module MyConcatty where

concatMapr f = foldr (\x acc -> f x <> acc) []

concatMapl f = foldl (\acc x -> acc <> f x) []
