module Newton where

diff f dx x = (f (x + dx) - f x) / dx

newtonIter f f' x 0 = x
newtonIter f f' x k = newtonIter f f' (x - f x / f' x) (k - 1)
