module MapExamples where

import Prelude hiding (map)

map func tgt =
  if null tgt
    then []
    else func (head tgt) : map func (tail tgt)
