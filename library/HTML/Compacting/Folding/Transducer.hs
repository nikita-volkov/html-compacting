module HTML.Compacting.Folding.Transducer
where

import HTML.Prelude hiding (filter)
import Control.Foldl


type Transducer input1 input2 =
  forall output. Fold input2 output -> Fold input1 output

filterMap :: (a -> Maybe b) -> Transducer a b
filterMap =
  undefined

map :: (a -> b) -> Transducer a b
map =
  premap

filter :: (a -> Bool) -> Transducer a a
filter predicate =
  handles (filtered predicate)
