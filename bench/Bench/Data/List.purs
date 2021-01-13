module Bench.Data.List where

import Prelude
import Data.Foldable (maximum)
import Data.Int (pow)
import Data.List (List(..), take, range, foldr, length, sortBy, addIndexReverse, mapReverse, nub, nubBy, nubBySafe, nubByAdjacentReverse)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith)

benchList :: Effect Unit
benchList = do
  --benchLists "map" $ map (_ + 1)
  --benchLists "foldr" $ foldr add 0

  --benchLists "nub" nub -- not stack safe
  benchLists "nubByAdjacentReverse" $ nubByAdjacentReverse eq -- safe
  benchLists "nubBySafe" $ nubBySafe compare -- safe
  benchLists "addIndexReverse" addIndexReverse -- safe
  benchLists "mapReverse" $ mapReverse $ add 1 -- safe
  benchLists "sortBy" $ sortBy compare -- NOT SAFE!!!!
  --benchLists "nubBy" $ nubBy compare -- not stack safe

  where

  listSizes = Cons 0 $ map (pow 10) $ range 0 6
  nats = range 0 $ (fromMaybe 0 $ maximum listSizes) - 1
  lists = map (\n -> take n nats) listSizes

  benchLists :: forall b. String -> (List Int -> b) -> Effect Unit
  benchLists label func =
    traverse_ (benchAList label func) lists

  benchAList :: forall a b. String -> (List a -> b) -> List a -> Effect Unit
  benchAList label func list = do
    log "---"
    log $ label <> ": list (" <> show (length list) <> " elems)"
    benchWith 1 \_ -> func list