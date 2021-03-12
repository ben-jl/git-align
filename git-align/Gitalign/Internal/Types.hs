module Gitalign.Internal.Types 
    (
        isConnected,
        peek
    )
where
import Data.Hashable as H
import Prelude
import Data.Maybe (listToMaybe)

import qualified Data.Graph.DGraph as D
import qualified Data.Graph.Types as G

isConnected :: forall f a. (Hashable a, Eq a) => f -> D.DGraph a () -> a -> a -> Bool
isConnected _ = G.areAdjacent

peek :: (Hashable a, Eq a) => D.DGraph a () -> Maybe a
peek = listToMaybe . G.vertices 
