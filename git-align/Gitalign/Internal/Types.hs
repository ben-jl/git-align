module Gitalign.Internal.Types 
    (
        isConnected
    )
where
import Data.Hashable as H
import Prelude
import qualified Data.Graph.DGraph as D
import qualified Data.Graph.Types as G

isConnected :: forall f a. (Hashable a, Eq a) => f -> D.DGraph a () -> a -> a -> Bool
isConnected _ = G.areAdjacent
