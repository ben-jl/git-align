{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Gitalign.Types
    (
        Repository(..)
        , SHA(..)

    )
where
import Data.Graph.Types qualified as  G
import Data.Graph.DGraph qualified as D
import Data.Hashable qualified as H
import Test.QuickCheck ( Arbitrary(arbitrary), Gen, oneof )
import Prelude
import Data.List (map, nub)
import Debug.Trace (trace)
import Data.Maybe

type SHA = Int

newtype Repository = R {unRepo :: D.DGraph Int ()} deriving Show

type Path = [SHA]
