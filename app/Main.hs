{-# LANGUAGE OverloadedStrings #-}
module Main where

import TFS.Types

main :: IO ()
main = print (TFSConnectionCfg "api-key" "username")
