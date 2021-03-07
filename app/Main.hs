{-# LANGUAGE OverloadedStrings #-}
module Main where

import TFSTypes

main :: IO ()
main = print (TFSConnectionCfg "api-key" "username")
