{-# LANGUAGE TemplateHaskell #-}

module Main (main) where


import Data.FileEmbed (embedStringFile)

import Lib

main :: IO ()
main = do
  let src = "main = print 42"
  tree <- parse src
  tokens  <- query $(embedStringFile "queries/haskell.scm") tree
  print tokens

  print $ highlight (lines src) tokens
