{-# LANGUAGE TemplateHaskell #-}

module Main (main) where


import Data.FileEmbed (embedStringFile)

import Lib

main :: IO ()
main = do
  src <- readFile "src/Lib.hs"
  tree <- parse src
  tokens  <- query $(embedStringFile "queries/haskell.scm") tree
  print tokens

  printHighlights $ highlight (lines src) tokens
