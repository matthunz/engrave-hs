{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.FileEmbed (embedStringFile)
import Engrave.Syntax

main :: IO ()
main = do
  src <- readFile "app/Main.hs"
  tree <- parse src Nothing
  tokens <- query $(embedStringFile "queries/haskell.scm") tree
  printHighlights defaultColors (highlight (lines src) tokens) 
