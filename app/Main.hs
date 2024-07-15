module Main (main) where

import Lib

main :: IO ()
main = do
  let src = "main = print 42"
  tree <- parse src
  tokens  <- query "(integer) @constant.numeric.integer" tree
  print tokens

  print $ highlight (lines src) tokens
