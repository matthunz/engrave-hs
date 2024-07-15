module Main (main) where

import Lib

main :: IO ()
main = do
  tree <- parse "main = print 42"
  tokens  <- query "(integer) @constant.numeric.integer" tree
  print tokens
