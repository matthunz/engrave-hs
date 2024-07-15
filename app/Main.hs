module Main (main) where

import Lib

main :: IO ()
main = do
  tree <- parse "main = putStrLn \"Hello, World!\""
  print "Hello, World!"

