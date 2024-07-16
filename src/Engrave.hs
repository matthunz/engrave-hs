{-# LANGUAGE TemplateHaskell #-}

module Engrave (Buffer, Editor, mkEditor, addBuffer, run) where

import Data.FileEmbed (embedStringFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Engrave.Syntax
import System.Console.ANSI (setSGR, useAlternateScreenBuffer, useNormalScreenBuffer)
import System.Console.ANSI.Codes

data Buffer = Buffer
  { _lines :: [String],
    _syntax :: Tree
  }

data Editor = Editor
  { _buffers :: [Buffer],
    _colors :: Map TokenKind (ColorIntensity, Color)
  }

mkEditor :: Editor
mkEditor = Editor [] defaultColors

addBuffer :: String -> Editor -> IO Editor
addBuffer src editor = do
  tree <- parse src Nothing
  tokens <- query $(embedStringFile "queries/haskell.scm") tree
  printLines (_colors editor) (highlight (lines src) tokens)
  return editor {_buffers = Buffer {_lines = [], _syntax = tree} : _buffers editor}

printLines :: Map TokenKind (ColorIntensity, Color) -> [[Highlight]] -> IO ()
printLines colors highlights =
  mapM_
    ( \(row, i) -> do
        setSGR [SetColor Background Dull Black]
        let iStr = show i
        putStr $ replicate (4 - length iStr) ' ' ++ iStr ++ " "
        setSGR [Reset]

        mapM_
          ( \(Highlight s t) -> do
              case t >>= (`Map.lookup` colors) of
                Just (ci, c) -> setSGR [SetColor Foreground ci c]
                Nothing -> pure ()
              putStr s
              setSGR [Reset]
          )
          row
        putStrLn ""
    )
    (zip highlights [(0 :: Int) ..])

run :: IO ()
run = do
  useAlternateScreenBuffer
  src <- readFile "src/Engrave.hs"
  editor <- addBuffer src mkEditor
  _ <- getChar
  useNormalScreenBuffer