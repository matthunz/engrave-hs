{-# LANGUAGE TemplateHaskell #-}

module Engrave (Buffer, Editor, mkEditor, addBuffer, run) where

import Data.FileEmbed (embedStringFile)
import Data.Word (Word32)
import Engrave.Syntax
import System.Console.ANSI (useAlternateScreenBuffer, useNormalScreenBuffer)

data Buffer = Buffer
  { _lines :: [String],
    _syntax :: Tree
  }

data Editor = Editor
  { _buffers :: [Buffer]
  }

mkEditor :: Editor
mkEditor = Editor []

addBuffer :: String -> Editor -> IO Editor
addBuffer src editor = do
  tree <- parse src Nothing
  tokens <- query $(embedStringFile "queries/haskell.scm") tree
  printHighlights defaultColors (highlight (lines src) tokens)
  return editor {_buffers = Buffer {_lines = [], _syntax = tree} : _buffers editor}

run :: IO ()
run = do
  useAlternateScreenBuffer
  src <- readFile "app/Main.hs"
  editor <- addBuffer src mkEditor
  _ <- getChar
  useNormalScreenBuffer