{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Engrave (Buffer, Editor, mkEditor, addBuffer, run) where

import Data.FileEmbed (embedStringFile)
import Data.Map (Map)
import qualified Data.Map as Map
import Engrave.Syntax
import System.Console.ANSI (clearLine, setCursorColumn, setSGR, useAlternateScreenBuffer, useNormalScreenBuffer, setCursorPosition)
import System.Console.ANSI.Codes
import System.IO (hFlush, stdout)

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
  edits <- highlightBuf src (_colors editor) tree
  drawEdits edits

  return editor {_buffers = Buffer {_lines = [], _syntax = tree} : _buffers editor}

highlightBuf :: String -> Map TokenKind (ColorIntensity, Color) -> Tree -> IO [BufferEdit]
highlightBuf src colors tree = do
  tokens <- query $(embedStringFile "queries/haskell.scm") tree
  let highlights = highlight (lines src) tokens
      edits = diff [] highlights 0
  return $
    map
      ( \case
          Insert i hs ->
            InsertBuf
              i
              ( concatMap
                  ( \(Highlight s t) ->
                      case t >>= (`Map.lookup` colors) of
                        Just (ci, c) -> setSGRCode [SetColor Foreground ci c]
                        Nothing -> ""
                        ++ s
                        ++ setSGRCode [Reset]
                  )
                  hs
              )
          Replace i hs -> error ""
          Remove i -> RemoveBuf i
      )
      edits

data BufferEdit = InsertBuf Int String | RemoveBuf Int deriving (Eq, Show)

drawEdits :: [BufferEdit] -> IO ()
drawEdits edits = do
  mapM_ drawEdit edits
  hFlush stdout

drawEdit :: BufferEdit -> IO ()
drawEdit (InsertBuf i s) = do
  setCursorPosition i 0
  putStr s
drawEdit (RemoveBuf i) = do
  setCursorPosition i 0
  clearLine

run :: IO ()
run = do
  useAlternateScreenBuffer
  src <- readFile "app/Main.hs"
  editor <- addBuffer src mkEditor
  _ <- getChar
  useNormalScreenBuffer