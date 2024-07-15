module Lib
  ( Tree,
    parse,
    Point,
    TokenKind (..),
    Token,
    query,
    Highlight,
    highlight,
    highlight',
  )
where

import Data.Word (Word32)
import Foreign (ForeignPtr, free, withForeignPtr)
import Foreign.C.String (newCStringLen, peekCString)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Alloc (malloc)
import Foreign.Ptr (nullPtr)
import Foreign.Storable
  ( peek,
    poke,
  )
import TreeSitter.Haskell (tree_sitter_haskell)
import TreeSitter.Node
  ( Node (nodeEndPoint),
    TSNode (TSNode),
    TSPoint (pointColumn, pointRow),
    ts_node_poke_p,
    ts_node_string_p,
  )
import TreeSitter.Parser
  ( ts_parser_new,
    ts_parser_parse_string,
    ts_parser_set_language,
  )
import TreeSitter.Query
  ( captureTSNode,
    matchCaptures,
    ts_query_cursor_exec_p,
    ts_query_cursor_new,
    ts_query_cursor_next_match,
    ts_query_new,
  )
import TreeSitter.Tree (ts_tree_root_node_p)
import qualified TreeSitter.Tree

data Tree = Tree
  { _root :: ForeignPtr Node,
    _ptr :: ForeignPtr TreeSitter.Tree.Tree
  }

parse :: String -> IO Tree
parse source = do
  (str, len) <- newCStringLen source

  parser <- ts_parser_new
  _ <- ts_parser_set_language parser tree_sitter_haskell
  treePtr <- ts_parser_parse_string parser nullPtr str len
  free parser
  free str

  rootPtr <- malloc
  ts_tree_root_node_p treePtr rootPtr

  root <- newForeignPtr rootPtr (free rootPtr)
  ptr <- newForeignPtr treePtr (free treePtr)

  return
    Tree
      { _root = root,
        _ptr = ptr
      }

data Point = Point {_row :: Word32, _col :: Word32} deriving (Show)

data TokenKind = IntToken deriving (Show)

data Token = Token TokenKind Point Point deriving (Show)

query :: String -> Tree -> IO [Token]
query s tree = do
  (queryStr, queryStrLen) <- newCStringLen s
  errorOffset <- malloc
  errorType <- malloc
  cursorPtr <- ts_query_cursor_new
  queryPtr <- ts_query_new tree_sitter_haskell queryStr queryStrLen errorOffset errorType

  e <- peek errorType
  print e

  withForeignPtr (_root tree) $ \node -> do
    ts_query_cursor_exec_p cursorPtr queryPtr node
    return ()

  matchPointer <- malloc
  let f = do
        isOk <- ts_query_cursor_next_match cursorPtr matchPointer
        if isOk
          then do
            match <- peek matchPointer
            capture <- peek $ matchCaptures match

            ts_node <- malloc
            poke ts_node (captureTSNode capture)

            node_ptr <- malloc
            ts_node_poke_p ts_node node_ptr
            node <- peek node_ptr

            kind <- ts_node_string_p ts_node >>= peekCString
            let (TSNode _ point _ _ _) = captureTSNode capture
                tokenKind = case kind of
                  "(integer)" -> IntToken
                  _ -> error "TODO"
            next <- f
            return $
              Token
                tokenKind
                (Point (pointRow point) (pointColumn point))
                (Point (pointRow $ nodeEndPoint node) (pointColumn $ nodeEndPoint node))
                : next
          else pure []
  f

data Highlight = Highlight String (Maybe TokenKind) deriving (Show)

highlight :: [String] -> [Token] -> [Highlight]
highlight buf tokens = concatMap (\(idx, row) -> highlight' row idx tokens) (zip [0 ..] buf)

highlight' :: String -> Word32 -> [Token] -> [Highlight]
highlight' s row ((Token t start end) : ts) =
  if row >= _row start && row <= _row end
    then
      let (x, len) =
            if _row start == row
              then
                let l =
                      if _row end == row
                        then fromIntegral (_col end - _col start)
                        else length s
                 in (fromIntegral (_col start), l)
              else
                if _row end == row
                  then (0, fromIntegral $ _col end)
                  else (0, length s)
          hs = highlight' (take x s) row ts ++ [Highlight (take len (drop x s)) (Just t)]
          trailing = drop (x + len) s
       in if null trailing then hs else hs ++ highlight' trailing row ts
    else highlight' s row ts
highlight' s _ [] = [Highlight s Nothing]
