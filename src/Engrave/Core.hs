module Engrave.Core where

import Data.Word (Word32)

data Point = Point {_row :: Word32, _col :: Word32} deriving (Show)
