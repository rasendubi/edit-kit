{-# LANGUAGE RecordWildCards #-}
module EditKit.Buffer
    ( Buffer(..)
    , Cursor(..)
    , fromFile
    , isValidCursor
    ) where

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Buffer = Buffer
  { lines  :: {-# UNPACK #-} !(Vector Text)
  , cursor :: {-# UNPACK #-} !Cursor
  } deriving (Show)

data Cursor = Cursor
  { cursorLine   :: {-# UNPACK #-} !Int
  , cursorColumn :: {-# UNPACK #-} !Int
  } deriving (Eq, Ord)

instance Show Cursor where
  show Cursor{..} = show cursorLine ++ ":" ++ show cursorColumn

fromFile :: FilePath -> IO Buffer
fromFile file = Buffer . V.fromList . T.lines <$> T.readFile file <*> pure (Cursor 0 0)

isValidCursor :: Buffer -> Cursor -> Bool
isValidCursor Buffer{..} Cursor{..} = cursorLine < V.length lines && cursorColumn <= T.length (lines V.! cursorLine)
