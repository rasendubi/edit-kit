{-# LANGUAGE RecordWildCards #-}
module EditKit.Editor
    ( Editor
    , new
    , newIO
    , insertBuffer
    , getBuffer
    , setCurrent
    , getCurrent
    , feedInput
    ) where

import EditKit.Buffer (Buffer(Buffer), Cursor(Cursor))
import qualified EditKit.Buffer as Buffer
import EditKit.Input

import Control.Concurrent.STM (STM, TVar, newTVar, newTVarIO, writeTVar, readTVar, atomically)
import Control.Monad (when, void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import STMContainers.Map (Map)
import qualified STMContainers.Map as Map

data Editor = Editor
  { buffers       :: {-# UNPACK #-} !(Map Text (TVar Buffer))
  , currentBuffer :: {-# UNPACK #-} !(TVar (Maybe (TVar Buffer)))
  }

new :: STM Editor
new = Editor <$> Map.new <*> newTVar Nothing

newIO :: IO Editor
newIO = Editor <$> Map.newIO <*> newTVarIO Nothing

insertBuffer :: Editor -> Text -> Buffer -> STM (TVar Buffer)
insertBuffer Editor{..} name buffer = do
  b <- newTVar buffer
  Map.insert b name buffers
  return b

getBuffer :: Editor -> Text -> STM (Maybe (TVar Buffer))
getBuffer Editor{..} name = Map.lookup name buffers

setCurrent :: Editor -> Maybe (TVar Buffer) -> STM ()
setCurrent Editor{..} current = writeTVar currentBuffer current

getCurrent :: Editor -> STM (Maybe (TVar Buffer))
getCurrent Editor{..} = readTVar currentBuffer

withCurrentBuffer :: Editor -> (TVar Buffer -> STM a) -> STM (Maybe a)
withCurrentBuffer editor f = do
  mtbuffer <- getCurrent editor
  case mtbuffer of
    Nothing -> return Nothing
    Just tbuffer -> Just <$> f tbuffer

feedInput :: Editor -> Event -> IO ()
feedInput editor (EventKey (KeyChar 'h') []) = atomically $ moveLeft editor
feedInput editor (EventKey (KeyChar 'j') []) = atomically $ moveDown editor
feedInput editor (EventKey (KeyChar 'k') []) = atomically $ moveUp editor
feedInput editor (EventKey (KeyChar 'l') []) = atomically $ moveRight editor
feedInput _ _ = return ()

moveUp :: Editor -> STM ()
moveUp = moveCursor $ \c -> c{ Buffer.cursorLine = Buffer.cursorLine c - 1 }

moveDown :: Editor -> STM ()
moveDown = moveCursor $ \c -> c{ Buffer.cursorLine = Buffer.cursorLine c + 1 }

moveLeft :: Editor -> STM ()
moveLeft = moveCursor $ \c -> c{ Buffer.cursorColumn = Buffer.cursorColumn c - 1 }

moveRight :: Editor -> STM ()
moveRight = moveCursor $ \c -> c{ Buffer.cursorColumn = Buffer.cursorColumn c + 1 }

moveCursor :: (Cursor -> Cursor) -> Editor -> STM ()
moveCursor f editor = do
  void $ withCurrentBuffer editor $ \tbuffer -> do
    buffer <- readTVar tbuffer
    let c = normalizeCursor buffer (f $ Buffer.cursor buffer)
    when (Buffer.isValidCursor buffer c) $
      writeTVar tbuffer buffer{ Buffer.cursor = c }

normalizeCursor :: Buffer -> Cursor -> Cursor
normalizeCursor Buffer{..} Cursor{..} = Cursor nLine nColumn
  where
    nLines = V.length lines
    nLine | cursorLine < 0        = 0
          | cursorLine >= nLines  = nLines - 1
          | otherwise             = cursorLine
    lineLen = if nLine < nLines then T.length (lines V.! nLine) else 0
    nColumn | cursorColumn < 0         = 0
            | cursorColumn >= lineLen  = lineLen - 1
            | otherwise                = cursorColumn
