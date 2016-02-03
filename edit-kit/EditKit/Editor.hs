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

import EditKit.Buffer (Buffer)
import qualified EditKit.Buffer as Buffer
import EditKit.Input

import Data.Text (Text)
import STMContainers.Map (Map)
import Control.Monad (when, void)
import Control.Concurrent.STM (STM, TVar, newTVar, newTVarIO, writeTVar, readTVar, atomically)
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
moveUp editor = do
  void $ withCurrentBuffer editor $ \tbuffer -> do
    buffer <- readTVar tbuffer
    let c  = Buffer.cursor buffer
    let c' = c{ Buffer.cursorLine = Buffer.cursorLine c - 1 }
    when (Buffer.isValidCursor buffer c') $
      writeTVar tbuffer buffer{ Buffer.cursor = c' }

moveDown :: Editor -> STM ()
moveDown editor = do
  void $ withCurrentBuffer editor $ \tbuffer -> do
    buffer <- readTVar tbuffer
    let c  = Buffer.cursor buffer
    let c' = c{ Buffer.cursorLine = Buffer.cursorLine c + 1 }
    when (Buffer.isValidCursor buffer c') $
      writeTVar tbuffer buffer{ Buffer.cursor = c' }

moveLeft :: Editor -> STM ()
moveLeft editor = do
  void $ withCurrentBuffer editor $ \tbuffer -> do
    buffer <- readTVar tbuffer
    let c  = Buffer.cursor buffer
    let c' = c{ Buffer.cursorColumn = Buffer.cursorColumn c - 1 }
    when (Buffer.isValidCursor buffer c') $
      writeTVar tbuffer buffer{ Buffer.cursor = c' }

moveRight :: Editor -> STM ()
moveRight editor = do
  void $ withCurrentBuffer editor $ \tbuffer -> do
    buffer <- readTVar tbuffer
    let c  = Buffer.cursor buffer
    let c' = c{ Buffer.cursorColumn = Buffer.cursorColumn c + 1 }
    when (Buffer.isValidCursor buffer c') $
      writeTVar tbuffer buffer{ Buffer.cursor = c' }
