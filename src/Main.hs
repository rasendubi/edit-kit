{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified EditKit.Buffer as Buffer
import qualified EditKit.Editor as Editor

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Monad (when)
import qualified Data.Vector as V

import Graphics.Vty

main :: IO ()
main = do
  editor <- Editor.newIO
  buffer <- Buffer.fromFile "1.txt"
  atomically $ Editor.insertBuffer editor "1.txt" buffer
  vty <- mkVty =<< standardIOConfig
  let out = outputIface vty

  let loop = do
        buffer <- atomically $ do
            Just tbuffer <- Editor.getBuffer editor "1.txt"
            readTVar tbuffer

        let Buffer.Cursor{..} = Buffer.cursor buffer

        update vty (picForImage $ bufferToImage buffer)

        setCursorPos out cursorColumn cursorLine
        showCursor out

        e <- nextEvent vty
        case e of
          EvKey (KChar 'q') [] -> return e
          EvKey (KChar 'h') [] -> do
            atomically $ do
              Just tbuffer <- Editor.getBuffer editor "1.txt"
              moveLeft tbuffer
            loop
          EvKey (KChar 'j') [] -> do
            atomically $ do
              Just tbuffer <- Editor.getBuffer editor "1.txt"
              moveDown tbuffer
            loop
          EvKey (KChar 'k') [] -> do
            atomically $ do
              Just tbuffer <- Editor.getBuffer editor "1.txt"
              moveUp tbuffer
            loop
          EvKey (KChar 'l') [] -> do
            atomically $ do
              Just tbuffer <- Editor.getBuffer editor "1.txt"
              moveRight tbuffer
            loop
          _ -> loop

  e <- loop
  shutdown vty
  print e

moveUp :: TVar Buffer.Buffer -> STM ()
moveUp tbuffer = do
  buffer <- readTVar tbuffer
  let c  = Buffer.cursor buffer
  let c' = c{ Buffer.cursorLine = Buffer.cursorLine c - 1 }
  when (Buffer.isValidCursor buffer c') $
    writeTVar tbuffer buffer{ Buffer.cursor = c' }

moveDown :: TVar Buffer.Buffer -> STM ()
moveDown tbuffer = do
  buffer <- readTVar tbuffer
  let c  = Buffer.cursor buffer
  let c' = c{ Buffer.cursorLine = Buffer.cursorLine c + 1 }
  when (Buffer.isValidCursor buffer c') $
    writeTVar tbuffer buffer{ Buffer.cursor = c' }

moveLeft :: TVar Buffer.Buffer -> STM ()
moveLeft tbuffer = do
  buffer <- readTVar tbuffer
  let c  = Buffer.cursor buffer
  let c' = c{ Buffer.cursorColumn = Buffer.cursorColumn c - 1 }
  when (Buffer.isValidCursor buffer c') $
    writeTVar tbuffer buffer{ Buffer.cursor = c' }

moveRight :: TVar Buffer.Buffer -> STM ()
moveRight tbuffer = do
  buffer <- readTVar tbuffer
  let c  = Buffer.cursor buffer
  let c' = c{ Buffer.cursorColumn = Buffer.cursorColumn c + 1 }
  when (Buffer.isValidCursor buffer c') $
    writeTVar tbuffer buffer{ Buffer.cursor = c' }

bufferToImage :: Buffer.Buffer -> Image
bufferToImage Buffer.Buffer{..} = vertCat . fmap (text' defAttr) . V.toList $ lines
