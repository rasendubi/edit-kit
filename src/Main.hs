{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified EditKit.Buffer as Buffer
import qualified EditKit.Editor as Editor
import qualified EditKit.Input as Input

import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Monad (when)
import qualified Data.Vector as V

import Graphics.Vty

main :: IO ()
main = do
  editor <- Editor.newIO
  buffer <- Buffer.fromFile "1.txt"
  atomically $ do
    tbuffer <- Editor.insertBuffer editor "1.txt" buffer
    Editor.setCurrent editor (Just tbuffer)

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
          e -> Editor.feedInput editor (toEditorInput e) >> loop

  e <- loop
  shutdown vty
  print e

toEditorInput (EvKey key modifiers) = Input.EventKey (toEditorKey key) (fmap toEditorModifier modifiers)

toEditorKey (KChar k) = Input.KeyChar k
toEditorKey _ = undefined

toEditorModifier MShift = Input.Shift
toEditorModifier MCtrl = Input.Ctrl
toEditorModifier MMeta = Input.Meta
toEditorModifier MAlt = Input.Alt

bufferToImage :: Buffer.Buffer -> Image
bufferToImage Buffer.Buffer{..} = vertCat . fmap (text' defAttr) . V.toList $ lines
