{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified EditKit.Buffer as Buffer
import qualified EditKit.Editor as Editor

import Control.Concurrent.STM (atomically, readTVar)
import qualified Data.Vector as V

import Graphics.Vty

main :: IO ()
main = do
  editor <- Editor.newIO
  buffer <- Buffer.fromFile "1.txt"
  atomically $ Editor.insertBuffer editor "1.txt" buffer

  buffer <- atomically $ do
    Just tbuffer <- Editor.getBuffer editor "1.txt"
    readTVar tbuffer

  let Buffer.Cursor{..} = Buffer.cursor buffer

  vty <- mkVty =<< standardIOConfig
  update vty (picForImage $ bufferToImage buffer)

  setCursorPos (outputIface vty) cursorLine cursorColumn
  showCursor (outputIface vty)

  e <- nextEvent vty
  shutdown vty

  print e

bufferToImage :: Buffer.Buffer -> Image
bufferToImage Buffer.Buffer{..} = vertCat . fmap (text' defAttr) . V.toList $ lines
