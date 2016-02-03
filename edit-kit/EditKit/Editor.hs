{-# LANGUAGE RecordWildCards #-}
module EditKit.Editor
    ( Editor
    , new
    , newIO
    , insertBuffer
    , getBuffer
    ) where

import EditKit.Buffer (Buffer)

import Data.Text (Text)
import STMContainers.Map (Map)
import Control.Concurrent.STM (STM, TVar, newTVar)
import qualified STMContainers.Map as Map

data Editor = Editor
  { buffers :: {-# UNPACK #-} !(Map Text (TVar Buffer))
  }

new :: STM Editor
new = Editor <$> Map.new

newIO :: IO Editor
newIO = Editor <$> Map.newIO

insertBuffer :: Editor -> Text -> Buffer -> STM ()
insertBuffer Editor{..} name buffer = do
  b <- newTVar buffer
  Map.insert b name buffers

getBuffer :: Editor -> Text -> STM (Maybe (TVar Buffer))
getBuffer Editor{..} name = Map.lookup name buffers
