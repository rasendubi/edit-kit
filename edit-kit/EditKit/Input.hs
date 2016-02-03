module EditKit.Input
  ( Key(..)
  , Modifier(..)
  , Event(..)
  ) where

data Key
  = KeyChar {-# UNPACK #-} !Char
  deriving (Eq, Show)

data Modifier
  = Shift
  | Ctrl
  | Meta
  | Alt
  deriving (Eq, Show)

data Event
  = EventKey !Key ![Modifier]
  deriving (Eq, Show)
