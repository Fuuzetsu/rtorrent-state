{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      :  Data.Rtorrent.StateFile
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the decoding and encoding functionality for rtorrent state
-- files. The files themselves are a bencoded set of data used by
-- rtorrent.
module Data.Rtorrent.StateFile.Types where

import Control.Lens (makeLenses)
import Data.BEncode
import Data.ByteString.UTF8 (ByteString)
import Data.Typeable (Typeable)

-- | Handy alias
type StateMod = StateFile → StateFile

newtype ByteStringUtf8 = BS8 { _bs8 ∷ ByteString }
                       deriving (Show, Eq, Read, Typeable)

instance BEncode ByteStringUtf8 where
  fromBEncode = fmap BS8 . fromBEncode
  toBEncode (BS8 s) = toBEncode s

data CustomSeeding = CustomSeeding
  { _addTime ∷ Maybe ByteStringUtf8
  , _seedingTime ∷ ByteStringUtf8
  } deriving (Show, Read, Eq, Typeable)

instance BEncode CustomSeeding where
  fromBEncode = fromDict $ CustomSeeding
    <$>? "addtime"
    <*>! "seedingtime"

  toBEncode CustomSeeding {..} = toDict $
       "addtime" .=? _addTime
    .: "seedingtime" .=! _seedingTime
    .: endDict

data StateFile = StateFile
  { _chokeHeuristicsDownLeech ∷ ByteStringUtf8
  , _chokeHeuristicsDownSeed ∷ ByteStringUtf8
  , _chokeHeuristicsUpLeech ∷ ByteStringUtf8
  , _chokeHeuristicsUpSeed ∷ ByteStringUtf8
  , _chunksDone ∷ Int
  , _chunksWanted ∷ Int
  , _complete ∷ Int
  , _connectionLeech ∷ ByteStringUtf8
  , _connectionSeed ∷ ByteStringUtf8
  , _custom ∷ Maybe CustomSeeding
  , _custom1 ∷ ByteStringUtf8
  , _custom2 ∷ ByteStringUtf8
  , _custom3 ∷ ByteStringUtf8
  , _custom4 ∷ ByteStringUtf8
  , _custom5 ∷ ByteStringUtf8
  , _directory ∷ ByteStringUtf8
  , _hashing ∷ Int
  , _ignoreCommands ∷ Int
  , _key ∷ Int
  , _loadedFile ∷ ByteStringUtf8
  , _priority ∷ Int
  , _state ∷ Int
  , _stateChanged ∷ Int
  , _stateCounter ∷ Int
  , _throttleName ∷ ByteStringUtf8
  , _tiedToFile ∷ ByteStringUtf8
  , _timestampFinished ∷ Int
  , _timestampStarted ∷ Int
  , _totalUploaded ∷ Int
  , _views ∷ [ByteStringUtf8]
  } deriving (Show, Read, Eq, Typeable)

instance BEncode StateFile where
  fromBEncode = fromDict $ StateFile
    <$>! "choke_heuristics.down.leech"
    <*>! "choke_heuristics.down.seed"
    <*>! "choke_heuristics.up.leech"
    <*>! "choke_heuristics.up.seed"
    <*>! "chunks_done"
    <*>! "chunks_wanted"
    <*>! "complete"
    <*>! "connection_leech"
    <*>! "connection_seed"
    <*>? "custom"
    <*>! "custom1"
    <*>! "custom2"
    <*>! "custom3"
    <*>! "custom4"
    <*>! "custom5"
    <*>! "directory"
    <*>! "hashing"
    <*>! "ignore_commands"
    <*>! "key"
    <*>! "loaded_file"
    <*>! "priority"
    <*>! "state"
    <*>! "state_changed"
    <*>! "state_counter"
    <*>! "throttle_name"
    <*>! "tied_to_file"
    <*>! "timestamp.finished"
    <*>! "timestamp.started"
    <*>! "total_uploaded"
    <*>! "views"

  toBEncode StateFile {..}  = toDict $
       "choke_heuristics.down.leech" .=! _chokeHeuristicsDownLeech
    .: "choke_heuristics.down.seed" .=! _chokeHeuristicsDownSeed
    .: "choke_heuristics.up.leech" .=! _chokeHeuristicsUpLeech
    .: "choke_heuristics.up.seed" .=! _chokeHeuristicsUpSeed
    .: "chunks_done" .=! _chunksDone
    .: "chunks_wanted" .=! _chunksWanted
    .: "complete" .=! _complete
    .: "connection_leech" .=! _connectionLeech
    .: "connection_seed" .=! _connectionSeed
    .: "custom" .=? _custom
    .: "custom1" .=! _custom1
    .: "custom2" .=! _custom2
    .: "custom3" .=! _custom3
    .: "custom4" .=! _custom4
    .: "custom5" .=! _custom5
    .: "directory" .=! _directory
    .: "hashing" .=! _hashing
    .: "ignore_commands" .=! _ignoreCommands
    .: "key" .=! _key
    .: "loaded_file" .=! _loadedFile
    .: "priority" .=! _priority
    .: "state" .=! _state
    .: "state_changed" .=! _stateChanged
    .: "state_counter" .=! _stateCounter
    .: "throttle_name" .=! _throttleName
    .: "tied_to_file" .=! _tiedToFile
    .: "timestamp.finished" .=! _timestampFinished
    .: "timestamp.started" .=! _timestampStarted
    .: "total_uploaded" .=! _totalUploaded
    .: "views" .=! _views
    .: endDict

makeLenses ''ByteStringUtf8
makeLenses ''CustomSeeding
makeLenses ''StateFile
