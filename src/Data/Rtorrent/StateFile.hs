{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Rtorrent.StateFile where

import Data.Typeable
import Data.BEncode
import Data.ByteString.UTF8 (ByteString, fromString, toString)
import Data.ByteString.Lazy (fromStrict)

data CustomSeeding = CustomSeeding
  { _seedingTime ∷ ByteString
  } deriving (Show, Read, Eq, Typeable)

instance BEncode CustomSeeding where
  fromBEncode = fromDict $ CustomSeeding <$>! "seedingtime"

  toBEncode CustomSeeding {..} =
    toDict $ "seedingtime" .=! _seedingTime .: endDict

data StateFile = StateFile
  { _chokeHeuristicsDownLeech ∷ ByteString
  , _chokeHeuristicsDownSeed ∷ ByteString
  , _chokeHeuristicsUpLeech ∷ ByteString
  , _chokeHeuristicsUpSeed ∷ ByteString
  , _chunksDone ∷ Int
  , _chunksWanted ∷ Int
  , _complete ∷ Int
  , _connectionLeech ∷ ByteString
  , _connectionSeed ∷ ByteString
  , _custom ∷ CustomSeeding
  , _custom1 ∷ ByteString
  , _custom2 ∷ ByteString
  , _custom3 ∷ ByteString
  , _custom4 ∷ ByteString
  , _custom5 ∷ ByteString
  , _directory ∷ ByteString
  , _hashing ∷ Int
  , _ignoreCommands ∷ Int
  , _key ∷ Int
  , _loadedFile ∷ ByteString
  , _priority ∷ Int
  , _state ∷ Int
  , _stateChanged ∷ Int
  , _stateCounter ∷ Int
  , _throttleName ∷ ByteString
  , _tiedToFile ∷ ByteString
  , _timestampFinished ∷ Int
  , _timestampStarted ∷ Int
  , _totalUploaded ∷ Int
  , _views ∷ [ByteString]
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
    <*>! "custom"
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
    .: "custom" .=! _custom
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
