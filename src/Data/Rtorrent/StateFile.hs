{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      :  Data.Rtorrent.StateFile
-- Copyright   :  (c) Mateusz Kowalczyk 2014
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Exposes few convenient functions to work with 'StateFile's. For the
-- types and the instances actually used during parsing see
-- "Data.Rtorrent.StateFile.Types".
--
-- A few handy lenses for accessing and manipulating a 'StateFile' are
-- provided but they are not meant to be comprehensive: for most
-- scenarios, you can just use the generated lenses directly.
--
-- An example use-case of this module is stopping all torrents without
-- relying on rtorrent itself. This might be useful if your rtorrent
-- has troubles starting up. Here it is as easy as:
--
-- @'overFilesIn' "rtorrent/session/dir" 'stopTorrent'@
module Data.Rtorrent.StateFile where

import           Control.Applicative ((<$>))
import           Control.Exception (catch, IOException)
import           Control.Lens ((.~), set, view)
import           Control.Monad ((>=>))
import           Data.BEncode (decode, encode, Result)
import           Data.ByteString (readFile, writeFile, length)
import qualified Data.ByteString.Lazy as L (toStrict, ByteString)
import           Data.ByteString.UTF8 (fromString, toString, ByteString)
import           Data.List (isSuffixOf)
import           Data.Map (Map, mapEither, fromList)
import           Data.Rtorrent.StateFile.Types
import           Prelude hiding (readFile, writeFile, length)
import           System.Directory (getDirectoryContents)
import           System.FilePath ((</>))


-- | Takes a directory, 'StateFile' modification function and does an
-- ‘in-place’ modifications to all .rtorrent files it can find and
-- parse in that directory. Returns the list of all file paths and any
-- potential errors that with each.
--
-- See 'getRtorrentFiles' for the type of exception this function can throw.
overFilesIn ∷ FilePath → StateMod → IO [(FilePath, Maybe String)]
overFilesIn fp f =
  getRtorrentFiles fp >>= mapM (\x → (x,) <$> withRtorrentState x f)

-- | Gets a list of all @.rtorrent@ files in the specified directory.
--
-- This function uses 'getDirectoryContents' which can throw various
-- IOExceptions. It's up to the user to catch these if they wish to do
-- so.
getRtorrentFiles ∷ FilePath → IO [FilePath]
getRtorrentFiles t =
  map (t </>) . filter (".rtorrent" `isSuffixOf`) <$> getDirectoryContents t

-- | Attempts to read in the file at specified file path, change it
-- with the user-supplied function and saved the file with changes.
--
-- In case the parsing fails, the result will be @Just errMsg@.
withRtorrentState ∷ FilePath → StateMod → IO (Maybe String)
withRtorrentState fp f = parseFile fp >>= \case
  Left s → return $ Just s
  Right x → writeFile fp (encodeState' (f x)) >> return Nothing

-- | Only keeps properly parsing results. Useful with 'parseFiles'.
keepValid ∷ Map FilePath (Result StateFile) → Map FilePath StateFile
keepValid = snd . mapEither id

-- | Similar to 'keepValid' but instead only keeps results that failed
-- to parse and the reason.
keepInvalid ∷ Map FilePath (Result StateFile) → Map FilePath String
keepInvalid = fst . mapEither id

-- | Given a directory path, produces a 'Map' of file paths from
-- individual files to their parsing results.
--
-- This function uses 'getRtorrentFiles' which can throw an
-- IOException in case there is a problem reading the directory.
parseFiles ∷ FilePath → IO (Map FilePath (Result StateFile))
parseFiles t = getRtorrentFiles t >>= mapM parseFile' >>= return . fromList

-- | Like 'parseFile'' but throws away the 'FilePath': useful if we
-- only play with single files.
parseFile ∷ FilePath → IO (Result StateFile)
parseFile = parseFile' >=> return . snd

-- | Parses a file and returns a pair of of its path and the parsing
-- result. The path is useful if we're processing whole directories.
parseFile' ∷ FilePath → IO (FilePath, Result StateFile)
parseFile' f = readFileE >>= return . (f,) . \case
  Left (e ∷ IOException) → Left $ show e
  Right c → length c `seq` decodeState c
  where
    readFileE ∷ IO (Either IOException ByteString)
    readFileE = (Right <$> readFile f) `catch` (return . Left)


-- | Sets the torrent to started state.
startTorrent ∷ StateMod
startTorrent = state .~ 1

-- | Sets the torrent to stopped stated.
stopTorrent ∷ StateMod
stopTorrent = state .~ 0

-- | Changes the file the torrent is tied to.
setTiedFile ∷ FilePath → StateMod
setTiedFile = set tiedToFile . BS8 . fromString

-- | Gets a path of the loaded file.
getLoadedFile ∷ StateFile → FilePath
getLoadedFile = toString . _bs8 . view loadedFile

-- | Wrapper for 'decode' which works for 'StateFile's.
decodeState ∷ ByteString → Result StateFile
decodeState = decode

-- | Wrapper for 'encode' which works for 'StateFile's.
--
-- See 'encodeState'' for strict 'ByteString' version.
encodeState ∷ StateFile → L.ByteString
encodeState = encode

-- | Encodes a 'StateFile' to a strict 'ByteString'.
encodeState' ∷ StateFile → ByteString
encodeState' = L.toStrict . encode
