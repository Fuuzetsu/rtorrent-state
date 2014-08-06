{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Rtorrent.StateFileSpec where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Rtorrent.StateFile
import Data.BEncode
import Data.ByteString.UTF8 (ByteString, fromString, toString)
import Data.ByteString.Lazy (toStrict)
import Data.Typeable

-- | 'arbitrary' ByteString encoding UTF8 strings
arbBs ∷ Gen ByteString
arbBs = fromString <$> arbitrary

instance Arbitrary CustomSeeding where
  arbitrary = CustomSeeding . fromString <$> arbitrary

instance Arbitrary StateFile where
  arbitrary = do
    (cdl, cds, cdul, cdus) ← (,,,) <$> arbBs <*> arbBs <*> arbBs <*> arbBs
    (cd, cw, com) ← (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    (cl, cs) ← (,) <$> arbBs <*> arbBs
    cust ← arbitrary
    (c1, c2, c3, c4, c5, dir) ← (,,,,,) <$> arbBs <*> arbBs
                                <*> arbBs <*> arbBs <*> arbBs <*> arbBs
    (hash, ign, k) ← (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    lf ← arbBs
    (p, st, stch, stco) ← (,,,) <$> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary
    (tn, tf) ← (,) <$> arbBs <*> arbBs
    (tsf, tss, totu) ← (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    vs ← listOf arbBs
    return StateFile { _chokeHeuristicsDownLeech = cdl
                     , _chokeHeuristicsDownSeed = cds
                     , _chokeHeuristicsUpLeech = cdul
                     , _chokeHeuristicsUpSeed = cdus
                     , _chunksDone = cd
                     , _chunksWanted = cw
                     , _complete = com
                     , _connectionLeech = cl
                     , _connectionSeed = cs
                     , _custom = cust
                     , _custom1 = c1
                     , _custom2 = c2
                     , _custom3 = c3
                     , _custom4 = c4
                     , _custom5 = c5
                     , _directory = dir
                     , _hashing = hash
                     , _ignoreCommands = ign
                     , _key = k
                     , _loadedFile = lf
                     , _priority = p
                     , _state = st
                     , _stateChanged = stch
                     , _stateCounter = stco
                     , _throttleName = tn
                     , _tiedToFile = tf
                     , _timestampFinished = tsf
                     , _timestampStarted = tss
                     , _totalUploaded = totu
                     , _views = vs
                     }

newtype RtorrentFileContent = RTFC ByteString
                            deriving (Show, Eq, Read, Typeable)

instance Arbitrary RtorrentFileContent where
  arbitrary = RTFC . toStrict . encode <$> (arbitrary  ∷ Gen StateFile)

spec ∷ Spec
spec = describe "StateFile" $ do
  modifyMaxSuccess (const 1000) . prop "decode . encode = id" $
    \s@(StateFile {}) → let e = toStrict $ encode s
                        in decode e `shouldBe` Right s
