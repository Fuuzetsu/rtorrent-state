{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Rtorrent.StateFileSpec where

import           Control.Applicative ((<$>), (<*>), liftA2)
import           Data.BEncode
import           Data.ByteString (writeFile)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.UTF8 (ByteString, fromString)
import qualified Data.Map as M
import           Data.Rtorrent.StateFile
import           Data.Rtorrent.StateFile.Types
import           Data.Typeable
import           Prelude hiding (writeFile)
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck hiding (Result)

-- | Just like 'withRtorrentFiles' but allows us to specify the number
-- of files to use. When 0 or negative, no files are created.
withNRtorrentFiles ∷ Int → StateMod → (FilePath → IO a) → IO a
withNRtorrentFiles i f g = do
  t ← getTemporaryDirectory
  withTempDirectory t "rtorrent-state-test-dir" $ \d → do
    mapM_ (\x → makeStateFile $ d </> show x ++ ".rtorrent") [1 .. i]
    g d
  where
    genStateFileContent ∷ IO ByteString
    genStateFileContent = generate arbitrary >>= return . encodeState' . f

    makeStateFile ∷ FilePath → IO ()
    makeStateFile p = genStateFileContent >>= writeFile p

-- | Produces a temporary directory with some temporary, generated
-- .rtorrent files for the functions that need it.
withRtorrentFiles ∷ StateMod -- ^ Modifies the 'StateFile' before
                             -- writing in case we need specific
                             -- conditions
                  → (FilePath → IO a) → IO a
withRtorrentFiles f g =
  generate arbitrary >>= \(Positive i) → withNRtorrentFiles i f g

-- | IO version of 'shouldSatisfy'.
shouldSatisfyIO ∷ IO a → (a → Bool) → Expectation
t `shouldSatisfyIO` p = p <$> t `shouldReturn` True

spec ∷ Spec
spec = describe "StateFile" $ do
  modifyMaxSuccess (const 10) . prop "decode . encode = id" $
    \s@(StateFile {}) → let e = toStrict $ encode s
                        in decode e `shouldBe` Right s

  prop "overFiles and parse files work with 0 files"
    . withNRtorrentFiles 0 id $ \d →
    afterRunning d id `shouldSatisfyIO` const True

  prop "can stop all torrents" . withRtorrentFiles startTorrent $ \d →
    afterRunning d stopTorrent `shouldSatisfyIO` allStopped

  it "can stop 10000 torrents" . once
    . withNRtorrentFiles 10000 startTorrent $ \d →
    afterRunning d stopTorrent `shouldSatisfyIO` allStopped

  prop "can start all torrents" . withRtorrentFiles stopTorrent $ \d →
    afterRunning d startTorrent `shouldSatisfyIO` allStarted

  where
    allSatisfy ∷ M.Map FilePath (Result StateFile) -- ^ parsed files
               → (Result StateFile → Bool) -- ^ predicate
               → Bool
    allSatisfy m p = all (== True) . map p $ M.elems m

    -- similar to allSatisfy but fails if a parsing failure has occured
    allAre ∷ M.Map FilePath (Result StateFile)
           → (StateFile → Bool)
           → Bool
    allAre m p = allSatisfy m (\case Right x → p x
                                     Left _ → False )

    allStopped, allStarted ∷ M.Map FilePath (Result StateFile) → Bool
    allStopped m = allAre m ((== 0) . _state)
    allStarted m = allAre m ((== 1) . _state)

    afterRunning d f = overFilesIn d f >> parseFiles d

instance Arbitrary ByteStringUtf8 where
  arbitrary = BS8 . fromString <$> arbitrary

instance Arbitrary CustomSeeding where
  arbitrary = liftA2 CustomSeeding arbitrary arbitrary

instance Arbitrary StateFile where
  arbitrary = do
    (cdl, cds, cdul, cdus) ← (,,,) <$> arbitrary <*> arbitrary <*> arbitrary
                             <*> arbitrary
    (cd, cw, com) ← (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    (cl, cs) ← (,) <$> arbitrary <*> arbitrary
    cust ← arbitrary
    (c1, c2, c3, c4, c5, dir) ← (,,,,,) <$> arbitrary <*> arbitrary
                                <*> arbitrary <*> arbitrary <*> arbitrary
                                <*> arbitrary
    (hash, ign, k) ← (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    lf ← arbitrary
    (p, st, stch, stco) ← (,,,) <$> arbitrary <*> arbitrary
                          <*> arbitrary <*> arbitrary
    (tn, tf) ← (,) <$> arbitrary <*> arbitrary
    (tsf, tss, totu) ← (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    vs ← listOf arbitrary
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
  arbitrary = RTFC . toStrict . encode <$> (arbitrary ∷ Gen StateFile)
