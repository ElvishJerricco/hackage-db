{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable
 -}

module Distribution.Hackage.DB.Unparsed
  ( HackageDB, PackageData(..), VersionData(..)
  , readTarball, parseTarball, revisions
  )
  where

import Distribution.Hackage.DB.Errors
import Distribution.Hackage.DB.Utility

import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Control.Exception
import Data.ByteString.Lazy as BS ( ByteString, empty, readFile )
import Data.Map as Map
import Data.Time.Clock
import Data.Vector as V
import Distribution.Package
import Distribution.Version
import GHC.Generics ( Generic )
import System.FilePath

type HackageDB = Map PackageName PackageData

data PackageData = PackageData { preferredVersions :: ByteString
                               , versions          :: Map Version VersionData
                               }
  deriving (Show, Eq, Generic)

data VersionData = VersionData { cabalFile         :: ByteString
                               , metaFile          :: ByteString
                               , previousRevisions :: Vector ByteString
                               }
  deriving (Show, Eq, Generic)

type HackageDBBuilder = Map PackageName PackageDataBuilder

data PackageDataBuilder = PackageDataBuilder { preferredVersionsBuilder :: ByteString
                                             , versionsBuilder          :: Map Version VersionDataBuilder
                                             }

data VersionDataBuilder = VersionDataBuilder { cabalFileBuilder         :: Maybe ByteString
                                             , metaFileBuilder          :: Maybe ByteString
                                             , previousRevisionsBuilder :: Vector ByteString
                                             }

vbuilder :: VersionDataBuilder
vbuilder = VersionDataBuilder Nothing Nothing V.empty

readTarball :: Maybe UTCTime -> FilePath -> IO HackageDB
readTarball snapshot path = fmap (parseTarball snapshot path) (BS.readFile path)

parseTarball :: Maybe UTCTime -> FilePath -> ByteString -> HackageDB
parseTarball snapshot path buf =
  mapException (\e -> HackageDBTarball path (e :: SomeException)) $
    foldEntriesUntil (maybe maxBound toEpochTime snapshot) Map.empty (Tar.read buf)

foldEntriesUntil :: EpochTime -> HackageDBBuilder -> Entries FormatError -> HackageDB
foldEntriesUntil _        db  Done       = buildHackageDB db
foldEntriesUntil _        _  (Fail err)  = throw (IncorrectTarfile err)
foldEntriesUntil snapshot db (Next e es) | entryTime e <= snapshot = foldEntriesUntil snapshot (handleEntry db e) es
                                         | otherwise               = buildHackageDB db

buildHackageDB :: HackageDBBuilder -> HackageDB
buildHackageDB = either throw id . Map.traverseWithKey buildPackageData
 where
  buildPackageData pn pdb = do
    vs <- Map.traverseWithKey buildVersionData (versionsBuilder pdb)
    return PackageData
      { preferredVersions = preferredVersionsBuilder pdb
      , versions = vs
      }
  buildVersionData vn vdb = do
    cf <- tarError "VersionDataBuilder" vn $ cabalFileBuilder vdb
    mf <- tarError "VersionDataBuilder" vn $ metaFileBuilder vdb
    return VersionData
      { cabalFile = cf
      , metaFile = mf
      , previousRevisions = previousRevisionsBuilder vdb
      }
  tarError msg field = maybe (Left (InvalidRepresentationOfType msg (show field))) Right

handleEntry :: HackageDBBuilder -> Entry -> HackageDBBuilder
handleEntry db e =
  let (pn':ep) = splitDirectories (entryPath e)
      pn = parseText "PackageName" pn'
  in
  case (ep, entryContent e) of

    (["preferred-versions"], NormalFile buf _) ->
      insertWith setConstraint pn (PackageDataBuilder buf Map.empty) db

    ([v',file], NormalFile buf _) -> let v = parseText "Version" v' in if
          | file == pn' <.> "cabal" -> insertVersionData setCabalFile pn v (vbuilder { cabalFileBuilder = Just buf }) db
          | file == "package.json"  -> insertVersionData setMetaFile pn v (vbuilder { metaFileBuilder = Just buf }) db
          | otherwise               -> throw (UnsupportedTarEntry e)

    (_, Directory) -> db                -- some tarballs have these superfluous entries
    ([], NormalFile {}) -> db
    ([], OtherEntryType {}) -> db

    _ -> throw (UnsupportedTarEntry e)

setConstraint :: PackageDataBuilder -> PackageDataBuilder -> PackageDataBuilder
setConstraint new old = old { preferredVersionsBuilder = preferredVersionsBuilder new }

insertVersionData :: (VersionDataBuilder -> VersionDataBuilder -> VersionDataBuilder)
                   -> PackageName -> Version -> VersionDataBuilder
                   -> HackageDBBuilder -> HackageDBBuilder
insertVersionData setFile pn v vd = insertWith mergeVersionData pn pd
  where
    pd = PackageDataBuilder BS.empty (Map.singleton v vd)
    mergeVersionData _ old = old { versionsBuilder = insertWith setFile v vd (versionsBuilder old) }

setCabalFile :: VersionDataBuilder -> VersionDataBuilder -> VersionDataBuilder
setCabalFile new old = old { cabalFileBuilder = cabalFileBuilder new
                           , previousRevisionsBuilder = let v = previousRevisionsBuilder old
                                                        in maybe v (V.snoc v) (cabalFileBuilder old)
                           }

setMetaFile :: VersionDataBuilder -> VersionDataBuilder -> VersionDataBuilder
setMetaFile new old = old { metaFileBuilder = metaFileBuilder new }

-- | Convenience function for getting all revisions, including the
-- latest, in one vector.
revisions :: VersionData -> Vector ByteString
revisions v = previousRevisions v `V.snoc` cabalFile v
