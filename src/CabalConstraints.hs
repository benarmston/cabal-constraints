import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Version (showVersion)
import System.Environment (getArgs, getProgName)

import Distribution.Package (PackageName(PackageName), Dependency(Dependency), pkgVersion)
import Distribution.Simple.PackageIndex (allPackagesByName)
import Distribution.InstalledPackageInfo (sourcePackageId)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, configFlags, installedPkgs)
import Distribution.Simple.Setup (configConstraints)
import Distribution.Version (Version, isSpecificVersion)


main :: IO ()
main = do
    args <- getArgs
    let mdeps = case args of
                   ["--shallow"] -> Just shallowDeps
                   ["--deep"]    -> Just deepDeps
                   []            -> Just deepDeps
                   _             -> Nothing
    case mdeps of
        Nothing   -> usage
        Just deps -> join $  putStrLn
                          .  formattedConstraints
                          .  deps
                          .  info
                         <$> readFile "dist/setup-config"


usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " [--shallow|--deep]"
    putStrLn   "Show precise dependency constraints for the current cabal project"
    putStrLn   ""
    putStrLn   "  --shallow\t\tInclude only the dependencies specified in the cabal file"
    putStrLn   "  --deep\t\tInclude all dependencies for the current project"


info :: String -> LocalBuildInfo
info conf = read . unlines . drop 1 . lines $ conf


shallowDeps :: LocalBuildInfo -> [(PackageName, [Version])]
shallowDeps = sortBy (compare `on` fst)
            . map format . configConstraints . configFlags
  where
    format dependency@(Dependency name versionRange) =
        let version = case isSpecificVersion versionRange of
                          Just version' -> version'
                          Nothing      -> error $ errorMsg dependency
        in (name, [version])
    errorMsg dependency =
        "malformed setup-config: " ++
        "dependency is not constrained to a specific version: "
        ++ show dependency


deepDeps :: LocalBuildInfo -> [(PackageName, [Version])]
deepDeps = map format
         . allPackagesByName
         . installedPkgs
  where
    format (name, pkgInfos) = (name, map (pkgVersion. sourcePackageId) pkgInfos)


formattedConstraints :: [(PackageName, [Version])] -> String
formattedConstraints = (prefix ++)
                     . intercalate separator
                     . map formatConstraint
  where
    prefix = "constraints: "
    separator = "\n" ++ (replicate (length prefix - 2) ' ') ++ ", " 


formatConstraint :: (PackageName, [Version]) -> String
formatConstraint ((PackageName name), versions) =
    name ++ " == " ++ (allVersionConstraints versions)
  where
    allVersionConstraints = intercalate " || "
                          . map showVersion
