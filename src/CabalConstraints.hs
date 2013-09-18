import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Version (showVersion)

import Distribution.Package (PackageName(PackageName), Dependency(Dependency), pkgVersion)
import Distribution.Simple.PackageIndex (allPackagesByName)
import Distribution.InstalledPackageInfo (sourcePackageId)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, configFlags, installedPkgs)
import Distribution.Simple.Setup (configConstraints)
import Distribution.Version (Version, isSpecificVersion)

import Options.Applicative

data Args = Args { depth           :: Depth
                 , setupConfigPath :: String
                 }

data Depth = Deep | Shallow


options :: Parser Args
options =  Args
    <$> flag Deep Shallow
        (  long "shallow"
        <> help "Only show the dependencies specified in the .cabal file"
        )
    <*> argument Just
        (  value "dist/setup-config"
        <> metavar "PATH"
        <> action "file"
        )


main :: IO ()
main = execParser opts >>= printConstraints
  where
    opts = info (helper <*> options)
        ( fullDesc
        <> progDesc (unlines [ "Show exact dependency constraints for a cabal project, by reading"
                             , "  the setup-config file located at PATH, defaulting to \"dist/setup-config\"."
                             ])
        )


printConstraints :: Args -> IO ()
printConstraints args = do
    let deps = case depth args of
                   Deep    -> deepDeps
                   Shallow -> shallowDeps
    join $  putStrLn
         .  formattedConstraints
         .  deps
         .  readInfo
         <$> readFile (setupConfigPath args)


readInfo :: String -> LocalBuildInfo
readInfo conf = read . unlines . drop 1 . lines $ conf


shallowDeps :: LocalBuildInfo -> [(PackageName, [Version])]
shallowDeps = sortBy (compare `on` fst)
            . map format . configConstraints . configFlags
  where
    format dependency@(Dependency name versionRange) =
        let version = fromMaybe (error $ errorMsg dependency)
                                (isSpecificVersion versionRange)
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
    separator = "\n" ++ replicate (length prefix - 2) ' ' ++ ", " 


formatConstraint :: (PackageName, [Version]) -> String
formatConstraint (PackageName name, versions) =
    name ++ " == " ++ allVersionConstraints versions
  where
    allVersionConstraints = intercalate " || "
                          . map showVersion
